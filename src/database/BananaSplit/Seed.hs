{-# LANGUAGE OverloadedRecordDot #-}

-- | Herramientas de desarrollo para medir el costo de calcular los netos de un
-- grupo grande. No se usan desde la app.
--
-- 'seedGrupo' arma un grupo sintético con gastos de las cuatro formas que
-- existen (equitativo, montos fijos, mixto y repartija con claims), porque lo
-- que se quiere estresar es cuántas queries hace falta para reconstruir un
-- gasto, y eso depende de la forma de sus distribuciones.
--
-- 'benchNetos' compara los dos caminos que hoy conviven en el código: sumar
-- desde el cache con una query agregada, contra reconstruir cada gasto desde
-- sus distribuciones. Corren sobre los mismos datos y en el mismo proceso, así
-- que la diferencia no depende de comparar contra un deploy anterior.
module BananaSplit.Seed (
  seedGrupo,
  benchNetos,
  probarClaimsConcurrentes,

  -- * Piezas del escenario de concurrencia, para el test de integración
  Escenario (..),
  prepararEscenario,
  correrVueltaDeClaims,
  correrVueltaDeGuardarYClaim,
  limpiarTodo,
) where

import Conferer qualified
import Data.String (fromString)
import Data.Text qualified as Text
import Data.Time (diffUTCTime, fromGregorian, getCurrentTime)
import Database.Beam
import Database.Beam.Postgres (Connection, runBeamPostgres)
import Database.PostgreSQL.Simple qualified as Simple

import BananaSplit qualified as M
import BananaSplit.Persistence
import BananaSplit.Persistence.Schema
import BananaSplit.ULID (ULID, nullUlid)
import Preludat

-- | Crea un grupo con @participantes@ integrantes y @gastos@ gastos válidos, y
-- devuelve su id para pasárselo a 'benchNetos'.
seedGrupo :: Connection -> Int -> Int -> IO ULID
seedGrupo conn cantidadParticipantes cantidadGastos = do
  grupo <- runBeamPostgres conn $ createGrupo "Grupo de prueba" "participante 0"
  resto <- forM [1 .. cantidadParticipantes - 1] $ \i ->
    runBeamPostgres conn (addParticipante grupo.id ("participante " <> show i))
      >>= either (panic . ("no pude crear el participante: " <>)) pure

  let participantes =
        fmap (\p -> M.ParticipanteId p.id) (grupo.participantes <> resto)

  comienzo <- getCurrentTime
  forM_ [1 .. cantidadGastos] $ \i -> do
    pago <- runBeamPostgres conn $ savePago grupo.id (gastoNumero participantes i)
    -- Los claims de una repartija no viajan con el pago: hay que guardarlos
    -- aparte, y son los que la vuelven válida.
    forM_ (itemsDeRepartija pago) $ \(repartijaId, item, participante) ->
      void $ runBeamPostgres conn $ saveRepartijaClaim repartijaId (M.RepartijaClaim nullUlid participante item Nothing)
    when (i `mod` 100 == 0) $ putText $ "seed: " <> show i <> " gastos"
  fin <- getCurrentTime

  putText
    $ "seed: listo, "
    <> show cantidadGastos
    <> " gastos entre "
    <> show (length participantes)
    <> " participantes en "
    <> show (diffUTCTime fin comienzo)
  putText $ "seed: grupo " <> show grupo.id
  pure grupo.id

-- | Las cuatro formas de gasto, rotando. La moneda también rota, para que los
-- netos queden repartidos en varias y el consolidado tenga trabajo real.
gastoNumero :: [M.ParticipanteId] -> Int -> M.Pago
gastoNumero participantes i =
  M.Pago
    { M.pagoId = nullUlid
    , M.monto = monto
    , M.moneda = enPosicion [M.ARS, M.USD, M.EUR] i
    , M.nombre = "Gasto " <> show i
    , M.fecha = fromGregorian 2026 1 1
    , M.pagadores = partes [M.MontoFijo monto pagador]
    , M.deudores = deudores
    }
  where
    cantidad = length participantes
    pagador = enPosicion participantes i
    -- Un monto divisible por la cantidad de participantes para que los repartos
    -- con montos fijos cierren exacto.
    porCabeza = 100
    monto = fromIntegral (cantidad * porCabeza)

    partes ps = M.Distribucion nullUlid $ M.TipoDistribucionPartes $ M.DistribucionPartes nullUlid ps

    deudores = case i `mod` 4 of
      0 -> partes $ fmap (M.Ponderado 1) participantes
      1 -> partes $ fmap (M.MontoFijo (fromIntegral porCabeza)) participantes
      2 ->
        -- Uno pone un monto fijo y además pondera; el resto sólo pondera.
        partes
          $ M.PonderadoYMontoFijo (fromIntegral porCabeza) 1 (enPosicion participantes 0)
          : fmap (M.Ponderado 1) (drop 1 participantes)
      _ ->
        M.Distribucion nullUlid
          $ M.TipoDistribucionRepartija
          $ M.Repartija
            { M.id = nullUlid
            , M.nombre = "Repartija " <> show i
            , M.extra = 0
            , M.distribucionDeSobras = M.SobrasProporcional
            , M.items =
                participantes
                  & zip [0 :: Int ..]
                  & fmap (\(n, _) -> M.RepartijaItem nullUlid ("Item " <> show n) (fromIntegral porCabeza) 1)
            , M.claims = []
            }

-- | Indexa dando la vuelta, para ir rotando entre las opciones.
enPosicion :: [a] -> Int -> a
enPosicion xs n =
  case drop (n `mod` length xs) xs of
    (x : _) -> x
    [] -> panic "enPosicion sobre una lista vacía"

-- | Si el gasto guardado tiene una repartija en deudores, con qué participante
-- reclamar cada item. Reclamarlos todos es lo que la deja válida.
itemsDeRepartija :: M.Pago -> [(ULID, ULID, M.ParticipanteId)]
itemsDeRepartija pago =
  case pago.deudores.tipo of
    M.TipoDistribucionRepartija repartija ->
      case pago.pagadores.tipo of
        M.TipoDistribucionPartes partes ->
          let participante = case partes.partes of
                (M.MontoFijo _ p : _) -> p
                _ -> panic "esperaba un pagador con monto fijo"
          in fmap (\item -> (repartija.id, item.id, participante)) repartija.items
        _ -> []
    _ -> []

-- | Corre los dos caminos varias veces y reporta cuánto tardó cada uno.
benchNetos :: Connection -> ULID -> Int -> IO ()
benchNetos conn grupoId vueltas = do
  -- Una vuelta fuera de la medición, para no medir el calentamiento del plan.
  runBeamPostgres conn $ void $ netosDeGrupo grupoId

  desdeCache <- medir "cache (una query agregada)" vueltas $ do
    netos <- runBeamPostgres conn $ netosDeGrupo grupoId
    pure $ Text.length (show netos)

  reconstruyendo <- medir "reconstruyendo cada gasto" vueltas $ do
    netos <- runBeamPostgres conn $ do
      shallowGrupo <- fetchGrupo grupoId >>= maybe (panic "grupo no encontrado") pure
      shallowPagos <- fetchShallowPagos grupoId Nothing
      pagos <- traverse (fetchPago . (.pagoId)) shallowPagos
      pure
        $ M.calcularNetosTotales
        $ M.Grupo
          { M.id = shallowGrupo.id
          , M.nombre = shallowGrupo.nombre
          , M.participantes = shallowGrupo.participantes
          , M.monedaPorDefecto = shallowGrupo.monedaPorDefecto
          , M.pagos = pagos
          }
    pure $ Text.length (show netos)

  putText
    $ "bench: el cache es "
    <> show (reconstruyendo / desdeCache)
    <> "x más rápido"

medir :: Text -> Int -> IO Int -> IO Double
medir etiqueta vueltas accion = do
  comienzo <- getCurrentTime
  -- El resultado se fuerza (se mide su largo) para que la pereza no se coma el
  -- trabajo que queremos medir.
  total <- foldM (\acc _ -> (acc +) <$> accion) 0 [1 .. vueltas]
  fin <- getCurrentTime
  let segundos = realToFrac (diffUTCTime fin comienzo) :: Double
  putText
    $ "bench: "
    <> etiqueta
    <> ": "
    <> show (segundos / fromIntegral vueltas * 1000)
    <> " ms por vuelta ("
    <> show vueltas
    <> " vueltas, checksum "
    <> show total
    <> ")"
  pure segundos

-- | Vacía la base para que el test de integración arranque siempre del mismo
-- estado. No puede apoyarse en transacciones que se rollbackeen como el resto
-- del suite: la carrera necesita dos conexiones que commiteen de verdad, así
-- que lo que escriben queda.
--
-- Las tablas salen del catálogo y no de una lista a mano, así que una tabla
-- nueva se limpia sola. Y @TRUNCATE ... CASCADE@ resuelve el orden entre claves
-- foráneas, que es lo que obligaría a ordenar la lista de hijas a padres.
--
-- Mira sólo @public@ porque ahí viven las tablas reales: el @search_path@
-- apunta al esquema versionado de pgroll, donde son vistas —y por eso
-- @pg_tables@ no las trae—, y el bookkeeping de pgroll está en su propio
-- esquema.
limpiarTodo :: Connection -> IO ()
limpiarTodo conn = do
  tablas <- Simple.query_ conn "SELECT tablename FROM pg_tables WHERE schemaname = 'public'"
  case fmap Simple.fromOnly tablas of
    [] -> pure ()
    nombres ->
      void
        $ Simple.execute_ conn
        $ fromString
        $ toS
        $ "TRUNCATE "
        <> Text.intercalate ", " (fmap entrecomillar nombres)
        <> " RESTART IDENTITY CASCADE"
  where
    entrecomillar nombre = "public.\"" <> Text.replace "\"" "\"\"" nombre <> "\""

-- | El escenario que reproduce la carrera: un gasto cuya repartija tiene dos
-- items, y dos participantes que reclaman uno cada uno.
data Escenario = Escenario
  { grupoId :: ULID
  , pagoId :: ULID
  , repartijaId :: ULID
  , itemA :: ULID
  , itemB :: ULID
  , participanteA :: M.ParticipanteId
  , participanteB :: M.ParticipanteId
  }

-- | Una vuelta de "dos personas reclaman a la vez en la misma repartija", cada
-- una en su propia conexión y transacción.
--
-- Devuelve el par (lo que dice el cache, lo que da recalcular desde las
-- distribuciones). Que sean iguales es un oráculo de consistencia que no sabe
-- nada de esta carrera en particular, así que también atraparía otras.
--
-- Con el lock de 'recalcularResumenGasto' siempre coinciden. Para ver la
-- versión rota, sacá el 'lockearPago' de esa función: deja de coincidir en casi
-- todas las vueltas.
correrVueltaDeClaims ::
  Connection
  -> Connection
  -> Escenario
  -> IO (M.PorMoneda (M.Netos M.Monto), M.PorMoneda (M.Netos M.Monto))
correrVueltaDeClaims connA connB escenario = do
  limpiarClaims connA escenario

  -- Las dos transacciones arrancan lo más juntas posible y cada una reclama su
  -- item. Sin lock, cada una lee los claims sin ver el de la otra.
  listoA <- newEmptyMVar
  listoB <- newEmptyMVar
  _ <- forkIO $ reclamar connA escenario.repartijaId escenario.itemA escenario.participanteA >> putMVar listoA ()
  _ <- forkIO $ reclamar connB escenario.repartijaId escenario.itemB escenario.participanteB >> putMVar listoB ()
  takeMVar listoA
  takeMVar listoB

  desdeCache <- runBeamPostgres connA $ netosDeGrupo escenario.grupoId
  recalculado <- runBeamPostgres connA $ do
    pago <- fetchPago escenario.pagoId
    pure $ M.calcularNetosPago pago `M.enMoneda` pago.moneda
  pure (desdeCache, recalculado)

-- | La otra carrera: alguien edita el gasto mientras otro reclama en su
-- repartija.
--
-- 'savePago' arma el resumen sin releer el gasto, así que tiene que ir a buscar
-- los claims por su cuenta: son el único insumo que no escribe él mismo y que
-- no viaja en lo que manda el front. Esto verifica justamente eso.
--
-- El item A viene reclamado de antes para que el final sea un gasto válido: si
-- quedaran items sin reclamar el gasto sería inválido igual y la carrera no se
-- notaría.
--
-- Para ver la versión rota, hacé que 'savePago' use las distribuciones tal como
-- se las devolvió 'saveDistribucion', sin pasarlas por 'conClaimsGuardados':
-- el cache queda diciendo que el gasto no cierra.
correrVueltaDeGuardarYClaim ::
  Connection
  -> Connection
  -> Escenario
  -> IO (M.PorMoneda (M.Netos M.Monto), M.PorMoneda (M.Netos M.Monto))
correrVueltaDeGuardarYClaim connA connB escenario = do
  limpiarClaims connA escenario
  reclamar connA escenario.repartijaId escenario.itemA escenario.participanteA

  -- El gasto como lo tiene el front antes de mandar la edición. Los claims no
  -- viajan con él, así que da igual cuáles trae: 'savePago' los relee.
  pago <- runBeamPostgres connA $ fetchPago escenario.pagoId

  listoA <- newEmptyMVar
  listoB <- newEmptyMVar
  _ <- forkIO $ reguardar connA escenario.grupoId pago >> putMVar listoA ()
  _ <- forkIO $ reclamar connB escenario.repartijaId escenario.itemB escenario.participanteB >> putMVar listoB ()
  takeMVar listoA
  takeMVar listoB

  desdeCache <- runBeamPostgres connA $ netosDeGrupo escenario.grupoId
  recalculado <- runBeamPostgres connA $ do
    guardado <- fetchPago escenario.pagoId
    pure $ M.calcularNetosPago guardado `M.enMoneda` guardado.moneda
  pure (desdeCache, recalculado)

-- | Una transacción como la del handler de editar: vuelve a guardar el gasto
-- con un cambio que no toca el reparto.
reguardar :: Connection -> ULID -> M.Pago -> IO ()
reguardar conn unGrupoId pago =
  conTransaccion Serializable conn
    $ runBeamPostgres conn
    $ void
    $ savePago unGrupoId pago{M.nombre = "Cena editada"}

-- | La versión de línea de comandos, para correr muchas más vueltas de las que
-- conviene meter en el suite.
probarClaimsConcurrentes :: Conferer.Config -> Int -> IO ()
probarClaimsConcurrentes config vueltas = do
  connA <- openConnection config
  connB <- openConnection config
  escenario <- prepararEscenario connA

  resultados <- forM [1 .. vueltas] $ \vuelta -> do
    (desdeCache, recalculado) <- correrVueltaDeClaims connA connB escenario
    if desdeCache == recalculado
      then pure Nothing
      else do
        putText $ "vuelta " <> show (vuelta :: Int) <> ": el cache no coincide"
        putText $ "  cache:       " <> show desdeCache
        putText $ "  recalculado: " <> show recalculado
        pure $ Just vuelta

  case catMaybes resultados of
    [] -> putText $ "concurrencia: " <> show vueltas <> " vueltas, el cache quedó siempre consistente"
    fallidas -> putText $ "concurrencia: falló en " <> show (length fallidas) <> " de " <> show vueltas <> " vueltas"

-- | Una transacción como la del handler: reclamar un item.
reclamar :: Connection -> ULID -> ULID -> M.ParticipanteId -> IO ()
reclamar conn unaRepartijaId unItemId participante =
  conTransaccion Serializable conn
    $ runBeamPostgres conn
    $ void
    $ saveRepartijaClaim unaRepartijaId (M.RepartijaClaim nullUlid participante unItemId Nothing)

-- | Vuelve al estado sin claims, con el cache al día.
limpiarClaims :: Connection -> Escenario -> IO ()
limpiarClaims conn escenario =
  conTransaccion Serializable conn $ runBeamPostgres conn $ do
    runDelete
      $ delete
        db.repartija_claims
        ( \claim ->
            claim.repartijaclaimRepartijaItem
              `in_` [val_ (RepartijaItemId escenario.itemA), val_ (RepartijaItemId escenario.itemB)]
        )
    recalcularResumenGasto escenario.pagoId

prepararEscenario :: Connection -> IO Escenario
prepararEscenario conn = conTransaccion Serializable conn $ runBeamPostgres conn $ do
  grupo <- createGrupo "Concurrencia" "uno"
  otro <-
    addParticipante grupo.id "otro" >>= \case
      Right participante -> pure $ M.ParticipanteId participante.id
      Left e -> panic e
  let uno = case grupo.participantes of
        (p : _) -> M.ParticipanteId p.id
        [] -> panic "el grupo deberia tener un participante"

  pago <-
    savePago grupo.id
      $ M.Pago
        { M.pagoId = nullUlid
        , M.monto = 100
        , M.moneda = M.ARS
        , M.nombre = "Cena"
        , M.fecha = fromGregorian 2026 1 1
        , M.pagadores =
            M.Distribucion nullUlid $ M.TipoDistribucionPartes $ M.DistribucionPartes nullUlid [M.MontoFijo 100 uno]
        , M.deudores =
            M.Distribucion nullUlid
              $ M.TipoDistribucionRepartija
              $ M.Repartija
                { M.id = nullUlid
                , M.nombre = "Cena"
                , M.extra = 0
                , M.distribucionDeSobras = M.SobrasNoDistribuir
                , M.items =
                    [ M.RepartijaItem nullUlid "Item A" 50 1
                    , M.RepartijaItem nullUlid "Item B" 50 1
                    ]
                , M.claims = []
                }
        }

  case pago.deudores.tipo of
    M.TipoDistribucionRepartija repartija ->
      case repartija.items of
        (a : b : _) ->
          pure
            Escenario
              { grupoId = grupo.id
              , pagoId = pago.pagoId
              , repartijaId = repartija.id
              , itemA = a.id
              , itemB = b.id
              , participanteA = uno
              , participanteB = otro
              }
        _ -> panic "esperaba dos items"
    _ -> panic "esperaba una repartija"
