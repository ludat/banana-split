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
) where

import Data.Text qualified as Text
import Data.Time (diffUTCTime, fromGregorian, getCurrentTime)
import Database.Beam.Postgres (Connection, runBeamPostgres)
import Protolude

import BananaSplit qualified as M
import BananaSplit.Persistence
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
  -- Fuera de la medición: la primera lectura podría estar reparando el cache.
  runBeamPostgres conn $ void $ netosYGastosDelGrupo grupoId

  desdeCache <- medir "cache (una query agregada)" vueltas $ do
    netos <- runBeamPostgres conn $ netosDeGrupo grupoId
    pure $ Text.length (show netos)

  reconstruyendo <- medir "reconstruyendo cada gasto" vueltas $ do
    netos <- runBeamPostgres conn $ do
      shallowGrupo <- fetchGrupo grupoId >>= maybe (panic "grupo no encontrado") pure
      shallowPagos <- fetchShallowPagos grupoId
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
