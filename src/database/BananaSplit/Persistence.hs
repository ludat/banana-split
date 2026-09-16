{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}

module BananaSplit.Persistence (
  MissingPGRollSchema (..),
  LoginEvent (..),
  countRecentAttempts,
  recordAttempt,
  clearAttempts,
  deleteOldLoginAttempts,
  conTransaccionDeEscritura,
  conTransaccionDeLecturaRapida,
  makePool,
  openConnection,
  runMigration,
  recomputePagos,
  addParticipante,
  claimParticipante,
  unclaimParticipante,
  createGrupo,
  createGrupoForUser,
  db,
  deletePago,
  deleteRepartijaClaim,
  deleteShallowParticipante,
  fetchGrupo,
  fetchGruposForUser,
  fetchMonedasConPagos,
  fetchUserById,
  fetchUserByEmail,
  createUser,
  updateUser,
  fetchGrupoIdFromClaim,
  fetchGrupoIdFromRepartija,
  fetchPago,
  fetchRepartija,
  ConteoDeGastos (..),
  contarGastos,
  fetchShallowPagos,
  fetchTasasDeCambio,
  fetchTransferencias,
  netosDeGrupo,
  freezeGrupo,
  guardarTasasDeCambio,
  borrarTransferencia,
  crearTransferenciaSaldada,
  desmarcarTransferenciaSaldada,
  marcarTransferenciaSaldada,
  normalizarTasa,
  TransferenciaGuardada (..),
  transferenciasHechas,
  transferenciasPendientes,
  recalcularResumenGasto,
  savePago,
  saveRepartija,
  saveRepartijaClaim,
  unfreezeGrupo,
  updateGrupo,
  updatePago,
) where

import Conferer qualified
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Pool qualified as Pool
import Data.String (String)
import Data.Text qualified as Text
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Database.Beam as Beam
import Database.Beam.Backend.SQL (BeamSqlBackendCanSerialize)
import Database.Beam.Postgres
import Database.Beam.Postgres.Full hiding (insert)
import Database.PostgreSQL.Simple (Only (..), execute, query)
import Database.PostgreSQL.Simple.Errors (isSerializationError)
import Database.PostgreSQL.Simple.Transaction qualified as Transaction

import BananaSplit qualified as M
import BananaSplit.Persistence.Migration_2026_05_26_FixDates qualified as FixDates
import BananaSplit.Persistence.ResumenGuardado (ResumenGuardado (..))
import BananaSplit.Persistence.ResumenGuardado qualified as ResumenGuardado
import BananaSplit.Persistence.Schema
import BananaSplit.PgRoll qualified as PgRoll
import BananaSplit.ULID (ULID, nullUlid)
import BananaSplit.ULID qualified as ULID
import Preludat

-- | Una conexión suelta, con el @search_path@ apuntando al esquema que pgroll
-- tiene activo. Para los comandos de línea que no levantan el server.
openConnection :: Conferer.Config -> IO Connection
openConnection config = do
  connString <- Conferer.fetchFromConfig "database.url" config
  schema <- PgRoll.getLatestSchema
  conn <- connectPostgreSQL connString
  _ <- execute conn "SET search_path TO ?" (Only schema)
  pure conn

-- | La transacción de cualquier cosa que escriba.
--
-- Va en SERIALIZABLE porque el cache del resumen de un gasto se calcula leyendo
-- cosas que otra transacción puede estar cambiando al mismo tiempo (los claims
-- de una repartija, sobre todo). Con READ COMMITTED cada statement ve un
-- snapshot nuevo, así que dos escrituras pueden leer los mismos insumos y
-- pisarse; en SERIALIZABLE el resultado tiene que ser equivalente a haberlas
-- corrido una después de la otra, y si no lo es Postgres aborta una.
--
-- De ahí el reintento: fallar con @40001@ es parte del protocolo, no un error.
-- La transacción que aborta no dejó nada escrito, así que volver a correr el
-- bloque entero es seguro.
conTransaccionDeEscritura :: Connection -> Pg a -> IO a
conTransaccionDeEscritura conn accion =
  Transaction.withTransactionModeRetry
    Transaction.TransactionMode
      { Transaction.isolationLevel = Transaction.RepeatableRead
      , Transaction.readWriteMode = Transaction.ReadWrite
      }
    isSerializationError
    conn
    (runBeamPostgres conn accion)

-- | La transacción de leer para mostrar, que es lo más barato que hay: no toma
-- predicate locks, no puede abortar por conflicto y no le agrega trabajo a las
-- escrituras que corren en paralelo.
--
-- El @READ ONLY@ no es sólo una pista: Postgres rechaza cualquier escritura que
-- entre por acá, así que una que se cuele rompe en el momento en vez de perder
-- en silencio las garantías de arriba.
--
-- Lo que se resigna es que cada statement ve un snapshot distinto, así que dos
-- queries de la misma transacción pueden ver estados distintos de la base.
conTransaccionDeLecturaRapida :: Connection -> Pg a -> IO a
conTransaccionDeLecturaRapida conn accion =
  Transaction.withTransactionMode
    Transaction.TransactionMode
      { Transaction.isolationLevel = Transaction.ReadCommitted
      , Transaction.readWriteMode = Transaction.ReadOnly
      }
    conn
    (runBeamPostgres conn accion)

runMigration :: Conferer.Config -> [String] -> IO ()
runMigration config args = do
  conn <- openConnection config
  case args of
    ["fix-pagos-fecha"] -> do
      runBeamPostgres conn FixDates.run
      putText "Done"
    ["recompute-pagos"] -> do
      recomputePagos conn
      putText "Done"
    ["prune-login-attempts"] -> do
      runBeamPostgres conn deleteOldLoginAttempts
      putText "Done"
    _ -> do
      putText $ "Unknown migration: " <> show args
      exitFailure
  close conn

makePool :: Conferer.Config -> IO (Pool.Pool Connection)
makePool config = do
  connString <- Conferer.fetchFromConfig "database.url" config
  schema <- PgRoll.getLatestSchema
  pool <-
    Pool.newPool
      $ Pool.defaultPoolConfig
        ( do
            conn <- connectPostgreSQL connString
            _ <- execute conn "SET search_path TO ?" (Only schema)
            pure conn
        )
        close
        60
        60
  Pool.withResource pool $ \conn -> do
    actualSchema <- query @_ @(Only Text) conn "SELECT schema_name FROM information_schema.schemata WHERE schema_name = ?;" (Only schema)
    when (length actualSchema /= 1)
      $ throwIO
      $ MissingPGRollSchema schema
  pure pool

newtype MissingPGRollSchema = MissingPGRollSchema
  { schemaName :: String
  }
  deriving stock (Show)
  deriving anyclass (Exception)

createGrupo :: Text -> Text -> Pg M.Grupo
createGrupo nombre participanteNombre =
  createGrupoWith
    nombre
    [M.Participante{M.id = nullUlid, M.nombre = participanteNombre, M.user = Nothing}]

createGrupoForUser :: Text -> M.User -> Pg M.Grupo
createGrupoForUser nombre user =
  createGrupoWith
    nombre
    [M.Participante{M.id = nullUlid, M.nombre = user.nombre, M.user = Just user}]

createGrupoWith :: Text -> [M.Participante] -> Pg M.Grupo
createGrupoWith nombre participantes = do
  newId <- liftIO ULID.getULID
  runInsert
    $ insert db.grupos
    $ insertValues
      [ Grupo newId nombre (M.ARS) Nothing
      ]
  savedParticipantes <- forM participantes $ \participante -> do
    participanteId <- liftIO ULID.getULID
    pure
      M.Participante
        { M.id = participanteId
        , M.nombre = participante.nombre
        , M.user = participante.user
        }
  unless (null savedParticipantes)
    $ runInsert
    $ insert db.participantes
    $ insertValues
    $ savedParticipantes
    & fmap (\p -> Participante p.id (GrupoId newId) p.nombre (UserId (fmap (.id) p.user)))
  pure
    $ M.Grupo
      { M.id = newId
      , M.nombre = nombre
      , M.pagos = []
      , M.participantes = savedParticipantes
      , M.monedaPorDefecto = M.ARS
      }

updateUser :: ULID -> Text -> Pg M.User
updateUser userId rawNombre = do
  let nombre = Text.strip rawNombre
  runUpdate
    $ update
      db.users
      (\u -> u.nombre <-. val_ nombre)
      (\u -> u.id ==. val_ userId)
  fetchUserById userId
    `orElseMay` fail ("user vanished while updating: " <> show userId)

fetchUserById :: ULID -> Pg (Maybe M.User)
fetchUserById userId = do
  existing <- runSelectReturningOne $ select $ do
    u <- all_ db.users
    guard_ (u.id ==. val_ userId)
    pure u
  pure $ fmap toModelUser existing

fetchUserByEmail :: M.Email -> Pg (Maybe M.User)
fetchUserByEmail email = do
  existing <- runSelectReturningOne $ select $ do
    u <- all_ db.users
    guard_ (u.email ==. val_ email)
    pure u
  pure $ fmap toModelUser existing

createUser :: M.Email -> Text -> Pg M.User
createUser email rawNombre = do
  let nombre = Text.strip rawNombre
  newId <- liftIO ULID.getULID
  now <- liftIO getCurrentTime
  runInsert
    $ insert db.users
    $ insertValues
      [ User{id = newId, email = email, nombre = nombre, created_at = now}
      ]
  pure
    $ M.User
      { M.id = newId
      , M.email = email
      , M.nombre = nombre
      }

toModelUser :: User -> M.User
toModelUser u =
  M.User
    { M.id = u.id
    , M.email = u.email
    , M.nombre = u.nombre
    }

attemptWindow :: NominalDiffTime
attemptWindow = 15 * 60

countRecentAttempts :: M.Email -> Pg Int
countRecentAttempts email = do
  now <- liftIO getCurrentTime
  let cutoff = now & addUTCTime (negate attemptWindow)
  mCount <-
    runSelectReturningOne
      $ select
      $ aggregate_ (\_ -> as_ @Int64 countAll_)
      $ do
        a <- all_ db.login_attempts
        guard_ (a.email ==. val_ email &&. a.created_at >=. val_ cutoff)
        pure a
  pure $ maybe 0 fromIntegral mCount

recordAttempt :: M.Email -> LoginEvent -> Pg ()
recordAttempt email event = do
  newId <- liftIO ULID.getULID
  now <- liftIO getCurrentTime
  runInsert
    $ insert db.login_attempts
    $ insertValues
      [LoginAttempt{id = newId, email = email, details = PgJSONB event, created_at = now}]
  let cutoff = now & addUTCTime (negate attemptWindow)
  runDelete
    $ delete
      db.login_attempts
      (\a -> a.email ==. val_ email &&. a.created_at <. val_ cutoff)

clearAttempts :: M.Email -> Pg ()
clearAttempts email = do
  runDelete
    $ delete
      db.login_attempts
      (\a -> a.email ==. val_ email)

deleteOldLoginAttempts :: Pg ()
deleteOldLoginAttempts = do
  now <- liftIO getCurrentTime
  let cutoff = now & addUTCTime (negate attemptWindow)
  runDelete
    $ delete
      db.login_attempts
      (\a -> a.created_at <. val_ cutoff)

fetchGrupo :: ULID -> Pg (Maybe M.ShallowGrupo)
fetchGrupo aGrupoId = do
  maybeGrupo <- runSelectReturningOne $ select $ do
    g <- all_ db.grupos
    guard_ (g.id ==. val_ aGrupoId)
    pure g

  case maybeGrupo of
    Nothing -> pure Nothing
    Just grupo -> do
      participantes <- fetchParticipantes aGrupoId
      tasasDeCambio <- fetchTasasDeCambio aGrupoId
      monedasConPagos <- fetchMonedasConPagos aGrupoId
      pure
        $ Just
        $ M.ShallowGrupo
          { M.id = grupo.id
          , M.nombre = grupo.nombre
          , M.participantes = participantes
          , M.congeladoAt = grupo.congelado_at
          , M.monedaPorDefecto = grupo.moneda_por_defecto
          , M.tasasDeCambio = tasasDeCambio
          , M.monedasConPagos = monedasConPagos
          }

-- | El join que filtra por usuario ya trae el participante que reclamó en cada
-- grupo, que es justo lo que muestra el listado: sale todo en una query.
fetchGruposForUser :: ULID -> Pg [M.GrupoParaUsuario]
fetchGruposForUser userId = do
  filas <- runSelectReturningList $ select $ do
    grupo <-
      all_ db.grupos
        & orderBy_ (asc_ . (.id))
    p <- all_ db.participantes
    guard_ (p.grupo ==. GrupoId grupo.id)
    guard_ (p.user ==. UserId (val_ (Just userId)))
    pure (grupo.id, grupo.nombre, p.nombre)
  pure
    $ filas
    & fmap
      ( \(grupoId, nombre, participanteNombre) ->
          M.GrupoParaUsuario
            { M.id = grupoId
            , M.nombre = nombre
            , M.participanteNombre = participanteNombre
            }
      )

fetchPago :: ULID -> ULID -> Pg M.Pago
fetchPago grupoId pagoId = do
  (dbPago :: Pago) <-
    fromMaybe (panic "Pago not found")
      <$> runSelectReturningOne
        ( select $ do
            pago <- all_ db.pagos
            guard_ (pago.pagoId ==. val_ pagoId)
            guard_ (pago.pagoGrupo ==. val_ (GrupoId grupoId))
            pure pago
        )

  (pagadores :: M.Distribucion) <- fromMaybe (panic "Pagadores not found") <$> fetchDistribucion dbPago.pagoMoneda (case dbPago.distribucion_pagadores of DistribucionId ulid -> ulid)
  (deudores :: M.Distribucion) <- fromMaybe (panic "deudores not found") <$> fetchDistribucion dbPago.pagoMoneda (case dbPago.distribucion_deudores of DistribucionId ulid -> ulid)

  pure
    M.Pago
      { M.pagoId = dbPago.pagoId
      , M.monto = desdeUnidadesMinimas dbPago.pagoMoneda dbPago.pagoMontoEnUnidadesMinimas
      , M.moneda = dbPago.pagoMoneda
      , M.nombre = dbPago.pagoNombre
      , M.fecha = dbPago.fecha
      , M.pagadores = pagadores
      , M.deudores = deudores
      }

-- | Cuántos gastos tiene un grupo y cuántos de ellos no están bien.
data ConteoDeGastos = ConteoDeGastos
  { total :: Int
  , invalidos :: Int
  }
  deriving stock (Show, Eq)

-- | Los dos conteos de un grupo.
--
-- Se cuenta en la base: para dos enteros no hace falta traer los gastos ni sus
-- filas de 'pagado_y_consumido_en_gasto'. Y van juntos porque salen de la misma pasada.
--
-- Qué cuenta como válido lo decide 'esValido_', al lado del decoder del blob.
contarGastos :: ULID -> Pg ConteoDeGastos
contarGastos grupoId = do
  resultado <- runSelectReturningOne $ select $ do
    aggregate_
      ( \pago ->
          ( as_ @Int32 countAll_
          , fromMaybe_ 0 $ sum_ $ ifThenElse_ (ResumenGuardado.esValido_ pago.resumen) (val_ 0) (val_ (1 :: Int32))
          )
      )
      $ do
        pago <- all_ db.pagos
        guard_ (pago.pagoGrupo ==. GrupoId (val_ grupoId))
        pure pago
  pure $ case resultado of
    Nothing -> ConteoDeGastos{total = 0, invalidos = 0}
    Just (cantidad, malos) ->
      ConteoDeGastos{total = fromIntegral cantidad, invalidos = fromIntegral malos}

escribirPagadoYConsumido :: ULID -> M.Pago -> M.ResumenGasto -> Pg ()
escribirPagadoYConsumido grupoId pago resumen = do
  let M.Netos pagado = resumen.pagado
      M.Netos consumido = resumen.consumido
      filas
        | not (M.resumenGastoEsValido resumen) = []
        | otherwise =
            Map.union pagado consumido
              & Map.keys
              & fmap
                ( \participante ->
                    PagadoYConsumidoEnGasto
                      { gasto = PagoId pago.pagoId
                      , participante = participanteId2Persistent participante
                      , grupo = GrupoId grupoId
                      , moneda = pago.moneda
                      , pagado_en_unidades_minimas =
                          aUnidadesMinimas pago.moneda $ Map.findWithDefault 0 participante pagado
                      , consumido_en_unidades_minimas =
                          aUnidadesMinimas pago.moneda $ Map.findWithDefault 0 participante consumido
                      }
                )

  runDelete
    $ delete
      db.pagado_y_consumido_en_gasto
      ( \pn ->
          pn.gasto
            ==. val_ (PagoId pago.pagoId)
            &&. not_ (pn.participante `in_` fmap (val_ . (.participante)) filas)
      )
  unless (null filas)
    $ runInsert
    $ insertOnConflict
      db.pagado_y_consumido_en_gasto
      (insertValues filas)
      (conflictingFields primaryKey)
      onConflictUpdateAll

aUnidadesMinimas :: M.Moneda -> M.Monto -> UnidadesMinimas
aUnidadesMinimas moneda monto
  | M.getLugaresDespuesDeLaComa monto > escala =
      panic
        $ "monto con más precisión que "
        <> show moneda
        <> ", que tiene "
        <> show escala
        <> " decimales: "
        <> show monto
  | otherwise = fromIntegral $ M.mantisaEn escala monto
  where
    escala = M.escalaDe moneda

aUnidadesMinimasMaybe :: M.Moneda -> Maybe M.Monto -> Maybe UnidadesMinimas
aUnidadesMinimasMaybe moneda = fmap (aUnidadesMinimas moneda)

desdeUnidadesMinimas :: M.Moneda -> UnidadesMinimas -> M.Monto
desdeUnidadesMinimas moneda unidades =
  M.mkMonto (M.escalaDe moneda) (fromIntegral unidades)

desdeUnidadesMinimasMaybe :: M.Moneda -> Maybe UnidadesMinimas -> Maybe M.Monto
desdeUnidadesMinimasMaybe moneda = fmap (desdeUnidadesMinimas moneda)

fetchDistribucionMontoEquitativoItems :: ULID -> Pg [M.ParticipanteId]
fetchDistribucionMontoEquitativoItems distribucionMontoEquitativoId = do
  items <- runSelectReturningList $ select $ do
    item <- all_ db.distribuciones_monto_equitativo_items
    guard_ (item.distribucion ==. DistribucionMontoEquitativoId (val_ distribucionMontoEquitativoId))
    pure item
  pure $ items & fmap (\item -> M.ParticipanteId $ case item.participante of ParticipanteId ulid -> ulid)

-- | Las distribuciones de montos específicos viejas se leen como partes con
-- montos fijos.
fetchDistribucionMontosEspecificosItems :: M.Moneda -> ULID -> Pg [M.Parte]
fetchDistribucionMontosEspecificosItems moneda distribucionMontosEspecificosId = do
  items <- runSelectReturningList $ select $ do
    item <- all_ db.distribuciones_montos_especificos_items
    guard_ (item.distribucion ==. DistribucionMontosEspecificosId (val_ distribucionMontosEspecificosId))
    pure item
  pure
    $ items
    & fmap
      ( \item ->
          M.MontoFijo
            (desdeUnidadesMinimas moneda item.monto_en_unidades_minimas)
            (M.ParticipanteId $ case item.participante of ParticipanteId ulid -> ulid)
      )

fetchDistribucionPartesItems :: M.Moneda -> ULID -> Pg [M.Parte]
fetchDistribucionPartesItems moneda distribucionPartesId = do
  items <- runSelectReturningList $ select $ do
    item <- all_ db.distribuciones_partes_items
    guard_ (item.distribucion ==. DistribucionPartesId (val_ distribucionPartesId))
    pure item
  pure
    $ items
    & fmap
      ( \item ->
          let participante = M.ParticipanteId $ case item.participante of ParticipanteId ulid -> ulid
          in case (desdeUnidadesMinimasMaybe moneda item.monto_en_unidades_minimas, item.cuota) of
               (Just monto, Just cuota) -> M.PonderadoYMontoFijo monto (fromIntegral cuota) participante
               (Just monto, Nothing) -> M.MontoFijo monto participante
               (Nothing, Just cuota) -> M.Ponderado (fromIntegral cuota) participante
               (Nothing, Nothing) -> panic $ "DistribucionPartesItem sin monto ni cuota: " <> show item.id
      )

fetchDistribucion :: M.Moneda -> ULID -> Pg (Maybe M.Distribucion)
fetchDistribucion moneda distribucionId = do
  dbDistribucion :: Distribucion <- fmap (fromMaybe (panic "Pago not found")) $ runSelectReturningOne $ select $ do
    distribucion <- all_ db.distribuciones
    guard_ (distribucion.id ==. val_ distribucionId)
    pure distribucion

  tipo <- case dbDistribucion.tipo of
    "DistribucionMontoEquitativo" -> do
      dbDistribucionMontoEquitativo :: Maybe DistribucionMontoEquitativo <- runSelectReturningOne $ select $ do
        distribucionMontoEquitativo <- all_ db.distribuciones_monto_equitativo
        guard_ (distribucionMontoEquitativo.distribucion ==. DistribucionId (val_ distribucionId))
        pure distribucionMontoEquitativo

      case dbDistribucionMontoEquitativo of
        Nothing -> pure Nothing
        Just dbDistrib -> do
          participantes <- fetchDistribucionMontoEquitativoItems dbDistrib.id
          -- Las distribuciones equitativas viejas se leen como partes con una
          -- ponderación de 1 para cada participante.
          pure
            $ Just
            $ M.TipoDistribucionPartes
            $ M.DistribucionPartes
              { M.id = dbDistrib.id
              , M.partes = participantes & fmap (M.Ponderado 1)
              }
    "DistribucionMontosEspecificos" -> do
      dbDistribucionMontosEspecificos :: Maybe DistribucionMontosEspecifico <- runSelectReturningOne $ select $ do
        distribucionMontosEspecificos <- all_ db.distribuciones_montos_especificos
        guard_ (distribucionMontosEspecificos.distribucion ==. DistribucionId (val_ distribucionId))
        pure distribucionMontosEspecificos

      case dbDistribucionMontosEspecificos of
        Nothing -> pure Nothing
        Just dbDistrib -> do
          partes <- fetchDistribucionMontosEspecificosItems moneda dbDistrib.id
          pure
            $ Just
            $ M.TipoDistribucionPartes
            $ M.DistribucionPartes
              { M.id = dbDistrib.id
              , M.partes = partes
              }
    "Repartija" -> do
      repartijaId <- fmap (fromMaybe (panic "Repartija not found")) $ runSelectReturningOne $ select $ do
        r <- all_ db.repartijas
        guard_ (r.distribucion ==. DistribucionId (val_ dbDistribucion.id))
        pure r.id
      repartija <- fetchRepartija repartijaId
      pure $ Just $ M.TipoDistribucionRepartija repartija.repartija
    "DistribucionPartes" -> do
      dbDistribucionPartes :: Maybe DistribucionPartes <- runSelectReturningOne $ select $ do
        distribucionPartes <- all_ db.distribuciones_partes
        guard_ (distribucionPartes.distribucion ==. DistribucionId (val_ distribucionId))
        pure distribucionPartes

      case dbDistribucionPartes of
        Nothing -> pure Nothing
        Just dbDistrib -> do
          partes <- fetchDistribucionPartesItems moneda dbDistrib.id
          pure
            $ Just
            $ M.TipoDistribucionPartes
            $ M.DistribucionPartes
              { M.id = dbDistrib.id
              , M.partes = partes
              }
    _ -> pure Nothing

  case tipo of
    (Just tipoData) ->
      pure
        $ Just
        $ M.Distribucion
          { M.id = dbDistribucion.id
          , M.tipo = tipoData
          }
    _ -> pure Nothing

-- | Los gastos de un grupo con su resumen. Con un participante, el resumen de
-- cada gasto viene recortado a esa persona: las filas de los demás no se leen
-- ni se serializan.
fetchShallowPagos :: ULID -> Maybe M.ParticipanteId -> Pg [M.ShallowPago]
fetchShallowPagos grupoId participanteId = do
  dbPagos <- runSelectReturningList $ select $ do
    pago <-
      all_ db.pagos
        & orderBy_ (desc_ . (.pagoId))
    guard_ (pago.pagoGrupo ==. GrupoId (val_ grupoId))
    pure pago

  -- Todas las filas del cache del grupo de una, no una query por gasto. Sin
  -- joinear 'pagos': la fila sabe de qué grupo es.
  filas <- runSelectReturningList $ select $ do
    fila <- all_ db.pagado_y_consumido_en_gasto
    guard_ (fila.grupo ==. GrupoId (val_ grupoId))
    forM_ participanteId $ \unParticipante ->
      guard_ (fila.participante ==. ParticipanteId (val_ (M.participanteId2ULID unParticipante)))
    pure fila

  let netosPorPago =
        filas
          & fmap (\fila -> (case fila.gasto of PagoId p -> p, [fila]))
          & Map.fromListWith (<>)

  -- Leer nunca calcula: el resumen de un gasto queda escrito por la misma
  -- transacción que lo modifica ('guardarResumenDeGasto'), así que acá siempre
  -- tiene que estar. Si falta es que algo lo dejó a medias, y eso se arregla
  -- corriendo el backfill, no disimulándolo en cada lectura.
  pure $ dbPagos & fmap (\pago -> toShallowPago pago (Map.findWithDefault [] pago.pagoId netosPorPago))

toShallowPago :: Pago -> [PagadoYConsumidoEnGasto] -> M.ShallowPago
toShallowPago pago filas =
  M.ShallowPago
    { M.pagoId = pago.pagoId
    , M.resumen = resumenDesde filas pago.resumen
    , M.nombre = pago.pagoNombre
    , M.monto = desdeUnidadesMinimas pago.pagoMoneda pago.pagoMontoEnUnidadesMinimas
    , M.moneda = pago.pagoMoneda
    , M.fecha = pago.fecha
    }

-- | Rearma el resumen de un gasto juntando sus dos mitades guardadas: los
-- números por participante salen de 'pagado_y_consumido_en_gasto' y el resto del jsonb.
resumenDesde :: [PagadoYConsumidoEnGasto] -> ResumenGuardado -> M.ResumenGasto
resumenDesde netos guardado =
  M.ResumenGasto
    { -- Los netos salen de las filas, que un cambio de formato del blob no toca:
      -- siguen siendo correctos aunque el resto venga en default.
      M.pagado = lado (.pagado_en_unidades_minimas) netos
    , M.consumido = lado (.consumido_en_unidades_minimas) netos
    , M.errores = guardado.errores
    , M.participantesEnRepartija = guardado.participantesEnRepartija
    }
  where
    lado columna =
      foldMap
        ( \fila ->
            M.mkDeuda
              (M.ParticipanteId $ case fila.participante of ParticipanteId p -> p)
              (desdeUnidadesMinimas fila.moneda (columna fila))
        )

-- | Los netos de un grupo sumados en la base, sin traer ningún gasto.
--
-- No filtra por el resumen guardado porque un gasto inválido no tiene filas: el
-- filtro vive en la escritura.
netosDeGrupo :: ULID -> Pg (M.PorMoneda (M.Netos M.Monto))
netosDeGrupo grupoId = do
  filas <- runSelectReturningList $ select $ do
    aggregate_
      -- El cast es necesario: en Postgres SUM(bigint) devuelve numeric, y sin
      -- él la fila no decodifica.
      ( \(participante, moneda, neto) ->
          (group_ participante, group_ moneda, cast_ (fromMaybe_ 0 (sum_ neto)) bigint)
      )
      $ do
        -- Una sola tabla: la fila del cache sabe de qué grupo es, así que la
        -- suma no toca 'pagos' para nada.
        fila <- all_ db.pagado_y_consumido_en_gasto
        guard_ (fila.grupo ==. GrupoId (val_ grupoId))
        let ParticipanteId participante = fila.participante
        pure
          ( participante
          , fila.moneda
          , fila.pagado_en_unidades_minimas - fila.consumido_en_unidades_minimas
          )
  pure
    $ filas
    & foldMap
      ( \(participante, moneda, neto) ->
          M.mkDeuda (M.ParticipanteId participante) (desdeUnidadesMinimas moneda neto)
            `M.enMoneda` moneda
      )

fetchParticipantes :: ULID -> Pg [M.Participante]
fetchParticipantes grupoId = do
  rows <- runSelectReturningList $ select $ orderBy_ (\(p, _) -> asc_ p.id) $ do
    p <- all_ db.participantes
    guard_ (p.grupo ==. GrupoId (val_ grupoId))
    mOwner <- leftJoin_ (all_ db.users) (\u -> just_ (primaryKey u) ==. p.user)
    pure (p, mOwner)
  pure $ fmap (\(p, mOwner) -> toModelParticipante p (fmap toModelUser mOwner)) rows

toModelParticipante :: Participante -> Maybe M.User -> M.Participante
toModelParticipante p mOwner =
  M.Participante
    { M.id = p.id
    , M.nombre = p.nombre
    , M.user = mOwner
    }

fetchParticipanteById :: ULID -> Pg M.Participante
fetchParticipanteById participanteId = do
  row <- runSelectReturningOne $ select $ do
    p <- all_ db.participantes
    guard_ (p.id ==. val_ participanteId)
    pure p
  case row of
    Nothing -> fail $ "participante vanished: " <> show participanteId
    Just p -> do
      mOwner <- case p.user of
        UserId (Just uid) -> fetchUserById uid
        UserId Nothing -> pure Nothing
      pure $ toModelParticipante p mOwner

addParticipante :: ULID -> Text -> Pg (Either Text M.Participante)
addParticipante grupoId name = do
  newId <- liftIO ULID.getULID
  runInsert
    $ insert db.participantes
    $ insertValues
      [ Participante newId (GrupoId grupoId) name (UserId Nothing)
      ]
  pure
    $ Right
    $ M.Participante
      { M.id = newId
      , M.nombre = name
      , M.user = Nothing
      }

-- | Claim a participante as belonging to a user. A user owns at most one
-- participante per grupo, enforced as a hard rule: the claim is refused if the
-- user already owns another participante in the same grupo (they must unclaim it
-- first). See 'M.ClaimRejection' for the ways it can be refused.
--
-- Re-claiming the participante you already own is a no-op that succeeds.
claimParticipante :: ULID -> ULID -> ULID -> Pg (Either M.ClaimRejection M.Participante)
claimParticipante grupoId participanteId userId = runExceptT $ do
  p <-
    ( runSelectReturningOne $ select $ do
        p <- all_ db.participantes
        guard_ (p.id ==. val_ participanteId)
        guard_ (p.grupo ==. GrupoId (val_ grupoId))
        pure p
    )
      `orElseMay` throwError M.ParticipanteNotFound
  case p.user of
    UserId (Just ownerId)
      -- Re-claiming your own participante succeeds without changes.
      | ownerId == userId -> lift $ fetchParticipanteById participanteId
      | otherwise -> throwError M.ClaimedByOtherUser
    UserId Nothing -> do
      -- Refuse if the user already owns another participante in this grupo.
      existingClaim <- runSelectReturningOne $ select $ do
        other <- all_ db.participantes
        guard_ (other.grupo ==. GrupoId (val_ grupoId))
        guard_ (other.user ==. UserId (val_ (Just userId)))
        pure other.id
      case existingClaim of
        Just _ -> throwError M.AlreadyOwnAnotherParticipante
        Nothing -> do
          runUpdate
            $ update
              db.participantes
              (\row -> row.user <-. UserId (val_ (Just userId)))
              (\row -> row.id ==. val_ participanteId)
          lift $ fetchParticipanteById participanteId

unclaimParticipante :: ULID -> ULID -> ULID -> Pg M.Participante
unclaimParticipante grupoId participanteId userId = do
  runUpdate
    $ update
      db.participantes
      (\row -> row.user <-. UserId (val_ Nothing))
      ( \row ->
          row.id
            ==. val_ participanteId
            &&. row.grupo
            ==. GrupoId (val_ grupoId)
            &&. row.user
            ==. UserId (val_ (Just userId))
      )
  fetchParticipanteById participanteId

deleteShallowParticipante :: ULID -> ULID -> Pg M.ParticipanteId
deleteShallowParticipante _grupoId participanteId = do
  runDelete
    $ delete
      db.participantes
      (\p -> p.id ==. val_ participanteId)
  pure $ M.ParticipanteId participanteId

participanteId2Persistent :: M.ParticipanteId -> ParticipanteId
participanteId2Persistent (M.ParticipanteId p) = ParticipanteId p

savePago :: ULID -> M.Pago -> Pg M.Pago
savePago grupoId pagoWithoutId = do
  pagoId <-
    if pagoWithoutId.pagoId == nullUlid
      then liftIO ULID.getULID
      else pure pagoWithoutId.pagoId
  let pago = pagoWithoutId{M.pagoId = pagoId} :: M.Pago

  distribucionesViejas <- runSelectReturningOne $ select $ do
    p <- all_ db.pagos
    guard_ (p.pagoId ==. val_ pagoId)
    pure (p.distribucion_pagadores, p.distribucion_deudores)

  distribucionPagadores <- saveDistribucion pago.moneda pago.pagadores
  distribucionDeudores <- saveDistribucion pago.moneda pago.deudores

  let pagoNuevo =
        pago
          { M.pagadores = distribucionPagadores
          , M.deudores = distribucionDeudores
          }

  let resumen = M.getResumenGasto pagoNuevo

  runInsert
    $ insertOnConflict
      db.pagos
      ( insertValues
          [ Pago
              { pagoId = pago.pagoId
              , pagoGrupo = GrupoId grupoId
              , pagoNombre = pago.nombre
              , pagoMontoEnUnidadesMinimas = aUnidadesMinimas pago.moneda pago.monto
              , pagoMoneda = pago.moneda
              , distribucion_pagadores = DistribucionId distribucionPagadores.id
              , distribucion_deudores = DistribucionId distribucionDeudores.id
              , resumen = ResumenGuardado.resumen2Guardado resumen
              , fecha = pago.fecha
              }
          ]
      )
      (conflictingFields (\p -> p.pagoId))
      onConflictUpdateAll

  forM_ distribucionesViejas $ \(DistribucionId viejaPagadores, DistribucionId viejaDeudores) -> do
    when (viejaPagadores /= distribucionPagadores.id) $ deleteDistribucion viejaPagadores
    when (viejaDeudores /= distribucionDeudores.id) $ deleteDistribucion viejaDeudores

  escribirPagadoYConsumido grupoId pagoNuevo resumen
  pure pagoNuevo

recomputePagos :: Connection -> IO ()
recomputePagos conn = go 0 nullUlid
  where
    recomputePagosBatchSize = 100

    -- 'total' es la cantidad de pagos ya recomputados (para loguear progreso).
    go :: Int -> ULID -> IO ()
    go total ultimoId = do
      resultado <- runBeamPostgres conn $ do
        lote <- runSelectReturningList $ select $ do
          limit_ recomputePagosBatchSize $ orderBy_ (asc_ . fst) $ do
            p <- all_ db.pagos
            guard_ (p.pagoId >. val_ ultimoId)
            pure (p.pagoId, p.pagoGrupo)
        case NE.nonEmpty lote of
          Nothing -> pure Nothing
          Just loteNE -> do
            forM_ loteNE $ \(pagoId, GrupoId grupoId) -> do
              pago <- fetchPago grupoId pagoId
              void $ savePago grupoId pago
            pure $ Just (NE.length loteNE, fst $ NE.last loteNE)
      case resultado of
        Nothing -> putText $ "recompute-pagos: listo, " <> show total <> " pagos recomputados"
        Just (procesados, siguienteId) -> do
          let total' = total + procesados
          putText $ "recompute-pagos: lote de " <> show procesados <> " procesado (" <> show total' <> " en total)"
          go total' siguienteId

saveDistribucion :: M.Moneda -> M.Distribucion -> Pg M.Distribucion
saveDistribucion moneda distribucionWithoutId = do
  distribucionId <-
    if distribucionWithoutId.id == nullUlid
      then liftIO ULID.getULID
      else pure distribucionWithoutId.id
  let distribucion = distribucionWithoutId{M.id = distribucionId} :: M.Distribucion

  oldTipo <- runSelectReturningOne $ select $ do
    d <- all_ db.distribuciones
    guard_ (d.id ==. val_ distribucionId)
    pure d.tipo

  let nuevoTipo = tipoDistribucionToText distribucion.tipo
  case oldTipo of
    Just t | t /= nuevoTipo -> deleteDistribucionSubtipo t distribucionId
    _ -> pure ()

  runInsert
    $ insertOnConflict
      db.distribuciones
      ( insertValues
          [ Distribucion distribucion.id nuevoTipo
          ]
      )
      (conflictingFields (\d -> d.id))
      onConflictUpdateAll
  case distribucionWithoutId.tipo of
    M.TipoDistribucionRepartija repartijaWithoutId -> do
      repartija <- saveRepartija moneda distribucion.id repartijaWithoutId
      pure $ distribucion{M.tipo = M.TipoDistribucionRepartija repartija}
    M.TipoDistribucionPartes tipoWithoutId -> do
      tipoId <-
        if tipoWithoutId.id == nullUlid
          then liftIO ULID.getULID
          else pure tipoWithoutId.id
      let tipo = tipoWithoutId{M.id = tipoId} :: M.DistribucionPartes

      runDelete
        $ delete
          db.distribuciones_partes
          (\dp -> dp.id /=. val_ tipo.id &&. dp.distribucion ==. val_ (DistribucionId distribucionId))

      runInsert
        $ insertOnConflict
          db.distribuciones_partes
          ( insertValues
              [ DistribucionPartes tipo.id (DistribucionId distribucion.id)
              ]
          )
          (conflictingFields (\dp -> (dp.id, dp.distribucion)))
          onConflictUpdateAll

      -- Las partes no tienen id propio asi que las reemplazamos todas
      runDelete
        $ delete
          db.distribuciones_partes_items
          (\item -> item.distribucion ==. DistribucionPartesId (val_ tipo.id))

      items <- forM tipo.partes $ \parte -> do
        itemId <- liftIO ULID.getULID
        let mkItem participante monto cuota =
              DistribucionPartesItem
                itemId
                (DistribucionPartesId tipo.id)
                (participanteId2Persistent participante)
                (aUnidadesMinimasMaybe moneda monto)
                (fromIntegral <$> cuota)
        pure $ case parte of
          M.MontoFijo monto participante ->
            mkItem participante (Just monto) Nothing
          M.Ponderado cuota participante ->
            mkItem participante Nothing (Just cuota)
          M.PonderadoYMontoFijo monto cuota participante ->
            mkItem participante (Just monto) (Just cuota)

      runInsert
        $ insert db.distribuciones_partes_items
        $ insertValues items
      pure $ distribucion{M.tipo = M.TipoDistribucionPartes tipo}

tipoDistribucionToText :: M.TipoDistribucion -> Text
tipoDistribucionToText tipo = case tipo of
  M.TipoDistribucionRepartija _ -> "Repartija"
  M.TipoDistribucionPartes _ -> "DistribucionPartes"

deletePago :: ULID -> Pg ()
deletePago unId = do
  maybePago <- runSelectReturningOne $ select $ do
    p <- all_ db.pagos
    guard_ (p.pagoId ==. val_ unId)
    pure p

  case maybePago of
    Nothing -> pure ()
    Just pago -> do
      let DistribucionId pagadoresId = pago.distribucion_pagadores
      let DistribucionId deudoresId = pago.distribucion_deudores

      runDelete
        $ delete
          db.pagos
          (\p -> p.pagoId ==. val_ unId)
      deleteDistribucion pagadoresId
      deleteDistribucion deudoresId

-- | Borra la fila padre del subtipo indicado por el texto de tipo guardado.
-- Los items/claims asociados se eliminan por las FKs con ON DELETE CASCADE,
-- así que sólo hay que borrar la fila padre. Cubre también los tipos legados
-- por si la distribución empezó siendo uno de ellos.
deleteDistribucionSubtipo :: Text -> ULID -> Pg ()
deleteDistribucionSubtipo tipo distribucionId = case tipo of
  "DistribucionPartes" ->
    runDelete
      $ delete
        db.distribuciones_partes
        (\d -> d.distribucion ==. val_ (DistribucionId distribucionId))
  "Repartija" ->
    runDelete
      $ delete
        db.repartijas
        (\r -> r.distribucion ==. val_ (DistribucionId distribucionId))
  "DistribucionMontoEquitativo" ->
    runDelete
      $ delete
        db.distribuciones_monto_equitativo
        (\d -> d.distribucion ==. val_ (DistribucionId distribucionId))
  "DistribucionMontosEspecificos" ->
    runDelete
      $ delete
        db.distribuciones_montos_especificos
        (\d -> d.distribucion ==. val_ (DistribucionId distribucionId))
  _ -> pure ()

deleteDistribucion :: ULID -> Pg ()
deleteDistribucion distribucionId = do
  -- Most references to distribucion have on delete cascade so
  -- we don't need to delete them manually
  runDelete
    $ delete
      db.distribuciones
      (\d -> d.id ==. val_ distribucionId)

updatePago :: ULID -> ULID -> M.Pago -> Pg M.Pago
updatePago grupoId pagoId pago = do
  savePago grupoId pago

saveRepartija :: M.Moneda -> ULID -> M.Repartija -> Pg M.Repartija
saveRepartija moneda distribucionId repartijaSinId = do
  repartijaId <-
    if repartijaSinId.id == nullUlid
      then liftIO ULID.getULID
      else pure repartijaSinId.id
  let repartija = repartijaSinId{M.id = repartijaId} :: M.Repartija

  runDelete
    $ delete
      db.repartijas
      (\r -> r.id /=. val_ repartija.id &&. r.distribucion ==. val_ (DistribucionId distribucionId))
  runInsert
    $ insertOnConflict
      db.repartijas
      ( insertValues
          [ Repartija
              { id = repartija.id
              , distribucion = DistribucionId distribucionId
              , extra_en_unidades_minimas = aUnidadesMinimas moneda repartija.extra
              , distribucion_de_sobras = distribucionDeSobrasToText repartija.distribucionDeSobras
              }
          ]
      )
      (conflictingFields (\r -> r.id))
      onConflictUpdateAll
  items <- saveRepartijaItems moneda repartijaId repartija.items

  claims <- claimsDeItems (fmap (RepartijaItemId . (.id)) items)
  pure
    repartija
      { M.items = items
      , M.claims = claims
      }

saveRepartijaItems :: M.Moneda -> ULID -> [M.RepartijaItem] -> Pg [M.RepartijaItem]
saveRepartijaItems moneda repartijaId repartijaItemsWithoutId = do
  repartijaItems <- forM repartijaItemsWithoutId $ \repartijaItem -> do
    itemId <-
      if repartijaItem.id == nullUlid
        then liftIO ULID.getULID
        else pure repartijaItem.id
    pure (repartijaItem{M.id = itemId} :: M.RepartijaItem)
  runDelete
    $ delete
      db.repartija_items
      ( \item ->
          item.repartijaitemRepartija
            ==. DistribucionRepartijaId (val_ repartijaId)
            &&. not_ (item.repartijaitemId `in_` [val_ i.id | i <- repartijaItems])
      )

  runInsert
    $ insertOnConflict
      db.repartija_items
      ( insertValues
          $ fmap
            ( \item ->
                RepartijaItem
                  { repartijaitemId = item.id
                  , repartijaitemRepartija = DistribucionRepartijaId repartijaId
                  , repartijaitemNombre = item.nombre
                  , repartijaitemMontoEnUnidadesMinimas = aUnidadesMinimas moneda item.monto
                  , repartijaitemCantidad = fromIntegral item.cantidad
                  }
            )
            repartijaItems
      )
      (conflictingFields (\item -> item.repartijaitemId))
      onConflictUpdateAll
  pure repartijaItems

fetchRepartija :: ULID -> Pg M.RepartijaForFrontend
fetchRepartija unRepartijaId = do
  (repartija, pagoNombre, pagoId, moneda) :: (DistribucionRepartija, Text, ULID, M.Moneda) <- fmap (fromMaybe (panic "Repartija not found")) $ runSelectReturningOne $ select $ do
    repartija <- all_ db.repartijas
    guard_ (repartija.id ==. val_ unRepartijaId)
    pago <- pagoDeRepartija repartija
    pure (repartija, pago.pagoNombre, pago.pagoId, pago.pagoMoneda)
  items :: [RepartijaItem] <- runSelectReturningList $ select $ do
    item <- all_ db.repartija_items
    guard_ $ item.repartijaitemRepartija ==. val_ (DistribucionRepartijaId repartija.id)
    pure item
  claims <- claimsDeItems (fmap (RepartijaItemId . (.repartijaitemId)) items)

  pure
    $ M.RepartijaForFrontend
      { repartija =
          M.Repartija
            { id = repartija.id
            , nombre = pagoNombre
            , extra = desdeUnidadesMinimas moneda repartija.extra_en_unidades_minimas
            , distribucionDeSobras = distribucionDeSobrasFromText repartija.distribucion_de_sobras
            , claims = claims
            , items =
                items
                  & fmap
                    ( \dbItem ->
                        M.RepartijaItem
                          { M.id = dbItem.repartijaitemId
                          , M.nombre = dbItem.repartijaitemNombre
                          , M.monto = desdeUnidadesMinimas moneda dbItem.repartijaitemMontoEnUnidadesMinimas
                          , M.cantidad = fromIntegral dbItem.repartijaitemCantidad
                          }
                    )
            }
      , pagoId = pagoId
      , pagoNombre = pagoNombre
      }

claimDesdeFila :: RepartijaClaim -> M.RepartijaClaim
claimDesdeFila r =
  M.RepartijaClaim
    { M.id = r.repartijaclaimId
    , M.cantidad = fromIntegral <$> r.repartijaclaimCantidad
    , M.participante = M.ParticipanteId $ case r.repartijaclaimParticipante of ParticipanteId ulid -> ulid
    , M.itemId = case r.repartijaclaimRepartijaItem of RepartijaItemId ulid -> ulid
    }

claimsDeItems :: [RepartijaItemId] -> Pg [M.RepartijaClaim]
claimsDeItems itemIds = do
  claims <- runSelectReturningList $ select $ do
    claim <- all_ db.repartija_claims
    guard_ (claim.repartijaclaimRepartijaItem `in_` fmap (val_) itemIds)
    pure claim
  pure $ fmap claimDesdeFila claims

-- | Guarda (o pisa) un claim y devuelve la repartija entera ya actualizada.
--
-- Recalcular el resumen del gasto obliga a leer el gasto completo, y ahí adentro
-- viene la repartija con todos sus claims: devolverla sale gratis y le ahorra al
-- frontend un GET extra después de cada click.
saveRepartijaClaim :: ULID -> M.RepartijaClaim -> Pg M.RepartijaForFrontend
saveRepartijaClaim repartijaId repartijaClaim = do
  claimId <-
    if repartijaClaim.id == nullUlid
      then liftIO ULID.getULID
      else pure repartijaClaim.id
  let claim' = repartijaClaim{M.id = claimId} :: M.RepartijaClaim
  grupoId <-
    fetchGrupoIdFromRepartija repartijaId
      `orElseMay` panic "grupoId not found"
  runInsert
    $ insertOnConflict
      db.repartija_claims
      (insertValues [claimToRow claim'])
      (conflictingFields (\c -> (c.repartijaclaimParticipante, c.repartijaclaimRepartijaItem)))
      onConflictUpdateAll
  -- (onConflictUpdateSet (\fields _oldValues ->
  --   repartijaClaimCantidad fields <-. val_ (fromIntegral <$> M.repartijaClaimCantidad claim')))
  maybePagoId <- fetchPagoIdFromRepartija repartijaId
  case maybePagoId of
    Nothing -> fetchRepartija repartijaId
    Just pagoId -> do
      pago <- recalcularResumenGasto grupoId pagoId
      case repartijaDePago repartijaId pago of
        Nothing -> fetchRepartija repartijaId
        Just repartija ->
          pure
            $ M.RepartijaForFrontend
              { repartija = repartija
              , pagoId = pago.pagoId
              , pagoNombre = pago.nombre
              }

-- | La repartija de un gasto, buscándola en las dos distribuciones (puede estar
-- tanto del lado de los pagadores como del de los deudores).
repartijaDePago :: ULID -> M.Pago -> Maybe M.Repartija
repartijaDePago repartijaId pago =
  [pago.pagadores, pago.deudores]
    & mapMaybe
      ( \distribucion -> case distribucion.tipo of
          M.TipoDistribucionRepartija repartija
            | repartija.id == repartijaId -> Just repartija
          _ -> Nothing
      )
    & head

deleteRepartijaClaim :: ULID -> Pg ()
deleteRepartijaClaim claimId = do
  -- Resolve the owning pago before deleting, since we navigate through the claim.
  pagoId <- fetchPagoIdFromClaim claimId
  grupoId <-
    fetchGrupoIdFromClaim claimId
      `orElseMay` panic "grupoId not found"
  runDelete
    $ delete
      db.repartija_claims
      (\c -> c.repartijaclaimId ==. val_ claimId)
  forM_ pagoId $ void . recalcularResumenGasto grupoId

-- | Recalcula y guarda el resumen de un gasto. Llamalo desde cualquier mutación
-- que pueda cambiar el reparto sin pasar por 'savePago' (editar los claims de
-- una repartija, por ejemplo).
-- Devuelve el gasto que leyó para recalcular, así quien la llama no lo tiene que
-- volver a pedir.
recalcularResumenGasto :: ULID -> ULID -> Pg M.Pago
recalcularResumenGasto grupoId pagoId = do
  pago <- fetchPago grupoId pagoId
  let resumen = M.getResumenGasto pago
  runUpdate
    $ update
      db.pagos
      (\p -> p.resumen <-. val_ (ResumenGuardado.resumen2Guardado resumen))
      (\p -> p.pagoId ==. val_ pago.pagoId)
  escribirPagadoYConsumido grupoId pago resumen
  pure pago

-- | Query fragment: the pago that owns a given repartija row, following
-- distribución → pago (one repartija belongs to one distribución, which is
-- referenced by exactly one pago, as either pagadores or deudores).
pagoDeRepartija ::
  (HasSqlEqualityCheck be ULID) =>
  DistribucionRepartijaT (QExpr be s)
  -> Q be BananaSplitDb s (PagoT (QExpr be s))
pagoDeRepartija repartija = do
  distrib <- all_ db.distribuciones
  guard_ (repartija.distribucion `references_` distrib)
  pago <- all_ db.pagos
  guard_ (pago.distribucion_pagadores `references_` distrib ||. pago.distribucion_deudores `references_` distrib)
  pure pago

-- | Query fragment: the pago that owns the repartija with the given id.
pagoDeRepartijaId ::
  (HasSqlEqualityCheck be ULID, BeamSqlBackendCanSerialize be ULID) =>
  ULID
  -> Q be BananaSplitDb s (PagoT (QExpr be s))
pagoDeRepartijaId repartijaId = do
  repartija <- all_ db.repartijas
  guard_ (repartija.id ==. val_ repartijaId)
  pagoDeRepartija repartija

-- | Query fragment: the pago that owns the claim with the given id, following
-- claim → item → repartija → pago.
pagoDeClaimId ::
  (HasSqlEqualityCheck be ULID, BeamSqlBackendCanSerialize be ULID) =>
  ULID
  -> Q be BananaSplitDb s (PagoT (QExpr be s))
pagoDeClaimId claimId = do
  claim <- all_ db.repartija_claims
  guard_ (claim.repartijaclaimId ==. val_ claimId)
  item <- all_ db.repartija_items
  guard_ (claim.repartijaclaimRepartijaItem `references_` item)
  repartija <- all_ db.repartijas
  guard_ (item.repartijaitemRepartija `references_` repartija)
  pagoDeRepartija repartija

grupoIdDePago :: PagoT (QExpr be s) -> QExpr be s ULID
grupoIdDePago pago = let GrupoId grupoId = pago.pagoGrupo in grupoId

fetchPagoIdFromRepartija :: ULID -> Pg (Maybe ULID)
fetchPagoIdFromRepartija repartijaId =
  runSelectReturningOne $ select $ (.pagoId) <$> pagoDeRepartijaId repartijaId

fetchPagoIdFromClaim :: ULID -> Pg (Maybe ULID)
fetchPagoIdFromClaim claimId =
  runSelectReturningOne $ select $ (.pagoId) <$> pagoDeClaimId claimId

claimToRow :: M.RepartijaClaim -> RepartijaClaim
claimToRow claim =
  RepartijaClaim
    { repartijaclaimId = claim.id
    , repartijaclaimParticipante = ParticipanteId $ M.participanteId2ULID claim.participante
    , repartijaclaimRepartijaItem = RepartijaItemId claim.itemId
    , repartijaclaimCantidad = fromIntegral <$> claim.cantidad
    }

-- | Deja las transferencias que hay que hacer para saldar el grupo. Vienen todas
-- en una sola moneda porque congelar consolida los netos con las tasas.
freezeGrupo :: ULID -> M.Moneda -> [M.Transferencia] -> Pg ()
freezeGrupo grupoId moneda transferencias = do
  ahora <- liftIO getCurrentTime
  borrarTransferenciasPendientes grupoId
  runUpdate
    $ update
      db.grupos
      (\g -> g.congelado_at <-. val_ (Just ahora))
      (\g -> g.id ==. val_ grupoId)
  filas <- liftIO $ forM transferencias $ \t -> do
    tid <- ULID.getULID
    pure $ transferenciaRow tid grupoId moneda t Nothing
  unless (null filas)
    $ runInsert
    $ insert db.transferencias
    $ insertValues filas

unfreezeGrupo :: ULID -> Pg ()
unfreezeGrupo grupoId = do
  borrarTransferenciasPendientes grupoId
  runUpdate
    $ update
      db.grupos
      (\g -> g.congelado_at <-. val_ Nothing)
      (\g -> g.id ==. val_ grupoId)

-- | Las pendientes son la sugerencia que dejó el congelamiento, así que
-- descongelar las tira. Las hechas se quedan: son plata que ya se movió y
-- siguen contando en los netos del grupo.
borrarTransferenciasPendientes :: ULID -> Pg ()
borrarTransferenciasPendientes grupoId =
  runDelete
    $ delete
      db.transferencias
      (\t -> t.grupo ==. GrupoId (val_ grupoId) &&. isNothing_ t.saldada_at)

transferenciaRow :: ULID -> ULID -> M.Moneda -> M.Transferencia -> Maybe UTCTime -> Transferencia
transferenciaRow transferenciaId grupoId moneda t saldadaAt =
  Transferencia
    { id = transferenciaId
    , grupo = GrupoId grupoId
    , participante_from = ParticipanteId $ M.participanteId2ULID t.from
    , participante_to = ParticipanteId $ M.participanteId2ULID t.to
    , monto_en_unidades_minimas = aUnidadesMinimas moneda t.monto
    , moneda = moneda
    , saldada_at = saldadaAt
    }

-- | Una transferencia que alguien hizo sin que el grupo estuviera congelado:
-- nace ya hecha, porque no hay un congelamiento que la haya sugerido.
crearTransferenciaSaldada :: ULID -> M.Moneda -> M.Transferencia -> Pg M.Transferencia
crearTransferenciaSaldada grupoId moneda transferencia = do
  transferenciaId <- liftIO ULID.getULID
  ahora <- liftIO getCurrentTime
  runInsert
    $ insert db.transferencias
    $ insertValues [transferenciaRow transferenciaId grupoId moneda transferencia (Just ahora)]
  pure
    M.Transferencia
      { M.id = Just transferenciaId
      , M.from = transferencia.from
      , M.to = transferencia.to
      , M.monto = transferencia.monto
      }

marcarTransferenciaSaldada :: ULID -> ULID -> Pg ()
marcarTransferenciaSaldada grupoId transferenciaId = do
  ahora <- liftIO getCurrentTime
  runUpdate
    $ update
      db.transferencias
      (\t -> t.saldada_at <-. val_ (Just ahora))
      (\t -> t.id ==. val_ transferenciaId &&. t.grupo ==. GrupoId (val_ grupoId))

-- | La vuelve a dejar pendiente. Solo tiene sentido con el grupo congelado, que
-- es lo único que le da lugar a una transferencia pendiente.
desmarcarTransferenciaSaldada :: ULID -> ULID -> Pg ()
desmarcarTransferenciaSaldada grupoId transferenciaId =
  runUpdate
    $ update
      db.transferencias
      (\t -> t.saldada_at <-. val_ Nothing)
      (\t -> t.id ==. val_ transferenciaId &&. t.grupo ==. GrupoId (val_ grupoId))

-- | La borra del todo, para cuando esa transferencia nunca pasó. Distinto de
-- desmarcarla, que la deja pendiente porque todavía hay que hacerla.
borrarTransferencia :: ULID -> ULID -> Pg ()
borrarTransferencia grupoId transferenciaId =
  runDelete
    $ delete
      db.transferencias
      (\t -> t.id ==. val_ transferenciaId &&. t.grupo ==. GrupoId (val_ grupoId))

updateGrupo :: ULID -> Text -> M.Moneda -> Pg ()
updateGrupo grupoId nombre monedaPorDefecto = do
  runUpdate
    $ update
      db.grupos
      ( \g ->
          mconcat
            [ g.nombre <-. val_ nombre
            , g.moneda_por_defecto <-. val_ monedaPorDefecto
            ]
      )
      (\g -> g.id ==. val_ grupoId)

-- | Una transferencia con lo que el modelo de dominio no lleva encima: en qué
-- moneda está y cuándo se hizo, si se hizo.
data TransferenciaGuardada = TransferenciaGuardada
  { transferencia :: M.Transferencia
  , moneda :: M.Moneda
  , saldadaAt :: Maybe UTCTime
  }
  deriving (Show, Eq)

fetchTransferencias :: ULID -> Pg [TransferenciaGuardada]
fetchTransferencias grupoId = do
  rows <- runSelectReturningList $ select $ do
    t <-
      all_ db.transferencias
        & orderBy_ (asc_ . (.id))
    guard_ (t.grupo ==. GrupoId (val_ grupoId))
    pure t
  pure
    $ rows
    & fmap
      ( \t ->
          TransferenciaGuardada
            { transferencia =
                M.Transferencia
                  { M.id = Just t.id
                  , M.from = M.ParticipanteId $ case t.participante_from of ParticipanteId ulid -> ulid
                  , M.to = M.ParticipanteId $ case t.participante_to of ParticipanteId ulid -> ulid
                  , M.monto = desdeUnidadesMinimas t.moneda t.monto_en_unidades_minimas
                  }
            , moneda = t.moneda
            , saldadaAt = t.saldada_at
            }
      )

-- | Lo que el grupo todavía tiene que saldar: solo existe si está congelado.
transferenciasPendientes :: [TransferenciaGuardada] -> M.PorMoneda [M.Transferencia]
transferenciasPendientes =
  agruparPorMoneda . filter (isNothing . (.saldadaAt))

transferenciasHechas :: [TransferenciaGuardada] -> M.PorMoneda [M.TransferenciaHecha]
transferenciasHechas guardadas =
  guardadas
    & mapMaybe
      ( \t ->
          t.saldadaAt
            & fmap
              ( \saldadaAt ->
                  ( t.moneda
                  , M.TransferenciaHecha{M.transferencia = t.transferencia, M.saldadaAt = saldadaAt}
                  )
              )
      )
    & foldMap (\(moneda, hecha) -> [hecha] `M.enMoneda` moneda)

agruparPorMoneda :: [TransferenciaGuardada] -> M.PorMoneda [M.Transferencia]
agruparPorMoneda =
  foldMap (\t -> [t.transferencia] `M.enMoneda` t.moneda)

-- | En qué monedas hay pagos cargados. Es lo único que las pantallas de monedas
-- necesitan saber de los pagos, así que no hace falta hidratarlos para eso.
fetchMonedasConPagos :: ULID -> Pg [M.Moneda]
fetchMonedasConPagos grupoId =
  runSelectReturningList $ select $ nub_ $ do
    pago <- all_ db.pagos
    guard_ (pago.pagoGrupo ==. GrupoId (val_ grupoId))
    pure pago.pagoMoneda

fetchTasasDeCambio :: ULID -> Pg [M.TasaDeCambio]
fetchTasasDeCambio grupoId = do
  rows <- runSelectReturningList $ select $ do
    tasa <-
      all_ db.tasas_de_cambio
        & orderBy_ (asc_ . (.id))
    guard_ (tasa.grupo ==. GrupoId (val_ grupoId))
    pure tasa
  pure
    $ rows
    & fmap
      ( \tasa ->
          M.TasaDeCambio
            { M.id = tasa.id
            , M.unaMoneda = tasa.una_moneda
            , M.otraMoneda = tasa.otra_moneda
            , M.unMonto = desdeUnidadesMinimas tasa.una_moneda tasa.un_monto_en_unidades_minimas
            , M.otroMonto = desdeUnidadesMinimas tasa.otra_moneda tasa.otro_monto_en_unidades_minimas
            }
      )

-- | Deja primero la moneda que va antes por código, que es el orden en que se
-- guarda el par y lo que pide el CHECK de la tabla. El @from@ y el @to@ existen
-- únicamente en las columnas, para poder guardar en dos lugares un par que en
-- el modelo no tiene sentido.
--
-- Comparamos por código y no por 'Ord' 'M.Moneda', que se movería al agregar
-- una moneda al medio del @data Moneda@ y desordenaría en silencio lo que ya
-- está guardado.
normalizarTasa :: M.TasaDeCambio -> M.TasaDeCambio
normalizarTasa tasa
  | (show tasa.unaMoneda :: Text) <= show tasa.otraMoneda = tasa
  | otherwise =
      tasa
        { M.unaMoneda = tasa.otraMoneda
        , M.otraMoneda = tasa.unaMoneda
        , M.unMonto = tasa.otroMonto
        , M.otroMonto = tasa.unMonto
        }

-- | Reemplaza todas las tasas que involucran a esa moneda. Que las tasas tengan
-- sentido lo decide 'M.validarTablaDeTasas' antes de llegar acá; lo único que
-- agrega esta capa es el orden del par.
guardarTasasDeCambio :: ULID -> M.Moneda -> [M.TasaDeCambio] -> Pg [M.TasaDeCambio]
guardarTasasDeCambio grupoId moneda tasas = do
  runDelete
    $ delete
      db.tasas_de_cambio
      ( \tasa ->
          (tasa.grupo ==. GrupoId (val_ grupoId))
            &&. (tasa.una_moneda ==. val_ moneda ||. tasa.otra_moneda ==. val_ moneda)
      )

  unless (null tasas) $ do
    liftIO
      ( forM tasas $ \tasa -> do
          tasaId <- ULID.getULID
          let normalizada = normalizarTasa tasa
          pure
            $ TasaDeCambio
              { id = tasaId
              , grupo = GrupoId grupoId
              , una_moneda = normalizada.unaMoneda
              , otra_moneda = normalizada.otraMoneda
              , un_monto_en_unidades_minimas = aUnidadesMinimas normalizada.unaMoneda normalizada.unMonto
              , otro_monto_en_unidades_minimas = aUnidadesMinimas normalizada.otraMoneda normalizada.otroMonto
              }
      )
      >>= (runInsert . insert db.tasas_de_cambio . insertValues)

  fetchTasasDeCambio grupoId

fetchGrupoIdFromRepartija :: ULID -> Pg (Maybe ULID)
fetchGrupoIdFromRepartija repartijaId =
  runSelectReturningOne $ select $ grupoIdDePago <$> pagoDeRepartijaId repartijaId

fetchGrupoIdFromClaim :: ULID -> Pg (Maybe ULID)
fetchGrupoIdFromClaim claimId =
  runSelectReturningOne $ select $ grupoIdDePago <$> pagoDeClaimId claimId

distribucionDeSobrasToText :: M.DistribucionDeSobras -> Text
distribucionDeSobrasToText = \case
  M.SobrasNoDistribuir -> "SobrasNoDistribuir"
  M.SobrasProporcional -> "SobrasProporcional"

distribucionDeSobrasFromText :: Text -> M.DistribucionDeSobras
distribucionDeSobrasFromText = \case
  "SobrasNoDistribuir" -> M.SobrasNoDistribuir
  "SobrasProporcional" -> M.SobrasProporcional
  other -> panic $ "Unknown DistribucionDeSobras: " <> other
