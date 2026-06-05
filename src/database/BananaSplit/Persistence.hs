{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module BananaSplit.Persistence (
  MissingPGRollSchema (..),
  makePool,
  runMigration,
  addParticipante,
  createGrupo,
  db,
  deletePago,
  deleteRepartijaClaim,
  deleteShallowParticipante,
  deleteTransaccionCongelada,
  fetchGrupo,
  fetchGrupoIdFromClaim,
  fetchGrupoIdFromRepartija,
  fetchPago,
  fetchRepartija,
  fetchShallowPagos,
  fetchTransaccionesCongeladas,
  freezeGrupo,
  savePago,
  saveRepartija,
  saveRepartijaClaim,
  unfreezeGrupo,
  updateGrupo,
  updateIsValidPago,
  updatePago,
) where

import Conferer qualified
import Data.Decimal qualified as Decimal
import Data.Pool qualified as Pool
import Data.String (String)
import Database.Beam as Beam
import Database.Beam.Backend.SQL (BeamSqlBackendCanSerialize)
import Database.Beam.Postgres
import Database.Beam.Postgres.Full hiding (insert)
import Database.PostgreSQL.Simple (Only (..), execute, query)

import BananaSplit qualified as M
import BananaSplit.Persistence.Migration_2026_05_26_FixDates qualified as FixDates
import BananaSplit.Persistence.Schema
import BananaSplit.PgRoll qualified as PgRoll
import BananaSplit.ULID (ULID, nullUlid)
import BananaSplit.ULID qualified as ULID
import Preludat

runMigration :: Conferer.Config -> [String] -> IO ()
runMigration config args = do
  connString <- Conferer.fetchFromConfig "database.url" config
  schema <- PgRoll.getLatestSchema
  conn <- connectPostgreSQL connString
  _ <- execute conn "SET search_path TO ?" (Only schema)
  case args of
    ["fix-pagos-fecha"] -> do
      runBeamPostgres conn FixDates.run
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
createGrupo nombre participante = do
  newId <- liftIO ULID.getULID
  runInsert
    $ insert db.grupos
    $ insertValues
      [ Grupo newId nombre False (M.ARS)
      ]
  p <-
    addParticipante newId participante
      `orElse` \e -> fail $ show e

  pure
    $ M.Grupo
      { M.id = newId
      , M.nombre = nombre
      , M.pagos = []
      , M.participantes = [p]
      , M.monedaPorDefecto = M.ARS
      }

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
      pure
        $ Just
        $ M.ShallowGrupo
          { M.id = grupo.id
          , M.nombre = grupo.nombre
          , M.participantes = participantes
          , M.isFrozen = grupo.is_frozen
          , M.monedaPorDefecto = grupo.moneda_por_defecto
          }

fetchPago :: ULID -> Pg M.Pago
fetchPago pagoId = do
  (dbPago :: Pago) <-
    fromMaybe (panic "Pago not found")
      <$> runSelectReturningOne
        ( select $ do
            pago <- all_ db.pagos
            guard_ (pago.pagoId ==. val_ pagoId)
            pure pago
        )

  (pagadores :: M.Distribucion) <- fromMaybe (panic "Pagadores not found") <$> fetchDistribucion (case dbPago.distribucion_pagadores of DistribucionId ulid -> ulid)
  (deudores :: M.Distribucion) <- fromMaybe (panic "deudores not found") <$> fetchDistribucion (case dbPago.distribucion_deudores of DistribucionId ulid -> ulid)
  dbPago
    & ( \p ->
          M.Pago
            { M.pagoId = p.pagoId
            , M.monto = constructMonto p.pagoMonto
            , M.moneda = dbPago.pagoMoneda
            , M.nombre = p.pagoNombre
            , M.isValid = p.pagoIsValid
            , M.fecha = p.fecha
            , M.pagadores = pagadores
            , M.deudores = deudores
            }
            & M.addIsValidPago
      )
    & pure

updateIsValidPago :: M.Pago -> Pg M.Pago
updateIsValidPago pago = do
  runUpdate
    $ update
      db.pagos
      (\p -> p.pagoIsValid <-. val_ pago.isValid)
      (\p -> p.pagoId ==. val_ pago.pagoId)
  pure pago

deconstructMonto :: M.Monto -> Monto
deconstructMonto (M.Monto (Decimal.Decimal l v)) =
  Monto (fromIntegral l) (fromIntegral v)

deconstructMontoMaybe :: Maybe M.Monto -> MontoT (Nullable Identity)
deconstructMontoMaybe Nothing = Monto Nothing Nothing
deconstructMontoMaybe (Just (M.Monto (Decimal.Decimal l v))) =
  Monto (Just $ fromIntegral l) (Just $ fromIntegral v)

constructMonto :: Monto -> M.Monto
constructMonto (Monto l v) =
  M.Monto (Decimal.Decimal (fromIntegral l) (fromIntegral v))

constructMontoMaybe :: MontoT (Nullable Identity) -> Maybe M.Monto
constructMontoMaybe (Monto Nothing Nothing) = Nothing
constructMontoMaybe (Monto (Just l) (Just v)) =
  Just (M.Monto (Decimal.Decimal (fromIntegral l) (fromIntegral v)))
constructMontoMaybe (Monto Nothing (Just _)) =
  Nothing
constructMontoMaybe (Monto (Just _) Nothing) =
  Nothing

fetchDistribucionMontoEquitativoItems :: ULID -> Pg [M.ParticipanteId]
fetchDistribucionMontoEquitativoItems distribucionMontoEquitativoId = do
  items <- runSelectReturningList $ select $ do
    item <- all_ db.distribuciones_monto_equitativo_items
    guard_ (item.distribucion ==. DistribucionMontoEquitativoId (val_ distribucionMontoEquitativoId))
    pure item
  pure $ items & fmap (\item -> M.ParticipanteId $ case item.participante of ParticipanteId ulid -> ulid)

fetchDistribucionMontosEspecificosItems :: ULID -> Pg [M.MontoEspecifico]
fetchDistribucionMontosEspecificosItems distribucionMontosEspecificosId = do
  items <- runSelectReturningList $ select $ do
    item <- all_ db.distribuciones_montos_especificos_items
    guard_ (item.distribucion ==. DistribucionMontosEspecificosId (val_ distribucionMontosEspecificosId))
    pure item
  pure
    $ items
    & fmap
      ( \item ->
          M.MontoEspecifico
            { M.id = item.id
            , M.participante = M.ParticipanteId $ case item.participante of ParticipanteId ulid -> ulid
            , M.monto = constructMonto item.monto
            }
      )

fetchDistribucion :: ULID -> Pg (Maybe M.Distribucion)
fetchDistribucion distribucionId = do
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
          pure
            $ Just
            $ M.TipoDistribucionMontoEquitativo
            $ M.DistribucionMontoEquitativo
              { M.id = dbDistrib.id
              , M.participantes = participantes
              }
    "DistribucionMontosEspecificos" -> do
      dbDistribucionMontosEspecificos :: Maybe DistribucionMontosEspecifico <- runSelectReturningOne $ select $ do
        distribucionMontosEspecificos <- all_ db.distribuciones_montos_especificos
        guard_ (distribucionMontosEspecificos.distribucion ==. DistribucionId (val_ distribucionId))
        pure distribucionMontosEspecificos

      case dbDistribucionMontosEspecificos of
        Nothing -> pure Nothing
        Just dbDistrib -> do
          montos <- fetchDistribucionMontosEspecificosItems dbDistrib.id
          pure
            $ Just
            $ M.TipoDistribucionMontosEspecificos
            $ M.DistribucionMontosEspecificos
              { M.id = dbDistrib.id
              , M.montos = montos
              }
    "Repartija" -> do
      repartijaId <- fmap (fromMaybe (panic "Repartija not found")) $ runSelectReturningOne $ select $ do
        r <- all_ db.repartijas
        guard_ (r.distribucion ==. DistribucionId (val_ dbDistribucion.id))
        pure r.id
      repartija <- fetchRepartija repartijaId
      pure $ Just $ M.TipoDistribucionRepartija repartija.repartija
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

fetchShallowPagos :: ULID -> Pg [M.ShallowPago]
fetchShallowPagos grupoId = do
  dbPagos <- runSelectReturningList $ select $ do
    pago <-
      all_ db.pagos
        & orderBy_ (desc_ . (.pagoId))
    guard_ (pago.pagoGrupo ==. GrupoId (val_ grupoId))
    pure pago

  forM dbPagos $ \pago -> do
    pure
      M.ShallowPago
        { M.pagoId = pago.pagoId
        , M.isValid = pago.pagoIsValid
        , M.nombre = pago.pagoNombre
        , M.monto = constructMonto pago.pagoMonto
        , M.moneda = pago.pagoMoneda
        , M.fecha = pago.fecha
        }

fetchParticipantes :: ULID -> Pg [M.Participante]
fetchParticipantes grupoId = do
  participantes <- runSelectReturningList $ select $ do
    p <-
      all_ db.participantes
        & orderBy_ (asc_ . (.id))
    guard_ (p.grupo ==. GrupoId (val_ grupoId))
    pure p
  pure
    $ fmap
      ( \p ->
          M.Participante
            { M.id = p.id
            , M.nombre = p.nombre
            }
      )
      participantes

addParticipante :: ULID -> Text -> Pg (Either Text M.Participante)
addParticipante grupoId name = do
  newId <- liftIO ULID.getULID
  runInsert
    $ insert db.participantes
    $ insertValues
      [ Participante newId (GrupoId grupoId) name
      ]
  pure
    $ Right
    $ M.Participante
      { M.id = newId
      , M.nombre = name
      }

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
  let pago = (pagoWithoutId{M.pagoId = pagoId} :: M.Pago) & M.addIsValidPago
  distribucionPagadores <- saveDistribucion pago.pagadores
  distribucionDeudores <- saveDistribucion pago.deudores
  runInsert
    $ insertOnConflict
      db.pagos
      ( insertValues
          [ Pago
              { pagoId = pago.pagoId
              , pagoIsValid = pago.isValid
              , pagoGrupo = GrupoId grupoId
              , pagoNombre = pago.nombre
              , pagoMonto = deconstructMonto pago.monto
              , pagoMoneda = pago.moneda
              , distribucion_pagadores = DistribucionId distribucionPagadores.id
              , distribucion_deudores = DistribucionId distribucionDeudores.id
              , fecha = pago.fecha
              }
          ]
      )
      (conflictingFields (\p -> p.pagoId))
      onConflictUpdateAll

  pure pago{M.pagadores = distribucionPagadores, M.deudores = distribucionDeudores}

saveDistribucion :: M.Distribucion -> Pg M.Distribucion
saveDistribucion distribucionWithoutId = do
  distribucionId <-
    if distribucionWithoutId.id == nullUlid
      then liftIO ULID.getULID
      else pure distribucionWithoutId.id
  let distribucion = distribucionWithoutId{M.id = distribucionId} :: M.Distribucion

  runInsert
    $ insertOnConflict
      db.distribuciones
      ( insertValues
          [ Distribucion distribucion.id (tipoDistribucionToText distribucion.tipo)
          ]
      )
      (conflictingFields (\d -> d.id))
      onConflictUpdateAll
  case distribucionWithoutId.tipo of
    M.TipoDistribucionMontoEquitativo tipoWithoutId -> do
      tipoId <-
        if tipoWithoutId.id == nullUlid
          then liftIO ULID.getULID
          else pure tipoWithoutId.id
      let tipo = tipoWithoutId{M.id = tipoId} :: M.DistribucionMontoEquitativo

      runDelete
        $ delete
          db.distribuciones_monto_equitativo
          (\dme -> dme.id /=. val_ tipo.id &&. dme.distribucion ==. val_ (DistribucionId distribucionId))

      runInsert
        $ insertOnConflict
          db.distribuciones_monto_equitativo
          ( insertValues
              [ DistribucionMontoEquitativo tipo.id (DistribucionId distribucion.id)
              ]
          )
          (conflictingFields (\dme -> (dme.id, dme.distribucion)))
          onConflictUpdateAll

      runDelete
        $ delete
          db.distribuciones_monto_equitativo_items
          ( \item ->
              item.distribucion
                ==. DistribucionMontoEquitativoId (val_ tipo.id)
                &&. not_ (item.participante `in_` [ParticipanteId (val_ $ M.participanteId2ULID p) | p <- tipo.participantes])
          )
      runInsert
        $ insertOnConflict
          db.distribuciones_monto_equitativo_items
          ( insertValues
              [DistribucionMontoEquitativoItem (DistribucionMontoEquitativoId tipo.id) (participanteId2Persistent p) | p <- tipo.participantes]
          )
          (conflictingFields (\item -> (item.distribucion, item.participante)))
          onConflictUpdateAll
      pure $ distribucion{M.tipo = M.TipoDistribucionMontoEquitativo tipo}
    M.TipoDistribucionMontosEspecificos tipoWithoutId -> do
      montosEspecificos <- forM tipoWithoutId.montos $ \monto -> do
        montoId <-
          if monto.id == nullUlid
            then liftIO ULID.getULID
            else pure monto.id
        pure (monto{M.id = montoId} :: M.MontoEspecifico)

      tipoId <-
        if tipoWithoutId.id == nullUlid
          then liftIO ULID.getULID
          else pure tipoWithoutId.id

      let tipo = tipoWithoutId{M.id = tipoId, M.montos = montosEspecificos} :: M.DistribucionMontosEspecificos

      runDelete
        $ delete
          db.distribuciones_montos_especificos
          (\dme -> dme.id /=. val_ tipo.id &&. dme.distribucion ==. val_ (DistribucionId distribucionId))

      runInsert
        $ insertOnConflict
          db.distribuciones_montos_especificos
          ( insertValues
              [ DistribucionMontosEspecificos tipoId (DistribucionId distribucion.id)
              ]
          )
          (conflictingFields (\dme -> (dme.id, dme.distribucion)))
          onConflictUpdateAll

      runDelete
        $ delete
          db.distribuciones_montos_especificos_items
          ( \item ->
              item.distribucion
                ==. val_ (DistribucionMontosEspecificosId tipo.id)
                &&. not_ (item.id `in_` [val_ m.id | m <- tipo.montos])
          )

      runInsert
        $ insertOnConflict
          db.distribuciones_montos_especificos_items
          ( insertValues
              [DistribucionMontosEspecificosItem m.id (DistribucionMontosEspecificosId tipo.id) (participanteId2Persistent m.participante) (deconstructMonto m.monto) | m <- tipo.montos]
          )
          (conflictingFields (\item -> item.id))
          onConflictUpdateAll
      pure $ distribucion{M.tipo = M.TipoDistribucionMontosEspecificos tipo}
    M.TipoDistribucionRepartija repartijaWithoutId -> do
      repartija <- saveRepartija distribucion.id repartijaWithoutId
      pure $ distribucion{M.tipo = M.TipoDistribucionRepartija repartija}

tipoDistribucionToText :: M.TipoDistribucion -> Text
tipoDistribucionToText tipo = case tipo of
  M.TipoDistribucionMontosEspecificos _ -> "DistribucionMontosEspecificos"
  M.TipoDistribucionMontoEquitativo _ -> "DistribucionMontoEquitativo"
  M.TipoDistribucionRepartija _ -> "Repartija"

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

saveRepartija :: ULID -> M.Repartija -> Pg M.Repartija
saveRepartija distribucionId repartijaSinId = do
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
              , extra = deconstructMonto repartija.extra
              , distribucion_de_sobras = distribucionDeSobrasToText repartija.distribucionDeSobras
              }
          ]
      )
      (conflictingFields (\r -> r.id))
      onConflictUpdateAll
  items <- saveRepartijaItems repartijaId repartija.items
  pure
    repartija
      { M.items = items
      }

saveRepartijaItems :: ULID -> [M.RepartijaItem] -> Pg [M.RepartijaItem]
saveRepartijaItems repartijaId repartijaItemsWithoutId = do
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
                  , repartijaitemMonto = deconstructMonto item.monto
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
  (repartija, pagoNombre, pagoId) :: (DistribucionRepartija, Text, ULID) <- fmap (fromMaybe (panic "Repartija not found")) $ runSelectReturningOne $ select $ do
    repartija <- all_ db.repartijas
    guard_ (repartija.id ==. val_ unRepartijaId)
    pago <- pagoDeRepartija repartija
    pure (repartija, pago.pagoNombre, pago.pagoId)
  items :: [RepartijaItem] <- runSelectReturningList $ select $ do
    item <- all_ db.repartija_items
    guard_ $ item.repartijaitemRepartija ==. val_ (DistribucionRepartijaId repartija.id)
    pure item
  claims :: [RepartijaClaim] <- runSelectReturningList $ select $ do
    claim <- all_ db.repartija_claims
    guard_ $ claim.repartijaclaimRepartijaItem `in_` fmap (val_ . RepartijaItemId . (.repartijaitemId)) items
    pure claim

  pure
    $ M.RepartijaForFrontend
      { repartija =
          M.Repartija
            { id = repartija.id
            , nombre = pagoNombre
            , extra = constructMonto repartija.extra
            , distribucionDeSobras = distribucionDeSobrasFromText repartija.distribucion_de_sobras
            , claims =
                claims
                  & fmap
                    ( \r ->
                        M.RepartijaClaim
                          { M.id = r.repartijaclaimId
                          , M.cantidad = fromIntegral <$> r.repartijaclaimCantidad
                          , M.participante = M.ParticipanteId $ case r.repartijaclaimParticipante of ParticipanteId ulid -> ulid
                          , M.itemId = case r.repartijaclaimRepartijaItem of RepartijaItemId ulid -> ulid
                          }
                    )
            , items =
                items
                  & fmap
                    ( \dbItem ->
                        M.RepartijaItem
                          { M.id = dbItem.repartijaitemId
                          , M.nombre = dbItem.repartijaitemNombre
                          , M.monto = constructMonto dbItem.repartijaitemMonto
                          , M.cantidad = fromIntegral dbItem.repartijaitemCantidad
                          }
                    )
            }
      , pagoId = pagoId
      , pagoNombre = pagoNombre
      }

saveRepartijaClaim :: ULID -> M.RepartijaClaim -> Pg M.RepartijaClaim
saveRepartijaClaim repartijaId repartijaClaim = do
  claimId <-
    if repartijaClaim.id == nullUlid
      then liftIO ULID.getULID
      else pure repartijaClaim.id
  let claim' = repartijaClaim{M.id = claimId} :: M.RepartijaClaim
  runInsert
    $ insertOnConflict
      db.repartija_claims
      (insertValues [claimToRow claim'])
      (conflictingFields (\c -> (c.repartijaclaimParticipante, c.repartijaclaimRepartijaItem)))
      onConflictUpdateAll
  -- (onConflictUpdateSet (\fields _oldValues ->
  --   repartijaClaimCantidad fields <-. val_ (fromIntegral <$> M.repartijaClaimCantidad claim')))
  fetchPagoIdFromRepartija repartijaId >>= traverse_ recalcValidezPago
  pure claim'

deleteRepartijaClaim :: ULID -> Pg ()
deleteRepartijaClaim claimId = do
  -- Resolve the owning pago before deleting, since we navigate through the claim.
  pagoId <- fetchPagoIdFromClaim claimId
  runDelete
    $ delete
      db.repartija_claims
      (\c -> c.repartijaclaimId ==. val_ claimId)
  forM_ pagoId recalcValidezPago

-- | Recompute and persist a pago's @isValid@ flag. Call this from any mutation
-- that can affect a pago's validity without going through 'savePago' (e.g.
-- editing repartija claims), so the stored flag never goes stale.
recalcValidezPago :: ULID -> Pg ()
recalcValidezPago pagoId = do
  pago <- fetchPago pagoId
  void $ updateIsValidPago (pago & M.addIsValidPago)

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

freezeGrupo :: ULID -> M.PorMoneda [M.Transaccion] -> Pg ()
freezeGrupo grupoId transaccionesPorMoneda = do
  runDelete
    $ delete
      db.transacciones_congeladas
      (\tc -> tc.grupo ==. GrupoId (val_ grupoId))
  runUpdate
    $ update
      db.grupos
      (\g -> g.is_frozen <-. val_ True)
      (\g -> g.id ==. val_ grupoId)
  transaccionesCongeladas <- liftIO $ M.forMonedaM transaccionesPorMoneda $ \moneda transacciones ->
    forM transacciones $ \t -> do
      tid <- ULID.getULID
      pure
        $ TransaccionCongelada
          { id = tid
          , grupo = GrupoId grupoId
          , participante_from = ParticipanteId $ M.participanteId2ULID t.from
          , participante_to = ParticipanteId $ M.participanteId2ULID t.to
          , monto = deconstructMonto t.monto
          , moneda = moneda
          }
  runInsert
    $ insert db.transacciones_congeladas
    $ insertValues
    $ transaccionesCongeladas

unfreezeGrupo :: ULID -> Pg ()
unfreezeGrupo grupoId = do
  runDelete
    $ delete
      db.transacciones_congeladas
      (\tc -> tc.grupo ==. GrupoId (val_ grupoId))
  runUpdate
    $ update
      db.grupos
      (\g -> g.is_frozen <-. val_ False)
      (\g -> g.id ==. val_ grupoId)

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

fetchTransaccionesCongeladas :: ULID -> Pg (M.PorMoneda [M.Transaccion])
fetchTransaccionesCongeladas grupoId = do
  rows <- runSelectReturningList $ select $ do
    tc <- all_ db.transacciones_congeladas
    guard_ (tc.grupo ==. GrupoId (val_ grupoId))
    pure tc
  pure
    $ rows
    & fmap
      ( \tc ->
          [ M.Transaccion
              { M.id = Just tc.id
              , M.from = M.ParticipanteId $ case tc.participante_from of ParticipanteId ulid -> ulid
              , M.to = M.ParticipanteId $ case tc.participante_to of ParticipanteId ulid -> ulid
              , M.monto = constructMonto tc.monto
              }
          ]
            `M.enMoneda` tc.moneda
      )
    & mconcat

deleteTransaccionCongelada :: ULID -> Pg ()
deleteTransaccionCongelada transaccionId = do
  runDelete
    $ delete
      db.transacciones_congeladas
      (\tc -> tc.id ==. val_ transaccionId)

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
