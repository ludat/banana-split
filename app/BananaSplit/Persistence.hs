{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module BananaSplit.Persistence
    ( addParticipante
    , createGrupo
    , deletePago
    , deleteRepartijaClaim
    , deleteShallowParticipante
    , fetchGrupo
    , fetchPago
    , fetchRepartija
    , fetchShallowPagos
    , savePago
    , saveRepartija
    , saveRepartijaClaim
    , updatePago
    ) where

import BananaSplit qualified as M
import BananaSplit.ULID (ULID, nullUlid)
import BananaSplit.ULID qualified as ULID

import Data.Decimal qualified as Decimal
import Data.Text qualified as Text

import Database.Beam as Beam
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.Beam.Postgres.Full hiding (insert)
import Database.Beam.Postgres.Syntax
import Database.PostgreSQL.Simple.FromField (FromField (..), returnError)

import Protolude
import Protolude.Error

data BananaSplitDb f = BananaSplitDb
  { grupos :: f (TableEntity GrupoT)
  , participantes :: f (TableEntity ParticipanteT)
  , pagos :: f (TableEntity PagoT)
  , _bananasplitDistribuciones :: f (TableEntity DistribucionT)
  , distribuciones_monto_equitativo :: f (TableEntity DistribucionMontoEquitativoT)
  , distribuciones_monto_equitativo_items :: f (TableEntity DistribucionMontoEquitativoItemT)
  , distribuciones_montos_especificos :: f (TableEntity DistribucionMontosEspecificosT)
  , distribuciones_montos_especificos_items :: f (TableEntity DistribucionMontosEspecificosItemT)
  , _bananasplitRepartijas :: f (TableEntity DistribucionRepartijaT)
  , _bananasplitRepartijaItems :: f (TableEntity RepartijaItemT)
  , _bananasplitRepartijaClaims :: f (TableEntity RepartijaClaimT)
  } deriving (Generic, Database be)

db :: DatabaseSettings be BananaSplitDb
db = defaultDbSettings

data GrupoT f = Grupo
  { id :: Columnar f ULID
  , nombre :: Columnar f Text
  } deriving (Generic, Beamable )

type GrupoId = PrimaryKey GrupoT Identity
type Grupo = GrupoT Identity

deriving instance Show GrupoId
deriving instance Show Grupo
deriving instance Eq GrupoId
deriving instance Eq Grupo

instance Beam.Table GrupoT where
  data PrimaryKey GrupoT f = GrupoId (Columnar f ULID) deriving (Generic, Beamable)
  primaryKey = GrupoId . (.id)


data ParticipanteT f = Participante
  { participanteId :: Columnar f ULID
  , participanteGrupo :: PrimaryKey GrupoT f
  , participanteNombre :: Columnar f Text
  } deriving (Generic, Beamable)

type ParticipanteId = PrimaryKey ParticipanteT Identity
type Participante = ParticipanteT Identity

deriving instance Show ParticipanteId
deriving instance Eq ParticipanteId
deriving instance Show Participante
deriving instance Eq Participante

instance Table ParticipanteT where
  data PrimaryKey ParticipanteT f = ParticipanteId (Columnar f ULID) deriving (Generic, Beamable)
  primaryKey = ParticipanteId . (.participanteId)

data PagoT f = Pago
  { pagoId :: Columnar f ULID
  , pagoIsValid :: Columnar f Bool
  , pagoGrupo :: PrimaryKey GrupoT f
  , pagoNombre :: Columnar f Text
  , pagoMonto :: MontoT f
  , distribucion_pagadores :: PrimaryKey DistribucionT f
  , distribucion_deudores :: PrimaryKey DistribucionT f
  } deriving (Generic, Beamable)

type PagoId = PrimaryKey PagoT Identity
type Pago = PagoT Identity

deriving instance Show PagoId
deriving instance Eq PagoId
deriving instance Ord PagoId
deriving instance Show Pago

instance Table PagoT where
  newtype PrimaryKey PagoT f = PagoId (Columnar f ULID)
    deriving stock (Generic)
    deriving anyclass (Beamable)

  primaryKey = PagoId . (.pagoId)


data MontoT f = Monto
  { montoLugaresDespuesDeLaComa :: Columnar f Int32
  , montoValor :: Columnar f Int32
  } deriving (Generic, Beamable)

type Monto = MontoT Identity
deriving instance Show Monto
deriving instance Show (MontoT (Nullable Identity))
deriving instance Eq Monto

data DistribucionT f = Distribucion
  { id :: Columnar f ULID
  , tipo :: Columnar f Text
  }
  deriving (Generic, Beamable)

type Distribucion = DistribucionT Identity
deriving instance Show Distribucion
deriving instance Eq Distribucion
type DistributionId = PrimaryKey DistribucionT Identity
deriving instance Show DistributionId
deriving instance Eq DistributionId

instance Table DistribucionT where
  data PrimaryKey DistribucionT f =
    DistribucionId (Columnar f ULID)
    deriving (Generic, Beamable)
  primaryKey = DistribucionId . (.id)

data DistribucionMontosEspecificosT f = DistribucionMontosEspecificos
  { id :: Columnar f ULID
  , distribucion :: PrimaryKey DistribucionT f
  }
  deriving (Generic, Beamable)

type DistribucionMontosEspecifico = DistribucionMontosEspecificosT Identity
deriving instance Show DistribucionMontosEspecifico
deriving instance Eq DistribucionMontosEspecifico
type DistribucionMontosEspecificoId = PrimaryKey DistribucionMontosEspecificosT Identity
deriving instance Show DistribucionMontosEspecificoId
deriving instance Eq DistribucionMontosEspecificoId

instance Table DistribucionMontosEspecificosT where
  data PrimaryKey DistribucionMontosEspecificosT f =
    DistribucionMontosEspecificosId (Columnar f ULID)
    deriving (Generic, Beamable)
  primaryKey distrubucion = DistribucionMontosEspecificosId distrubucion.id

data DistribucionMontosEspecificosItemT f = DistribucionMontosEspecificosItem
  { id :: Columnar f ULID
  , distribucion :: PrimaryKey DistribucionMontosEspecificosT f
  , participante :: PrimaryKey ParticipanteT f
  , monto :: MontoT f
  }
  deriving (Generic, Beamable)

type DistribucionMontosEspecificosItem = DistribucionMontosEspecificosItemT Identity
deriving instance Show DistribucionMontosEspecificosItem
deriving instance Eq DistribucionMontosEspecificosItem
type DistribucionMontosEspecificosItemId = PrimaryKey DistribucionMontosEspecificosItemT Identity
deriving instance Show DistribucionMontosEspecificosItemId
deriving instance Eq DistribucionMontosEspecificosItemId

instance Table DistribucionMontosEspecificosItemT where
  data PrimaryKey DistribucionMontosEspecificosItemT f =
    DistribucionMontosEspecificosItemId (Columnar f ULID)
    deriving (Generic, Beamable)
  primaryKey distrubucion = DistribucionMontosEspecificosItemId distrubucion.id

data DistribucionMontoEquitativoT f = DistribucionMontoEquitativo
  { id :: Columnar f ULID
  , distribucion :: PrimaryKey DistribucionT f
  } deriving (Generic, Beamable)

type DistribucionMontoEquitativo = DistribucionMontoEquitativoT Identity
deriving instance Show DistribucionMontoEquitativo
deriving instance Eq DistribucionMontoEquitativo
type DistribucionMontoEquitativoId = PrimaryKey DistribucionMontoEquitativoT Identity
deriving instance Show DistribucionMontoEquitativoId
deriving instance Eq DistribucionMontoEquitativoId
instance Table DistribucionMontoEquitativoT where
  data PrimaryKey DistribucionMontoEquitativoT f =
    DistribucionMontoEquitativoId (Columnar f ULID)
    deriving (Generic, Beamable)
  primaryKey = DistribucionMontoEquitativoId . (.id)

-- ==============
-- DistribucionMontoEquitativoItem
-- ==============
data DistribucionMontoEquitativoItemT f = DistribucionMontoEquitativoItem
  { distribucion :: PrimaryKey DistribucionMontoEquitativoT f
  , participante :: PrimaryKey ParticipanteT f
  }
  deriving (Generic, Beamable)

type DistribucionMontoEquitativoItem = DistribucionMontoEquitativoItemT Identity
deriving instance Show DistribucionMontoEquitativoItem
deriving instance Eq DistribucionMontoEquitativoItem
type DistribucionMontoEquitativoItemId = PrimaryKey DistribucionMontoEquitativoItemT Identity
deriving instance Show DistribucionMontoEquitativoItemId
deriving instance Eq DistribucionMontoEquitativoItemId

instance Table DistribucionMontoEquitativoItemT where
  data PrimaryKey DistribucionMontoEquitativoItemT f =
    DistribucionMontoEquitativoItemId (PrimaryKey DistribucionMontoEquitativoT f)
                                      (PrimaryKey ParticipanteT f)
    deriving (Generic, Beamable)
  primaryKey = DistribucionMontoEquitativoItemId <$> (.distribucion) <*> (.participante)

data DistribucionRepartijaT f = Repartija
  { id :: Columnar f ULID
  , distribucion :: PrimaryKey DistribucionT f
  , extra :: MontoT f
  } deriving (Generic, Beamable)

type DistribucionRepartijaId = PrimaryKey DistribucionRepartijaT Identity
type DistribucionRepartija = DistribucionRepartijaT Identity

deriving instance Show DistribucionRepartijaId
deriving instance Show DistribucionRepartija

instance Table DistribucionRepartijaT where
  data PrimaryKey DistribucionRepartijaT f = DistribucionRepartijaId (C f ULID)
    deriving (Generic, Beamable)
  primaryKey = DistribucionRepartijaId . (.id)

data RepartijaItemT f = RepartijaItem
  { repartijaitemId :: Columnar f ULID
  , repartijaitemRepartija :: PrimaryKey DistribucionRepartijaT f
  , repartijaitemNombre :: C f Text
  , repartijaitemMonto :: MontoT f
  , repartijaitemCantidad :: C f Int32
  } deriving (Generic, Beamable)

type RepartijaItem = RepartijaItemT Identity
type RepartijaItemId = PrimaryKey RepartijaItemT Identity

deriving instance Show RepartijaItem
deriving instance Show RepartijaItemId

instance Table RepartijaItemT where
  data PrimaryKey RepartijaItemT f = RepartijaItemId (C f ULID) deriving (Generic, Beamable)
  primaryKey = RepartijaItemId . (.repartijaitemId)

data RepartijaClaimT f = RepartijaClaim
  { repartijaclaimId :: Columnar f ULID
  , repartijaclaimRepartijaItem :: PrimaryKey RepartijaItemT f
  , repartijaclaimParticipante :: PrimaryKey ParticipanteT f
  , repartijaclaimCantidad :: C (Nullable f) Int32
  } deriving (Generic, Beamable)

type RepartijaClaim = RepartijaClaimT Identity
type RepartijaClaimId = PrimaryKey RepartijaClaimT Identity
deriving instance Show RepartijaClaim
deriving instance Show RepartijaClaimId

instance Table RepartijaClaimT where
  data PrimaryKey RepartijaClaimT f = RepartijaClaimId (C f ULID) deriving (Generic, Beamable)
  primaryKey = RepartijaClaimId . (.repartijaclaimId)

createGrupo :: Text -> Pg M.Grupo
createGrupo nombre = do
  newId <- liftIO ULID.getULID
  runInsert $ insert db.grupos  $ insertValues [
    Grupo newId nombre
   ]
  pure $ M.Grupo
    { M.id = newId
    , M.nombre = nombre
    , M.pagos = []
    , M.participantes = []
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
      pure $ Just $ M.ShallowGrupo
        { M.id = grupo.id
        , M.nombre = grupo.nombre
        , M.participantes = participantes
        }

fetchPago :: ULID -> ULID -> Pg M.Pago
fetchPago grupoId pagoId = do
  (dbPago :: Pago) <- fromMaybe (error "Pago not found") <$> runSelectReturningOne (select $ do
    pago <- all_ db.pagos
    guard_ (pago.pagoId ==. val_ pagoId)
    pure pago)

  (pagadores :: M.Distribucion) <- fromMaybe (error "Pagadores not found") <$> fetchDistribucion (case dbPago.distribucion_pagadores of DistribucionId ulid -> ulid)
  (deudores :: M.Distribucion) <- fromMaybe (error "deudores not found") <$> fetchDistribucion (case dbPago.distribucion_deudores of DistribucionId ulid -> ulid)
  dbPago
    & (\p ->
        M.Pago
        { M.pagoId = p.pagoId
        , M.monto = constructMonto p.pagoMonto
        , M.nombre = p.pagoNombre
        , M.isValid = p.pagoIsValid
        , M.pagadores = pagadores
        , M.deudores = deudores
        } & M.addIsValidPago)
    & pure

-- monto2dbMaybe :: Maybe M.Monto -> MontoT (Nullable Identity)
-- monto2dbMaybe (Just (Monto n d)) = Monto (Just n) (Just d)
-- monto2dbMaybe Nothing = Monto Nothing Nothing

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
  pure $ items & fmap (\item -> M.MontoEspecifico
    { M.id = item.id
    , M.participante = M.ParticipanteId $ case item.participante of ParticipanteId ulid -> ulid
    , M.monto = constructMonto item.monto
    })

fetchDistribucion :: ULID -> Pg (Maybe M.Distribucion)
fetchDistribucion distribucionId = do
  dbDistribucion :: Distribucion <- fmap (fromMaybe (error "Pago not found")) $ runSelectReturningOne $ select $ do
    distribucion <- all_ db._bananasplitDistribuciones
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
          pure $ Just $ M.TipoDistribucionMontoEquitativo $ M.DistribucionMontoEquitativo
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
          pure $ Just $ M.TipoDistribucionMontosEspecificos $ M.DistribucionMontosEspecificos
            { M.id = dbDistrib.id
            , M.montos = montos
            }
    "Repartija" -> do
      repartijaId <- fmap (fromMaybe (error "Repartija not found")) $ runSelectReturningOne $ select $ do
        r <- all_ db._bananasplitRepartijas
        guard_ (r.distribucion ==. DistribucionId (val_ dbDistribucion.id))
        pure r.id
      repartija <- fetchRepartija repartijaId
      pure $ Just $ M.TipoDistribucionRepartija repartija
    _ -> pure Nothing

  case tipo of
    (Just tipoData) ->
      pure $ Just $ M.Distribucion
        { M.id = dbDistribucion.id
        , M.tipo = tipoData
        }
    _ -> pure Nothing


fetchShallowPagos :: ULID -> Pg [M.ShallowPago]
fetchShallowPagos grupoId = do
  dbPagos <- runSelectReturningList $ select $ do
    pago <- all_ db.pagos
      & orderBy_ (asc_ . (.pagoId))
    guard_ (pago.pagoGrupo ==. GrupoId (val_ grupoId))
    pure pago

  dbPagos
    <&> (\pago ->
        M.ShallowPago
        { M.pagoId = pago.pagoId
        , M.isValid = pago.pagoIsValid
        , M.nombre = pago.pagoNombre
        , M.monto = constructMonto pago.pagoMonto
        })
    & pure

fetchParticipantes :: ULID -> Pg [M.Participante]
fetchParticipantes grupoId = do
  participantes <- runSelectReturningList $ select $ do
    p <- all_ db.participantes
      & orderBy_ (asc_ . (.participanteId))
    guard_ (p.participanteGrupo ==. GrupoId (val_ grupoId))
    pure p
  pure $ fmap (\p -> M.Participante
    { M.participanteNombre = p.participanteNombre
    , M.participanteId = p.participanteId
    }) participantes

addParticipante :: ULID -> Text -> Pg (Either Text M.Participante)
addParticipante grupoId name = do
  newId <- liftIO ULID.getULID
  runInsert $ insert db.participantes $ insertValues
    [ Participante newId (GrupoId grupoId) name
    ]
  pure $ Right $ M.Participante
    { M.participanteId = newId
    , M.participanteNombre = name
    }

deleteShallowParticipante :: ULID -> ULID -> Pg M.ParticipanteId
deleteShallowParticipante _grupoId pId = do
  runDelete $ delete db.participantes
    (\p -> p.participanteId  ==. val_ pId)
  pure $ M.ParticipanteId pId

participanteId2Persistent :: M.ParticipanteId -> ParticipanteId
participanteId2Persistent (M.ParticipanteId p)= ParticipanteId p

savePago :: ULID -> M.Pago -> Pg M.Pago
savePago grupoId pagoWithoutId = do
  pagoId <- if pagoWithoutId.pagoId == nullUlid
    then liftIO ULID.getULID
    else pure pagoWithoutId.pagoId
  let pago = (pagoWithoutId {M.pagoId = pagoId} :: M.Pago) & M.addIsValidPago
  distribucionPagadores <- saveDistribucion pago.pagadores
  distribucionDeudores <- saveDistribucion pago.deudores
  runInsert $
    insertOnConflict db.pagos
      (insertValues
        [ Pago
            { pagoId = pago.pagoId
            , pagoIsValid = pago.isValid
            , pagoGrupo = GrupoId grupoId
            , pagoNombre = pago.nombre
            , pagoMonto = deconstructMonto pago.monto
            , distribucion_pagadores = DistribucionId distribucionPagadores.id
            , distribucion_deudores = DistribucionId distribucionDeudores.id
            }
        ])
      (conflictingFields (\p -> p.pagoId))
      onConflictUpdateAll

  pure pago {M.pagadores = distribucionPagadores, M.deudores = distribucionDeudores}

saveDistribucion :: M.Distribucion -> Pg M.Distribucion
saveDistribucion distribucionWithoutId = do
  distribucionId <- if distribucionWithoutId.id == nullUlid
    then liftIO ULID.getULID
    else pure distribucionWithoutId.id
  case distribucionWithoutId.tipo of
    M.TipoDistribucionMontoEquitativo tipoWithoutId -> do
      let distribucion = distribucionWithoutId { M.id = distribucionId } :: M.Distribucion

      runInsert $
        insertOnConflict db._bananasplitDistribuciones
          (insertValues
            [ Distribucion distribucion.id "DistribucionMontoEquitativo"
            ])
          (conflictingFields (\d -> d.id))
          onConflictUpdateAll
      tipoId <- if tipoWithoutId.id == nullUlid
        then liftIO ULID.getULID
        else pure tipoWithoutId.id
      let tipo = tipoWithoutId { M.id = tipoId } :: M.DistribucionMontoEquitativo

      runInsert $
        insertOnConflict db.distribuciones_monto_equitativo
          (insertValues
            [ DistribucionMontoEquitativo tipoId (DistribucionId distribucion.id)
            ])
          (conflictingFields (\dme -> (dme.id, dme.distribucion)))
          onConflictUpdateAll

      runDelete $ delete db.distribuciones_monto_equitativo_items
        (\item ->
          item.distribucion ==. DistribucionMontoEquitativoId (val_ tipo.id)
          &&. not_ (item.participante `in_` [ ParticipanteId (val_ $ M.participanteId2ULID p) | p <- tipo.participantes ]))
      runInsert $
        insertOnConflict db.distribuciones_monto_equitativo_items
          (insertValues
            [ DistribucionMontoEquitativoItem (DistribucionMontoEquitativoId tipo.id) (participanteId2Persistent p) | p <- tipo.participantes ])
          (conflictingFields (\item -> (item.distribucion, item.participante)))
          onConflictUpdateAll
      pure $ distribucion { M.tipo = M.TipoDistribucionMontoEquitativo tipo }

    M.TipoDistribucionMontosEspecificos tipoWithoutId -> do
      let distribucion = distribucionWithoutId { M.id = distribucionId } :: M.Distribucion
      runInsert $
        insertOnConflict db._bananasplitDistribuciones
          (insertValues
            [ Distribucion distribucion.id "DistribucionMontosEspecificos"
            ])
          (conflictingFields (\d -> d.id))
          onConflictUpdateAll

      montosEspecificos <- forM tipoWithoutId.montos $ \monto -> do
        montoId <- if monto.id == nullUlid
          then liftIO ULID.getULID
          else pure monto.id
        pure (monto { M.id = montoId } :: M.MontoEspecifico)

      tipoId <- if tipoWithoutId.id == nullUlid
        then liftIO ULID.getULID
        else pure tipoWithoutId.id

      let tipo = tipoWithoutId { M.id = tipoId, M.montos = montosEspecificos } :: M.DistribucionMontosEspecificos

      runInsert $
        insertOnConflict db.distribuciones_montos_especificos
          (insertValues
            [ DistribucionMontosEspecificos tipoId (DistribucionId distribucion.id)
            ])
          (conflictingFields (\dme -> (dme.id, dme.distribucion)))
          onConflictUpdateAll

      runDelete $ delete db.distribuciones_montos_especificos_items
        (\item ->
          item.distribucion ==. DistribucionMontosEspecificosId (val_ tipo.id)
          &&. not_ (item.id `in_` [ val_ m.id | m <- tipo.montos ]))

      runInsert $
        insertOnConflict db.distribuciones_montos_especificos_items
          (insertValues
            [ DistribucionMontosEspecificosItem m.id (DistribucionMontosEspecificosId tipo.id) (participanteId2Persistent m.participante) (deconstructMonto m.monto) | m <- tipo.montos ])
          (conflictingFields (\item -> item.id))
          onConflictUpdateAll
      pure $ distribucion { M.tipo = M.TipoDistribucionMontosEspecificos tipo }
    M.TipoDistribucionRepartija repartijaWithoutId -> do
      let distribucion = distribucionWithoutId { M.id = distribucionId } :: M.Distribucion
      runInsert $
        insertOnConflict db._bananasplitDistribuciones
          (insertValues
            [ Distribucion distribucion.id "Repartija"
            ])
          (conflictingFields (\d -> d.id))
          onConflictUpdateAll
      repartija <- saveRepartija distribucion.id repartijaWithoutId
      pure $ distribucion { M.tipo = M.TipoDistribucionRepartija repartija }

-- savePagadores :: M.Pago -> Pg ()
-- savePagadores pago =
--   runInsert . insert db._bananasplitPagadores . insertValues
--     =<< partes2db (PagoId pago.id) pago.pagadores

-- saveDeudores :: M.Pago -> Pg ()
-- saveDeudores pago =
--   runInsert . insert db._bananasplitDeudores . insertValues
--     =<< partes2db (PagoId $ M.pagoId pago) (M.deudores pago)

-- partes2db :: MonadIO m => PagoId -> [M.Parte] -> m [Parte]
-- partes2db pagoId partes =
--   forM partes $ \case
--     M.Ponderado cuota (M.ParticipanteId participanteId) -> do
--       _newId <- liftIO ULID.getULID
--       pure Parte
--         -- { parteId = newId
--         { partePago = pagoId
--         , parteParticipante = ParticipanteId participanteId
--         , parteTipo = "Ponderado"
--         , parteMonto = Monto Nothing Nothing
--         , parteCuota = Just $ fromInteger cuota
--         }
--     M.MontoFijo monto (M.ParticipanteId participanteId) -> do
--       _newId <- liftIO ULID.getULID
--       pure Parte
--         -- { parteId = newId
--         { partePago = pagoId
--         , parteParticipante = ParticipanteId participanteId
--         , parteTipo = "MontoFijo"
--         , parteMonto = xd $ deconstructMonto monto
--         , parteCuota = Nothing
--         }
--         where
--           xd :: Monto -> MontoT (Nullable Identity)
--           xd (Monto n d) = Monto (Just n) (Just d)

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

      runDelete $ delete db.pagos
        (\p -> p.pagoId ==. val_ unId)
      deleteDistribucion pagadoresId
      deleteDistribucion deudoresId

deleteDistribucion :: ULID -> Pg ()
deleteDistribucion distribucionId = do
  -- Most references to distribucion have on delete cascade so
  -- we don't need to delete them manually
  runDelete $ delete db._bananasplitDistribuciones
    (\d -> d.id ==. val_ distribucionId)

updatePago :: ULID -> ULID -> M.Pago -> Pg M.Pago
updatePago grupoId pagoId pago = do
  savePago grupoId pago

saveRepartija :: ULID -> M.Repartija -> Pg M.Repartija
saveRepartija distribucionId repartijaSinId = do
  repartijaId <- if repartijaSinId.id == nullUlid
    then liftIO ULID.getULID
    else pure repartijaSinId.id
  let repartija = repartijaSinId {M.id = repartijaId} :: M.Repartija
  runInsert $
    insertOnConflict db._bananasplitRepartijas
      (insertValues
        [ Repartija
            { id = repartija.id
            , distribucion = DistribucionId distribucionId
            , extra = deconstructMonto repartija.extra
            }
        ])
      (conflictingFields (\r -> r.id))
      onConflictUpdateAll
  items <- saveRepartijaItems repartijaId repartija.items
  pure repartija
    { M.items = items
    }

saveRepartijaItems :: ULID -> [M.RepartijaItem] -> Pg [M.RepartijaItem]
saveRepartijaItems repartijaId repartijaItemsWithoutId = do
  repartijaItems <- forM repartijaItemsWithoutId $ \repartijaItem -> do
    itemId <- if repartijaItem.id == nullUlid
      then liftIO ULID.getULID
      else pure repartijaItem.id
    pure (repartijaItem {M.id = itemId} :: M.RepartijaItem)
  runDelete $ delete db._bananasplitRepartijaItems
    (\item ->
      item.repartijaitemRepartija ==. DistribucionRepartijaId (val_ repartijaId)
      &&. not_ (item.repartijaitemId `in_` [ val_ i.id | i <- repartijaItems ]))

  runInsert $
    insertOnConflict db._bananasplitRepartijaItems
      (insertValues $
        fmap (\item -> RepartijaItem
          { repartijaitemId = item.id
          , repartijaitemRepartija = DistribucionRepartijaId repartijaId
          , repartijaitemNombre = item.nombre
          , repartijaitemMonto = deconstructMonto item.monto
          , repartijaitemCantidad = fromIntegral item.cantidad
          }) repartijaItems)
      (conflictingFields (\item -> item.repartijaitemId))
      onConflictUpdateAll
  pure repartijaItems

fetchRepartija :: ULID -> Pg M.Repartija
fetchRepartija unRepartijaId = do
  repartija :: DistribucionRepartija <- fmap (fromMaybe (error "Repartija not found"))$ runSelectReturningOne $ select $ do
    repartija <- all_ db._bananasplitRepartijas
    guard_ (repartija.id ==. val_ unRepartijaId)
    pure repartija
  items :: [RepartijaItem] <- runSelectReturningList $ select $ do
    item <- all_ db._bananasplitRepartijaItems
    guard_ $ item.repartijaitemRepartija ==. val_ (DistribucionRepartijaId repartija.id)
    pure item
  claims :: [RepartijaClaim] <- runSelectReturningList $ select $ do
    claim <- all_ db._bananasplitRepartijaClaims
    guard_ $ claim.repartijaclaimRepartijaItem `in_` fmap (val_ . RepartijaItemId . (.repartijaitemId)) items
    pure claim

  pure $ M.Repartija
    { id = repartija.id
    , extra = constructMonto repartija.extra
    , claims = claims
              & fmap (\r -> M.RepartijaClaim
                { M.id = r.repartijaclaimId
                , M.cantidad = fromIntegral <$> r.repartijaclaimCantidad
                , M.participante = M.ParticipanteId $ case r.repartijaclaimParticipante of ParticipanteId ulid -> ulid
                , M.itemId = case r.repartijaclaimRepartijaItem of RepartijaItemId ulid -> ulid
                })
    , items = items
              & fmap (\dbItem -> M.RepartijaItem
                { M.id = dbItem.repartijaitemId
                , M.nombre = dbItem.repartijaitemNombre
                , M.monto = constructMonto dbItem.repartijaitemMonto
                , M.cantidad = fromIntegral dbItem.repartijaitemCantidad
                }
                )
  }

saveRepartijaClaim :: ULID -> M.RepartijaClaim -> Pg M.RepartijaClaim
saveRepartijaClaim unRepartijaId repartijaClaim = do
  claimId <- if repartijaClaim.id == nullUlid
    then liftIO ULID.getULID
    else pure repartijaClaim.id
  let claim' = repartijaClaim { M.id = claimId } :: M.RepartijaClaim
  runInsert $
    insertOnConflict db._bananasplitRepartijaClaims
      (insertValues [claimToRow claim'])
      (conflictingFields (\c -> (c.repartijaclaimParticipante, c.repartijaclaimRepartijaItem)))
      onConflictUpdateAll
      -- (onConflictUpdateSet (\fields _oldValues ->
      --   repartijaClaimCantidad fields <-. val_ (fromIntegral <$> M.repartijaClaimCantidad claim')))
  pure claim'

deleteRepartijaClaim :: ULID -> Pg ()
deleteRepartijaClaim claimId = do
  runDelete $ delete db._bananasplitRepartijaClaims
    (\c -> c.repartijaclaimId  ==. val_ claimId)

claimToRow :: M.RepartijaClaim -> RepartijaClaim
claimToRow claim =
  RepartijaClaim
    { repartijaclaimId = claim.id
    , repartijaclaimParticipante = ParticipanteId $ M.participanteId2ULID claim.participante
    , repartijaclaimRepartijaItem = RepartijaItemId claim.itemId
    , repartijaclaimCantidad = fromIntegral <$> claim.cantidad
    }

instance HasSqlValueSyntax PgValueSyntax ULID where
  sqlValueSyntax :: ULID -> PgValueSyntax
  sqlValueSyntax ulid = sqlValueSyntax $ Text.pack $ show ulid
instance FromBackendRow Postgres ULID
instance FromField ULID where
  fromField f bs = do
    s <- fromField @Text f bs
    case readEither @ULID s of
      Left _ -> returnError ConversionFailed f $ "invalid ulid: " <> toS s
      Right ulid -> pure ulid

instance HasSqlEqualityCheck Postgres ULID
