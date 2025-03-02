{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module BananaSplit.Persistence
    ( addParticipante
    , createGrupo
    , deletePago
    , deleteRepartijaClaim
    , deleteShallowParticipante
    , fetchGrupo
    , fetchRepartija
    , fetchRepartijas
    , savePago
    , saveRepartija
    , saveRepartijaClaim
    , updatePago
    ) where

import BananaSplit qualified as M

import Control.Monad (forM)

import Data.Function ((&))
import Data.Int (Int32)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, maybeToList)
import Data.Ratio ((%))
import Data.Ratio qualified as Ratio
import Data.Text (Text)
import Data.Text qualified as Text
import Data.ULID (ULID)
import Data.ULID qualified as ULID

import Database.Beam as Beam
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.Beam.Postgres.Full hiding (insert)
import Database.Beam.Postgres.Syntax
import Database.PostgreSQL.Simple.FromField (FromField (..), returnError)

import Money qualified

import Text.Read (readEither)

data BananaSplitDb f = BananaSplitDb
  { _bananasplitGrupos :: f (TableEntity GrupoT)
  , _bananasplitParticipantes :: f (TableEntity ParticipanteT)
  , _bananasplitPagos :: f (TableEntity PagoT)
  , _bananasplitPagadores :: f (TableEntity ParteT)
  , _bananasplitDeudores :: f (TableEntity ParteT)
  , _bananasplitRepartijas :: f (TableEntity RepartijaT)
  , _bananasplitRepartijaItems :: f (TableEntity RepartijaItemT)
  , _bananasplitRepartijaClaims :: f (TableEntity RepartijaClaimT)
  } deriving (Generic, Database be)

db :: DatabaseSettings be BananaSplitDb
db = defaultDbSettings `withDbModification`
  dbModification
  { _bananasplitParticipantes =
      modifyTableFields
        tableModification {
          participanteGrupo = GrupoId $ fieldNamed "grupo_id"
        }
  , _bananasplitPagos =
      modifyTableFields
        tableModification
          { pagoGrupo = GrupoId $ fieldNamed "grupo_id"
          , pagoMonto = Monto
            { montoDenominador = fieldNamed "monto_denominador"
            , montoNumerador = fieldNamed "monto_numerador"
            }
          }
  , _bananasplitPagadores =
      setEntityName "partes_pagadores" <> parteModifications
  , _bananasplitDeudores =
      setEntityName "partes_deudores" <> parteModifications
  , _bananasplitRepartijas =
      modifyTableFields
        tableModification
        { repartijaGrupo = GrupoId $ fieldNamed "grupo_id"
        , repartijaExtra = Monto
            { montoDenominador = fieldNamed "extra_denominador"
            , montoNumerador = fieldNamed "extra_numerador"
            }
        }
  , _bananasplitRepartijaItems =
      setEntityName "repartijas_items" <> modifyTableFields
        tableModification
        { repartijaItemRepartija = RepartijaId $ fieldNamed "repartija_id"
        , repartijaItemId = fieldNamed "id"
        , repartijaItemNombre = fieldNamed "nombre"
        , repartijaItemCantidad = fieldNamed "cantidad"
        , repartijaItemMonto = Monto
            { montoDenominador = fieldNamed "monto_denominador"
            , montoNumerador = fieldNamed "monto_numerador"
            }
        }
  , _bananasplitRepartijaClaims =
      setEntityName "repartijas_claims" <> modifyTableFields
        tableModification
        { repartijaClaimRepartijaItem = RepartijaItemId $ fieldNamed "repartija_item_id"
        , repartijaClaimParticipante = ParticipanteId $ fieldNamed "participante_id"
        , repartijaClaimId = fieldNamed "id"
        , repartijaClaimCantidad = fieldNamed "cantidad"
        }
  }
  where
    parteModifications =
      modifyTableFields
        tableModification
        { parteParticipante = ParticipanteId $ fieldNamed "participante_id"
        , partePago = PagoId $ fieldNamed "pago_id"
        , parteMonto = Monto
            { montoDenominador = fieldNamed "monto_denominador"
            , montoNumerador = fieldNamed "monto_numerador"
            }
        }

data GrupoT f = Grupo
  { grupoId :: Columnar f ULID
  , grupoNombre :: Columnar f Text
  } deriving (Generic, Beamable )

type GrupoId = PrimaryKey GrupoT Identity
type Grupo = GrupoT Identity

deriving instance Show GrupoId
deriving instance Show Grupo

instance Beam.Table GrupoT where
  data PrimaryKey GrupoT f = GrupoId (Columnar f ULID) deriving (Generic, Beamable)
  primaryKey = GrupoId . grupoId


data ParticipanteT f = Participante
  { participanteId :: Columnar f ULID
  , participanteGrupo :: PrimaryKey GrupoT f
  , participanteNombre :: Columnar f Text
  } deriving (Generic, Beamable)

type ParticipanteId = PrimaryKey ParticipanteT Identity
type Participante = ParticipanteT Identity

deriving instance Show ParticipanteId
deriving instance Show Participante

instance Table ParticipanteT where
  data PrimaryKey ParticipanteT f = ParticipanteId (Columnar f ULID) deriving (Generic, Beamable)
  primaryKey = ParticipanteId . participanteId


data PagoT f = Pago
  { pagoId :: Columnar f ULID
  , pagoGrupo :: PrimaryKey GrupoT f
  , pagoNombre :: Columnar f Text
  , pagoMonto :: MontoT f
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

  primaryKey = PagoId . pagoId


data MontoT f = Monto
  { montoNumerador :: Columnar f Int32
  , montoDenominador :: Columnar f Int32
  } deriving (Generic, Beamable)

type Monto = MontoT Identity
deriving instance Show Monto
deriving instance Show (MontoT (Nullable Identity))
deriving instance Eq Monto


data ParteT f = Parte
  -- TODO(ludat) agregarle id a esta tabla
  -- en realidad tiene en el schema porque
  -- haskell no tiene idea pero estaria bueno
  -- que este de este lado
  -- { parteId :: Columnar f ULID
  { partePago :: PrimaryKey PagoT f
  , parteParticipante :: PrimaryKey ParticipanteT f
  , parteTipo :: Columnar f Text
  , parteMonto :: MontoT (Nullable f)
  , parteCuota :: Columnar (Nullable f) Int32
  } deriving (Generic, Beamable)

type ParteId = PrimaryKey ParteT Identity
type Parte = ParteT Identity

deriving instance Show ParteId
deriving instance Show Parte
deriving instance Eq (PrimaryKey ParteT Identity)
deriving instance Ord (PrimaryKey ParteT Identity)

instance Table ParteT where
  data PrimaryKey ParteT f = NoId
    deriving (Generic, Beamable)
  primaryKey _ = NoId

data RepartijaT f = Repartija
  { repartijaId :: Columnar f ULID
  , repartijaGrupo :: PrimaryKey GrupoT f
  , repartijaNombre :: Columnar f Text
  , repartijaExtra :: MontoT f
  } deriving (Generic, Beamable)

type RepartijaId = PrimaryKey RepartijaT Identity
type Repartija = RepartijaT Identity

deriving instance Show RepartijaId
deriving instance Show Repartija

instance Table RepartijaT where
  data PrimaryKey RepartijaT f = RepartijaId (C f ULID) deriving (Generic, Beamable)
  primaryKey = RepartijaId . repartijaId

data RepartijaItemT f = RepartijaItem
  { repartijaItemId :: Columnar f ULID
  , repartijaItemRepartija :: PrimaryKey RepartijaT f
  , repartijaItemNombre :: C f Text
  , repartijaItemMonto :: MontoT f
  , repartijaItemCantidad :: C f Int32
  } deriving (Generic, Beamable)

type RepartijaItem = RepartijaItemT Identity
type RepartijaItemId = PrimaryKey RepartijaItemT Identity

deriving instance Show RepartijaItem
deriving instance Show RepartijaItemId

instance Table RepartijaItemT where
  data PrimaryKey RepartijaItemT f = RepartijaItemId (C f ULID) deriving (Generic, Beamable)
  primaryKey = RepartijaItemId . repartijaItemId

data RepartijaClaimT f = RepartijaClaim
  { repartijaClaimId :: Columnar f ULID
  , repartijaClaimRepartijaItem :: PrimaryKey RepartijaItemT f
  , repartijaClaimParticipante :: PrimaryKey ParticipanteT f
  , repartijaClaimCantidad :: C (Nullable f) Int32
  } deriving (Generic, Beamable)

type RepartijaClaim = RepartijaClaimT Identity
type RepartijaClaimId = PrimaryKey RepartijaClaimT Identity
deriving instance Show RepartijaClaim
deriving instance Show RepartijaClaimId

instance Table RepartijaClaimT where
  data PrimaryKey RepartijaClaimT f = RepartijaClaimId (C f ULID) deriving (Generic, Beamable)
  primaryKey = RepartijaClaimId . repartijaClaimId

createGrupo :: Text -> Pg M.Grupo
createGrupo nombre = do
  newId <- liftIO ULID.getULID
  runInsert $ insert (_bananasplitGrupos db) $ insertValues [
    Grupo newId nombre
    ]
  pure $ M.Grupo
    { M.grupoId = newId
    , M.grupoNombre = nombre
    , M.pagos = []
    , M.participantes = []
    }

    -- participantes <- oneToMany_ (_bananasplitParticipantes db) participanteGrupo g
    -- participantes <- leftJoin_ (all_ (_bananasplitParticipantes db))
    --   (\p -> participanteGrupo p ==. primaryKey g)
fetchGrupo :: ULID -> Pg (Maybe M.Grupo)
fetchGrupo aGrupoId = do
  maybeGrupo <- runSelectReturningOne $ select $ do
    g <- all_ (_bananasplitGrupos db)
    guard_ (grupoId g ==. val_ aGrupoId)
    pure g


  case maybeGrupo of
    Nothing -> pure Nothing
    Just grupo -> do
      participantes <- fetchParticipantes aGrupoId
      pagos <- fetchPagos aGrupoId
      pure $ Just $ M.Grupo
        { M.grupoId = grupoId grupo
        , M.grupoNombre = grupoNombre grupo
        , M.participantes = participantes
        , M.pagos = pagos
        }

fetchPagos :: ULID -> Pg [M.Pago]
fetchPagos aGrupoId = do
  dbPagos <- runSelectReturningList $ select $ do
    pago <- all_ (_bananasplitPagos db)
      & orderBy_ (asc_ . pagoId)
    guard_ (pagoGrupo pago ==. GrupoId (val_ aGrupoId))
    pure pago

  let pagosIds = dbPagos & fmap (val_ . PagoId . pagoId)

  partesPagadores <- runSelectReturningList $ select $ do
    partePagador <- all_ (_bananasplitPagadores db)
    guard_ (partePago partePagador `in_` pagosIds)
    pure partePagador

  partesDeudores <- runSelectReturningList $ select $ do
    partePagador <- all_ (_bananasplitDeudores db)
    guard_ (partePago partePagador `in_` pagosIds)
    pure partePagador

  let deudoresMap = partesDeudores & fmap dbParte2Parte & Map.fromListWith (++)
  let pagadoresMap = partesPagadores & fmap dbParte2Parte & Map.fromListWith (++)
  dbPagos
    & fmap (\pago ->
        M.Pago
        { M.pagoId = pagoId pago
        , M.monto = montoFromDb $ pagoMonto pago
        , M.nombre = pagoNombre pago
        , M.deudores = Map.lookup (PagoId $ pagoId pago) deudoresMap & fromMaybe []
        , M.pagadores = Map.lookup (PagoId $ pagoId pago) pagadoresMap & fromMaybe []
        }
      )
    & pure
  where
    dbParte2Parte :: Parte -> (PagoId, [M.Parte])
    dbParte2Parte parteR =
      case parteTipo parteR of
        "Ponderado" ->
          case parteCuota parteR of
            (Just cuota) ->
              (partePago parteR, [M.Ponderado (fromIntegral cuota) (M.ParticipanteId $ participanteId2ULID $ parteParticipante parteR)])
            _ -> undefined
        "MontoFijo" ->
          case parteMonto parteR of
            (Monto (Just numerador) (Just denominador)) ->
              let monto = montoFromDb $ Monto numerador denominador
              in (partePago parteR, [M.MontoFijo monto $ M.ParticipanteId $ participanteId2ULID $ parteParticipante parteR])
            _ -> error "parte monto es un para un monto fijo"
        _ -> error $ "parte tipo desconocida: " ++ show (parteTipo parteR)

montoFromDb :: Monto -> M.Monto
montoFromDb (Monto numerador denominador) =
  M.Monto $ Money.dense' $ fromIntegral numerador % fromIntegral denominador


participanteId2ULID :: ParticipanteId -> ULID
participanteId2ULID (ParticipanteId ulid) = ulid

fetchParticipantes :: ULID -> Pg [M.Participante]
fetchParticipantes grupoId = do
  participantes <- runSelectReturningList $ select $ do
    p <- all_ (_bananasplitParticipantes db)
      & orderBy_ (asc_ . participanteId)
    guard_ (participanteGrupo p ==. GrupoId (val_ grupoId))
    pure p
  pure $ fmap (\p -> M.Participante
    { M.participanteNombre = participanteNombre p
    , M.participanteId = participanteId p
    }) participantes

addParticipante :: ULID -> Text -> Pg (Either String M.Participante)
addParticipante grupoId name = do
  newId <- liftIO ULID.getULID
  runInsert $ insert (_bananasplitParticipantes db) $ insertValues
    [ Participante newId (GrupoId grupoId) name
    ]
  pure $ Right $ M.Participante
    { M.participanteId = newId
    , M.participanteNombre = name
    }

deleteShallowParticipante :: ULID -> ULID -> Pg M.ParticipanteId
deleteShallowParticipante _grupoId pId = do
  runDelete $ delete (_bananasplitParticipantes db)
    (\p -> participanteId p ==. val_ pId)
  pure $ M.ParticipanteId pId

savePago :: ULID -> M.Pago -> Pg M.Pago
savePago grupoId pagoWithoutId = do
  newId <- liftIO ULID.getULID
  let pago = pagoWithoutId {M.pagoId = newId}
  runInsert $ insert (_bananasplitPagos db) $ insertValues
    [ Pago (M.pagoId pago) (GrupoId grupoId) (M.nombre pago) (deconstructMonto $ M.monto pago)
    ]
  savePagadores pago
  saveDeudores pago

  pure pago { M.pagoId = newId }

savePagadores :: M.Pago -> Pg ()
savePagadores pago =
  runInsert . insert (_bananasplitPagadores db) . insertValues
    =<< partes2db (PagoId $ M.pagoId pago) (M.pagadores pago)

saveDeudores :: M.Pago -> Pg ()
saveDeudores pago =
  runInsert . insert (_bananasplitDeudores db) . insertValues
    =<< partes2db (PagoId $ M.pagoId pago) (M.deudores pago)

partes2db :: MonadIO m => PagoId -> [M.Parte] -> m [Parte]
partes2db pagoId partes =
  forM partes $ \case
    M.Ponderado cuota (M.ParticipanteId participanteId) -> do
      _newId <- liftIO ULID.getULID
      pure Parte
        -- { parteId = newId
        { partePago = pagoId
        , parteParticipante = ParticipanteId participanteId
        , parteTipo = "Ponderado"
        , parteMonto = Monto Nothing Nothing
        , parteCuota = Just $ fromInteger cuota
        }
    M.MontoFijo monto (M.ParticipanteId participanteId) -> do
      _newId <- liftIO ULID.getULID
      pure Parte
        -- { parteId = newId
        { partePago = pagoId
        , parteParticipante = ParticipanteId participanteId
        , parteTipo = "MontoFijo"
        , parteMonto = xd $ deconstructMonto monto
        , parteCuota = Nothing
        }
        where
          xd :: Monto -> MontoT (Nullable Identity)
          xd (Monto n d) = Monto (Just n) (Just d)

deconstructMonto :: M.Monto -> Monto
deconstructMonto (M.Monto monto) =
  monto
  & Money.toSomeDense
  & Money.someDenseAmount
  & (\n ->
      Monto
        (fromInteger $ Ratio.numerator n)
        (fromInteger $ Ratio.denominator n)
    )

deletePago :: ULID -> Pg ()
deletePago unId = do
  deleteDeudores unId
  deletePagadores unId
  runDelete $ delete (_bananasplitPagos db)
      (\p -> pagoId p ==. val_ unId)

deletePagadores :: ULID -> Pg ()
deletePagadores unId =
  runDelete $ delete (_bananasplitPagadores db)
      (\parte -> partePago parte ==. PagoId (val_ unId))

deleteDeudores :: ULID -> Pg ()
deleteDeudores unId =
  runDelete $ delete (_bananasplitDeudores db)
      (\parte -> partePago parte ==. PagoId (val_ unId))

updatePago :: ULID -> ULID -> M.Pago -> Pg M.Pago
updatePago grupoId unPagoId pagoWithoutId = do
  let pago = pagoWithoutId {M.pagoId = unPagoId}
  runUpdate $
    update (_bananasplitPagos db)
      (\p -> mconcat
        [ pagoNombre p <-. val_ (M.nombre pago)
        , pagoMonto p <-. val_ (deconstructMonto $ M.monto pago)
        ])
      (\p -> pagoId p ==. val_ (M.pagoId pago))
  deletePagadores (M.pagoId pago)
  deleteDeudores (M.pagoId pago)
  savePagadores pago
  saveDeudores pago
  pure pago

saveRepartija :: ULID -> M.Repartija -> Pg M.Repartija
saveRepartija unGrupoId repartijaSinId = do
  newId <- liftIO ULID.getULID
  let repartija = repartijaSinId {M.repartijaId = newId}
  runInsert $ insert (_bananasplitRepartijas db) $ insertValues
    [ Repartija
        (M.repartijaId repartija)
        (GrupoId unGrupoId)
        (M.repartijaNombre repartija)
        (deconstructMonto $ M.repartijaExtra repartija)
    ]
  items <- saveRepartijaItems newId $ M.repartijaItems repartija
  pure repartija
    { M.repartijaItems = items
    }

saveRepartijaItems :: ULID -> [M.RepartijaItem] -> Pg [M.RepartijaItem]
saveRepartijaItems repartijaId repartijaItemsWithoutId = do
  repartijaItems <- forM repartijaItemsWithoutId $ \repartijaItem -> do
    itemId <- liftIO ULID.getULID
    pure $ repartijaItem {M.repartijaItemId = itemId}
  runInsert $ insert (_bananasplitRepartijaItems db) $ insertValues $
    fmap (\item -> RepartijaItem
      { repartijaItemId = M.repartijaItemId item
      , repartijaItemRepartija = RepartijaId repartijaId
      , repartijaItemNombre = M.repartijaItemNombre item
      , repartijaItemMonto = deconstructMonto $ M.repartijaItemMonto item
      , repartijaItemCantidad = fromIntegral $ M.repartijaItemCantidad item
      }) repartijaItems
  pure repartijaItems

fetchRepartijas :: ULID -> Pg [M.ShallowRepartija]
fetchRepartijas unGrupoId = do
  repartijas <- runSelectReturningList $ select $ do
    repartija <- all_ (_bananasplitRepartijas db)
    guard_ (repartijaGrupo repartija ==. val_ (GrupoId unGrupoId))
    pure repartija
  pure $ repartijas
    & fmap (\repartija -> M.ShallowRepartija
      { M.repartijaShallowId = repartijaId repartija
      , M.repartijaShallowNombre = repartijaNombre repartija
      })


fetchRepartija :: ULID -> Pg M.Repartija
fetchRepartija unRepartijaId = do
  repartijasAndItemsAndClaims :: [(Repartija, Maybe RepartijaItem, Maybe RepartijaClaim)] <- runSelectReturningList $ select $ do
    repartija <- all_ (_bananasplitRepartijas db)
    guard_ (repartijaId repartija ==. val_ unRepartijaId)
    item <- leftJoin_ (all_ $ _bananasplitRepartijaItems db)
      (\item -> repartijaItemRepartija item `references_` repartija)
    -- item <- oneToManyOptional_ (_bananasplitRepartijaItems db) (just_ . repartijaItemId) repartija
    claim <- leftJoin_' (all_ $ _bananasplitRepartijaClaims db)
      (\claim -> just_ (repartijaClaimRepartijaItem claim) ==?. pk item)
    pure (repartija, item, claim)

  repartijasAndItemsAndClaims
    & fmap (\(r, i, c) -> (repartijaId r, (r, Map.fromList $ maybeToList $ fmap (\item -> (repartijaItemId item, item)) i, maybeToList c)))
    & Map.fromListWith (\(r1, items1, claims1) (_r2, items2, claims2) ->
        (r1, Map.union items1 items2, claims1 ++ claims2))
    & fmap (\(repartija, items, claims) -> M.Repartija
      { M.repartijaId = repartijaId repartija
      , M.repartijaExtra = montoFromDb $ repartijaExtra repartija
      , M.repartijaNombre = repartijaNombre repartija
      , M.repartijaGrupoId = case repartijaGrupo repartija of GrupoId ulid -> ulid
      , M.repartijaClaims = claims
          & fmap (\RepartijaClaim{..} -> M.RepartijaClaim
            { M.repartijaClaimId = repartijaClaimId
            , M.repartijaClaimCantidad = fromIntegral <$> repartijaClaimCantidad
            , M.repartijaClaimParticipante = M.ParticipanteId $ case repartijaClaimParticipante of ParticipanteId ulid -> ulid
            , M.repartijaClaimItemId = case repartijaClaimRepartijaItem of RepartijaItemId ulid -> ulid
            })
      , M.repartijaItems = items
          & Map.elems
          & fmap (\dbItem -> M.RepartijaItem
            { M.repartijaItemId = repartijaItemId dbItem
            , M.repartijaItemNombre = repartijaItemNombre dbItem
            , M.repartijaItemMonto = montoFromDb $ repartijaItemMonto dbItem
            , M.repartijaItemCantidad = fromIntegral $ repartijaItemCantidad dbItem
            }
            )
      })
    & Map.elems
    & head -- TODO HEAD!
    & pure

saveRepartijaClaim :: ULID -> M.RepartijaClaim -> Pg M.RepartijaClaim
saveRepartijaClaim unRepartijaId repartijaClaim = do
  claimId <- liftIO ULID.getULID
  let claim' = repartijaClaim { M.repartijaClaimId = claimId } -- Use provided ID
  runInsert $
    insertOnConflict (_bananasplitRepartijaClaims db)
      (insertValues [claimToRow claim'])
      (conflictingFields (\c -> (repartijaClaimParticipante c, repartijaClaimRepartijaItem c)))
      onConflictUpdateAll
      -- (onConflictUpdateSet (\fields _oldValues ->
      --   repartijaClaimCantidad fields <-. val_ (fromIntegral <$> M.repartijaClaimCantidad claim')))
  pure claim'

deleteRepartijaClaim :: ULID -> Pg ()
deleteRepartijaClaim claimId = do
  runDelete $ delete (_bananasplitRepartijaClaims db)
    (\c -> repartijaClaimId c ==. val_ claimId)

-- Helper function to convert RepartijaClaim to a row for insertion (without ID)
claimToRow :: M.RepartijaClaim -> RepartijaClaim
claimToRow claim =
  RepartijaClaim
    { repartijaClaimId = M.repartijaClaimId claim
    , repartijaClaimParticipante = ParticipanteId $ M.participanteId2ULID $ M.repartijaClaimParticipante claim
    , repartijaClaimRepartijaItem = RepartijaItemId $ M.repartijaClaimItemId claim
    , repartijaClaimCantidad = fromIntegral <$> M.repartijaClaimCantidad claim
    }

instance HasSqlValueSyntax PgValueSyntax ULID where
  sqlValueSyntax :: ULID -> PgValueSyntax
  sqlValueSyntax ulid = sqlValueSyntax $ Text.pack $ show ulid
instance FromBackendRow Postgres ULID
instance FromField ULID where
  fromField f bs = do
    s <- fromField @String f bs
    case readEither @ULID s of
      Left _ -> returnError ConversionFailed f $ "invalid ulid: " ++ s
      Right ulid -> pure ulid

instance HasSqlEqualityCheck Postgres ULID
