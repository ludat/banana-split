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
    , fetchRepartija
    , fetchRepartijas
    , savePago
    , saveRepartija
    , saveRepartijaClaim
    , updatePago
    ) where

import BananaSplit qualified as M

import Data.Decimal qualified as Decimal
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.ULID (ULID)
import Data.ULID qualified as ULID

import Database.Beam as Beam
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.Beam.Postgres.Full hiding (insert)
import Database.Beam.Postgres.Syntax
import Database.PostgreSQL.Simple.FromField (FromField (..), returnError)

import Protolude
import Protolude.Error
import Protolude.Partial as Partial

data BananaSplitDb f = BananaSplitDb
  { grupos :: f (TableEntity GrupoT)
  , participantes :: f (TableEntity ParticipanteT)
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
  { _bananasplitPagadores =
      setEntityName "partes_pagadores"
  , _bananasplitDeudores =
      setEntityName "partes_deudores"
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
  primaryKey = GrupoId . (.grupoId)


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
  primaryKey = ParticipanteId . (.participanteId)

data PagoT f = Pago
  { pagoId :: Columnar f ULID
  , pagoIsValid :: Columnar f Bool
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

  primaryKey = PagoId . (.pagoId)


data MontoT f = Monto
  { montoLugaresDespuesDeLaComa :: Columnar f Int32
  , montoValor :: Columnar f Int32
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
  primaryKey = RepartijaId . (.repartijaId)

data RepartijaItemT f = RepartijaItem
  { repartijaitemId :: Columnar f ULID
  , repartijaitemRepartija :: PrimaryKey RepartijaT f
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
    { M.grupoId = newId
    , M.grupoNombre = nombre
    , M.pagos = []
    , M.participantes = []
    }

    -- participantes <- oneToMany_ (participantes db) participanteGrupo g
    -- participantes <- leftJoin_ (all_ (participantes db))
    --   (\p -> participanteGrupo p ==. primaryKey g)
fetchGrupo :: ULID -> Pg (Maybe M.Grupo)
fetchGrupo aGrupoId = do
  maybeGrupo <- runSelectReturningOne $ select $ do
    g <- all_ db.grupos
    guard_ (g.grupoId ==. val_ aGrupoId)
    pure g


  case maybeGrupo of
    Nothing -> pure Nothing
    Just grupo -> do
      participantes <- fetchParticipantes aGrupoId
      pagos <- fetchPagos aGrupoId
      pure $ Just $ M.Grupo
        { M.grupoId = grupo.grupoId
        , M.grupoNombre = grupo.grupoNombre
        , M.participantes = participantes
        , M.pagos = pagos
        }

fetchPagos :: ULID -> Pg [M.Pago]
fetchPagos aGrupoId = do
  dbPagos <- runSelectReturningList $ select $ do
    pago <- all_ db._bananasplitPagos
      & orderBy_ (asc_ . (.pagoId))
    guard_ (pago.pagoGrupo ==. GrupoId (val_ aGrupoId))
    pure pago

  let pagosIds = dbPagos & fmap (val_ . PagoId . (.pagoId))

  partesPagadores <- runSelectReturningList $ select $ do
    partePagador <- all_ db._bananasplitPagadores
    guard_ (partePagador.partePago `in_` pagosIds)
    pure partePagador

  partesDeudores <- runSelectReturningList $ select $ do
    partePagador <- all_ db._bananasplitDeudores
    guard_ (partePagador.partePago `in_` pagosIds)
    pure partePagador

  let deudoresMap = partesDeudores & fmap dbParte2Parte & Map.fromListWith (++)
  let pagadoresMap = partesPagadores & fmap dbParte2Parte & Map.fromListWith (++)
  dbPagos
    & fmap (\pago ->
        M.Pago
        { M.pagoId = pago.pagoId
        , M.isValid = False
        , M.monto = montoFromDb pago.pagoMonto
        , M.nombre = pago.pagoNombre
        , M.deudores = Map.lookup (PagoId pago.pagoId) deudoresMap & fromMaybe []
        , M.pagadores = Map.lookup (PagoId pago.pagoId) pagadoresMap & fromMaybe []
        } & M.addIsValidPago
      )
    & pure
  where
    dbParte2Parte :: Parte -> (PagoId, [M.Parte])
    dbParte2Parte parteR =
      case parteR.parteTipo of
        "Ponderado" ->
          case parteR.parteCuota of
            (Just cuota) ->
              (parteR.partePago, [M.Ponderado (fromIntegral cuota) (M.ParticipanteId $ participanteId2ULID $ parteR.parteParticipante)])
            _ -> error $ "falta cuota para una parte ponderada: " <> show parteR.parteTipo
        "MontoFijo" ->
          case parteR.parteMonto of
            (Monto (Just numerador) (Just denominador)) ->
              let monto = montoFromDb $ Monto numerador denominador
              in (parteR.partePago, [M.MontoFijo monto $ M.ParticipanteId $ participanteId2ULID parteR.parteParticipante])
            _ -> error "falta monto para una parte ponderada"
        _ -> error $ "parte tipo desconocida: " <> show parteR.parteTipo

montoFromDb :: Monto -> M.Monto
montoFromDb (Monto lugaresDespuesDeLaComa valor) =
  M.Monto $ Decimal.Decimal (fromIntegral lugaresDespuesDeLaComa) (fromIntegral valor)

participanteId2ULID :: ParticipanteId -> ULID
participanteId2ULID (ParticipanteId ulid) = ulid

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

savePago :: ULID -> M.Pago -> Pg M.Pago
savePago grupoId pagoWithoutId = do
  newId <- liftIO ULID.getULID
  let pago = pagoWithoutId {M.pagoId = newId} & M.addIsValidPago
  runInsert $ insert db._bananasplitPagos $ insertValues
    [ Pago (M.pagoId pago) (M.isValid pago) (GrupoId grupoId) (M.nombre pago) (deconstructMonto $ M.monto pago)
    ]
  savePagadores pago
  saveDeudores pago

  pure pago

savePagadores :: M.Pago -> Pg ()
savePagadores pago =
  runInsert . insert db._bananasplitPagadores . insertValues
    =<< partes2db (PagoId $ M.pagoId pago) (M.pagadores pago)

saveDeudores :: M.Pago -> Pg ()
saveDeudores pago =
  runInsert . insert db._bananasplitDeudores . insertValues
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
deconstructMonto (M.Monto (Decimal.Decimal l v)) =
  Monto (fromIntegral l) (fromIntegral v)

deletePago :: ULID -> Pg ()
deletePago unId = do
  deleteDeudores unId
  deletePagadores unId
  runDelete $ delete db._bananasplitPagos
      (\p -> p.pagoId ==. val_ unId)

deletePagadores :: ULID -> Pg ()
deletePagadores unId =
  runDelete $ delete db._bananasplitPagadores
      (\parte -> parte.partePago ==. PagoId (val_ unId))

deleteDeudores :: ULID -> Pg ()
deleteDeudores unId =
  runDelete $ delete db._bananasplitDeudores
      (\parte -> parte.partePago ==. PagoId (val_ unId))

updatePago :: ULID -> ULID -> M.Pago -> Pg M.Pago
updatePago _grupoId unPagoId pagoWithoutId = do
  let pago = pagoWithoutId {M.pagoId = unPagoId}
  runUpdate $
    update db._bananasplitPagos
      (\p -> mconcat
        [ p.pagoNombre <-. val_ (M.nombre pago)
        , p.pagoMonto <-. val_ (deconstructMonto $ M.monto pago)
        ])
      (\p -> p.pagoId ==. val_ (M.pagoId pago))
  deletePagadores (M.pagoId pago)
  deleteDeudores (M.pagoId pago)
  savePagadores pago
  saveDeudores pago
  pure pago

saveRepartija :: ULID -> M.Repartija -> Pg M.Repartija
saveRepartija unGrupoId repartijaSinId = do
  newId <- liftIO ULID.getULID
  let repartija = repartijaSinId {M.repartijaId = newId}
  runInsert $ insert db._bananasplitRepartijas $ insertValues
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
  runInsert $ insert db._bananasplitRepartijaItems $ insertValues $
    fmap (\item -> RepartijaItem
      { repartijaitemId = M.repartijaItemId item
      , repartijaitemRepartija = RepartijaId repartijaId
      , repartijaitemNombre = M.repartijaItemNombre item
      , repartijaitemMonto = deconstructMonto $ M.repartijaItemMonto item
      , repartijaitemCantidad = fromIntegral $ M.repartijaItemCantidad item
      }) repartijaItems
  pure repartijaItems

fetchRepartijas :: ULID -> Pg [M.ShallowRepartija]
fetchRepartijas unGrupoId = do
  repartijas <- runSelectReturningList $ select $ do
    repartija <- all_ db._bananasplitRepartijas
    guard_ (repartija.repartijaGrupo ==. val_ (GrupoId unGrupoId))
    pure repartija
  pure $ repartijas
    & fmap (\repartija -> M.ShallowRepartija
      { M.repartijaShallowId = repartija.repartijaId
      , M.repartijaShallowNombre = repartija.repartijaNombre
      })


fetchRepartija :: ULID -> Pg M.Repartija
fetchRepartija unRepartijaId = do
  repartijasAndItemsAndClaims :: [(Repartija, Maybe RepartijaItem, Maybe RepartijaClaim)] <- runSelectReturningList $ select $ do
    repartija <- all_ db._bananasplitRepartijas
    guard_ (repartija.repartijaId ==. val_ unRepartijaId)
    item <- leftJoin_ (all_ db._bananasplitRepartijaItems)
      (\item -> item.repartijaitemRepartija  `references_` repartija)
    -- item <- oneToManyOptional_ (_bananasplitRepartijaItems db) (just_ . repartijaItemId) repartija
    claim <- leftJoin_' (all_ db._bananasplitRepartijaClaims)
      (\claim -> just_ claim.repartijaclaimRepartijaItem ==?. pk item)
    pure (repartija, item, claim)

  repartijasAndItemsAndClaims
    & fmap (\(r, i, c) -> (r.repartijaId, (r, Map.fromList $ maybeToList $ fmap (\item -> (item.repartijaitemId , item)) i, maybeToList c)))
    & Map.fromListWith (\(r1, items1, claims1) (_r2, items2, claims2) ->
        (r1, Map.union items1 items2, claims1 ++ claims2))
    & fmap (\(repartija, items, claims) -> M.Repartija
      { M.repartijaId = repartija.repartijaId
      , M.repartijaExtra = montoFromDb repartija.repartijaExtra
      , M.repartijaNombre = repartija.repartijaNombre
      , M.repartijaGrupoId = case repartija.repartijaGrupo of GrupoId ulid -> ulid
      , M.repartijaClaims = claims
          & fmap (\r -> M.RepartijaClaim
            { M.repartijaClaimId = r.repartijaclaimId
            , M.repartijaClaimCantidad = fromIntegral <$> r.repartijaclaimCantidad
            , M.repartijaClaimParticipante = M.ParticipanteId $ case r.repartijaclaimParticipante of ParticipanteId ulid -> ulid
            , M.repartijaClaimItemId = case r.repartijaclaimRepartijaItem of RepartijaItemId ulid -> ulid
            })
      , M.repartijaItems = items
          & Map.elems
          & fmap (\dbItem -> M.RepartijaItem
            { M.repartijaItemId = dbItem.repartijaitemId
            , M.repartijaItemNombre = dbItem.repartijaitemNombre
            , M.repartijaItemMonto = montoFromDb dbItem.repartijaitemMonto
            , M.repartijaItemCantidad = fromIntegral dbItem.repartijaitemCantidad
            }
            )
      })
    & Map.elems
    & Partial.head
    -- & (?: (error "repartija no encontrada"))
    & pure

saveRepartijaClaim :: ULID -> M.RepartijaClaim -> Pg M.RepartijaClaim
saveRepartijaClaim unRepartijaId repartijaClaim = do
  claimId <- liftIO ULID.getULID
  let claim' = repartijaClaim { M.repartijaClaimId = claimId } -- Use provided ID
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

-- Helper function to convert RepartijaClaim to a row for insertion (without ID)
claimToRow :: M.RepartijaClaim -> RepartijaClaim
claimToRow claim =
  RepartijaClaim
    { repartijaclaimId = M.repartijaClaimId claim
    , repartijaclaimParticipante = ParticipanteId $ M.participanteId2ULID $ M.repartijaClaimParticipante claim
    , repartijaclaimRepartijaItem = RepartijaItemId $ M.repartijaClaimItemId claim
    , repartijaclaimCantidad = fromIntegral <$> M.repartijaClaimCantidad claim
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
