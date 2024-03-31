{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
module BananaSplit.Persistence
    ( addParticipante
    , createGrupo
    , createTables
    , deletePago
    , fetchGrupo
    , pagoTable
    , parteDeudorTable
    , partePagadorTable
    , participanteTable
    , savePago
    , updatePago
    ) where

import BananaSplit qualified as M

import Data.Data (Proxy)
import Data.Either (fromRight)
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.Ratio qualified as Ratio
import Data.Text (pack, unpack)
import Data.ULID (ULID)
import Data.ULID qualified as ULID

import Database.Selda
import Database.Selda.SqlType

import Money qualified

import Text.Read (readMaybe)

data GrupoRecord = GrupoRecord
  { grupoId :: ULID
  , grupoNombre :: Text
  } deriving (Show, Eq, Generic)

instance SqlRow GrupoRecord

grupoTable :: Table GrupoRecord
grupoTable = table "grupos"
  [ #grupoId :- primary
  ]

data ParticipanteRecord = ParticipanteRecord
  { participanteId :: ULID
  , participanteGrupoId :: ULID
  , participanteNombre :: Text
  } deriving (Show, Eq, Generic)

instance SqlRow ParticipanteRecord

participanteTable :: Table ParticipanteRecord
participanteTable = table "participantes"
  [ #participanteId :- primary
  , #participanteGrupoId :- foreignKey grupoTable #grupoId
  ]

data PagoRecord = PagoRecord
  { pagoId :: ULID
  , pagoGrupoId :: ULID
  , pagoNombre :: Text
  , pagoMontoNumerador :: Int
  , pagoMontoDenominador :: Int
  } deriving (Show, Eq, Generic)

instance SqlRow PagoRecord

pagoTable :: Table PagoRecord
pagoTable = table "pagos"
  [ #pagoId :- primary
  , #pagoGrupoId :- foreignKey grupoTable #grupoId
  ]

data ParteRecord = ParteRecord
  { partePagoId :: ULID
  , parteParticipanteId :: ULID
  , parteTipo :: Text
  , parteMontoNumerador :: Maybe Int
  , parteMontoDenominador :: Maybe Int
  , parteCuota :: Maybe Int
  } deriving (Show, Eq, Generic)

instance SqlRow ParteRecord

parteDeudorTable :: Table ParteRecord
parteDeudorTable = table "partes_deudores"
  [ #partePagoId :- foreignKey pagoTable #pagoId
  , #parteParticipanteId :- foreignKey participanteTable #participanteId
  ]

partePagadorTable :: Table ParteRecord
partePagadorTable = table "partes_pagadores"
  [ #partePagoId :- foreignKey pagoTable #pagoId
  , #parteParticipanteId :- foreignKey participanteTable #participanteId
  ]

updatePago :: (MonadSelda m, MonadMask m) => ULID -> ULID -> M.Pago -> m ()
updatePago grupoId pagoId pago = do
  let realPago = pago { M.pagoId = pagoId}
  transaction $ do
    update_ pagoTable (\p -> p ! #pagoId .== literal pagoId) (\_p -> row $ pago2PagoRecord grupoId realPago)
    deletePartesFromPago pagoId
    savePartes pagoId pago

fetchGrupo :: MonadSelda m => ULID -> m (Maybe M.Grupo)
fetchGrupo ulid = do
  grupos :: [GrupoRecord] <- query $ do
    select grupoTable `suchThat` (\grupo -> grupo ! #grupoId .== literal ulid)
  case grupos of
    [GrupoRecord {grupoId, grupoNombre}] -> do
      participantes <- fetchParticipantes ulid
      pagos <- fetchPagos ulid
      pure $ Just $ M.Grupo
        { M.grupoId = grupoId
        , M.grupoNombre = grupoNombre
        , M.pagos = pagos
        , M.participantes = participantes
        }
    [] -> pure Nothing
    _ -> error $ "multiple grupos returned from db with id: " <> show ulid

fetchPagos :: MonadSelda m => ULID -> m [M.Pago]
fetchPagos grupoId = do
  pagosR :: [PagoRecord] <- query $ do
    pago <- select pagoTable
    restrict (pago ! #pagoGrupoId .== literal grupoId)
    order (pago ! #pagoId) descending
    pure pago
  let pagosIds = fmap pagoId pagosR
  partesDeudoresR <- query $ do
    parte <- select parteDeudorTable
    restrict (parte ! #partePagoId `isIn` (literal <$> pagosIds))
    order (parte ! #partePagoId) ascending
    pure parte
  partesPagadoresR <- query $ do
    parte <- select partePagadorTable
    restrict (parte ! #partePagoId `isIn` (literal <$> pagosIds))
    order (parte ! #partePagoId) ascending
    pure parte
  let deudoresMap = partesDeudoresR & fmap parteRecord2Parte & Map.fromListWith (++)
  let pagadoresMap = partesPagadoresR & fmap parteRecord2Parte & Map.fromListWith (++)
  pagosR
    & fmap (\pagoR ->
        let monto = M.Monto $ Money.dense' $ fromIntegral (pagoMontoNumerador pagoR) % fromIntegral (pagoMontoDenominador pagoR)
        in M.Pago
        { M.pagoId = pagoId pagoR
        , M.monto = monto
        , M.nombre = pagoNombre pagoR
        , M.deudores = Map.lookup (pagoId pagoR) deudoresMap & fromMaybe []
        , M.pagadores = Map.lookup (pagoId pagoR) pagadoresMap & fromMaybe []
        }
      )
    & pure
  where
    parteRecord2Parte :: ParteRecord -> (ULID, [M.Parte])
    parteRecord2Parte parteR =
      case parteTipo parteR of
        "Ponderado" ->
          case (parteCuota parteR) of
            (Just cuota) ->
              (partePagoId parteR, [M.Ponderado (fromIntegral cuota) (parteParticipanteId parteR)])
            _ -> undefined
        "MontoFijo" ->
          case (parteMontoDenominador parteR, parteMontoNumerador parteR) of
            (Just denominador, Just numerador) ->
              let monto = M.Monto $ Money.dense' $ fromIntegral numerador % fromIntegral denominador
              in (partePagoId parteR, [M.MontoFijo monto $ parteParticipanteId parteR])
            _ -> undefined
        _ -> undefined

parte2ParteRecord :: ULID -> M.Parte -> ParteRecord
parte2ParteRecord pagoId parte =
  case parte of
    M.Ponderado cuota participanteId ->
      ParteRecord
        { partePagoId = pagoId
        , parteParticipanteId = participanteId
        , parteTipo = "Ponderado"
        , parteMontoDenominador = Nothing
        , parteMontoNumerador = Nothing
        , parteCuota = Just $ fromInteger cuota
        }
    M.MontoFijo monto participanteId ->
      let (numerador, denominador) = deconstructMonto monto
      in ParteRecord
        { partePagoId = pagoId
        , parteParticipanteId = participanteId
        , parteTipo = "MontoFijo"
        , parteMontoDenominador = Just denominador
        , parteMontoNumerador = Just numerador
        , parteCuota = Nothing
        }
deletePartesFromPago :: MonadSelda m => ULID -> m ()
deletePartesFromPago pagoId = do
  deleteFrom_ parteDeudorTable (\p -> p ! #partePagoId .== literal pagoId)
  deleteFrom_ partePagadorTable (\p -> p ! #partePagoId .== literal pagoId)

deletePago :: MonadSelda m => ULID -> m ()
deletePago pagoId = do
  deletePartesFromPago pagoId
  deleteShallowPago pagoId

deleteShallowPago :: MonadSelda m => ULID -> m ()
deleteShallowPago pagoId =
    deleteFrom_ pagoTable (\p -> p ! #pagoId .== literal pagoId)

pago2PagoRecord :: ULID -> M.Pago -> PagoRecord
pago2PagoRecord grupoId pago =
  let (numerador, denominador) = deconstructMonto $ M.monto pago
  in PagoRecord
    { pagoId = M.pagoId pago
    , pagoGrupoId = grupoId
    , pagoNombre = M.nombre pago
    , pagoMontoDenominador = denominador
    , pagoMontoNumerador = numerador
    }

saveShallowPago :: MonadSelda m => ULID -> M.Pago -> m ()
saveShallowPago grupoId pago = do
  insert_ pagoTable
    [ pago2PagoRecord grupoId pago
    ]

savePartes :: MonadSelda m => ULID -> M.Pago -> m ()
savePartes pagoId pago = do
  insert_ parteDeudorTable (parte2ParteRecord pagoId <$> M.deudores pago)

  insert_ partePagadorTable (parte2ParteRecord pagoId <$> M.pagadores pago)

savePago :: (MonadSelda m) => ULID -> M.Pago -> m M.Pago
savePago grupoId pagoWihtoutId = do
  pagoId <- liftIO $ ULID.getULID
  let pago = pagoWihtoutId { M.pagoId = pagoId }
  saveShallowPago grupoId pago

  insert_ parteDeudorTable (parte2ParteRecord pagoId <$> M.deudores pago)

  insert_ partePagadorTable (parte2ParteRecord pagoId <$> M.pagadores pago)

  pure pago

deconstructMonto :: M.Monto -> (Int, Int)
deconstructMonto (M.Monto money) =
  money
  & Money.toSomeDense
  & Money.someDenseAmount
  & (\n ->
      ( fromInteger $ Ratio.numerator n
      , fromInteger $ Ratio.denominator n
      )
    )

fetchParticipantes :: MonadSelda m => ULID -> m [M.Participante]
fetchParticipantes ulid = do
  participantesR <- query $ do
    participante <- select participanteTable
    restrict (participante ! #participanteGrupoId .== literal ulid)
    order (participante ! #participanteId) ascending
    return participante
  pure $ fmap record2Participante participantesR
  where
    record2Participante :: ParticipanteRecord -> M.Participante
    record2Participante participante =
      M.Participante
      { M.participanteId = participante.participanteId
      , M.participanteNombre = participante.participanteNombre
      }

createGrupo :: MonadSelda m => Text -> m M.Grupo
createGrupo nombre = do
  ulid <- liftIO ULID.getULID
  insert_ grupoTable
    [ GrupoRecord { grupoId = ulid, grupoNombre = nombre }
    ]
  pure $ M.Grupo
    { M.grupoId = ulid
    , M.grupoNombre = nombre
    , M.pagos = []
    , M.participantes = []
    }

addParticipante :: MonadSelda m => ULID -> Text -> m (Either String M.Participante)
addParticipante grupoId nombre = do
  ulid <- liftIO ULID.getULID
  insert_ participanteTable
    [ ParticipanteRecord
      { participanteId = ulid
      , participanteNombre = nombre
      , participanteGrupoId = grupoId
      }
    ]
  pure $ Right $ M.Participante
    { M.participanteId = ulid
    , M.participanteNombre = nombre
    }

createTables :: MonadSelda m => m ()
createTables = do
  tryCreateTable grupoTable
  tryCreateTable participanteTable
  tryCreateTable pagoTable
  tryCreateTable parteDeudorTable
  tryCreateTable partePagadorTable

instance SqlType ULID where
  mkLit :: ULID -> Lit ULID
  mkLit ulid =
    LCustom TText $ LText $ pack $ show ulid
  sqlType :: Proxy ULID -> SqlTypeRep
  sqlType _ = TText
  fromSql :: SqlValue -> ULID
  fromSql (SqlString s) =
    case readMaybe @ULID $ unpack s of
      Just ulid -> ulid
      Nothing -> error $ "Failed reading ulid from: " <> show s
  fromSql v =
      error $ "Failed reading ulid: expected a string but got " <> show v

  defaultValue :: Lit ULID
  defaultValue = mkLit $ fromRight undefined $ ULID.ulidFromInteger 0
