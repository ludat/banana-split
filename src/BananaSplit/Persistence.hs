{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}
module BananaSplit.Persistence
    ( addParticipante
    , createGrupo
    , createTables
    , deletePago
    , deleteShallowParticipante
    , fetchGrupo
    , fetchParticipantes
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
import qualified Data.Text as Text
import Data.String.Interpolate (i)

data GrupoRecord = GrupoRecord
  { grupo_id :: ULID
  , grupo_nombre :: Text
  } deriving (Show, Eq, Generic)

instance SqlRow GrupoRecord

unprefixAndLower :: Text -> Text -> Text
unprefixAndLower prefix t =
  t
  & Text.stripPrefix prefix
  & fromMaybe (error [i|Failed to unprefix table name: '#{prefix}' is not a prefix of table name '#{t}'|])
  & Text.toLower

grupoTable :: Table GrupoRecord
grupoTable = tableFieldMod "grupos"
  [ #grupo_id :- primary
  ]
  (unprefixAndLower "grupo_")

data ParticipanteRecord = ParticipanteRecord
  { participante_id :: ULID
  , participante_grupo_id :: ULID
  , participante_nombre :: Text
  } deriving (Show, Eq, Generic)

instance SqlRow ParticipanteRecord

participanteTable :: Table ParticipanteRecord
participanteTable = tableFieldMod "participantes"
  [ #participante_id :- primary
  , #participante_grupo_id :- foreignKey grupoTable #grupo_id
  ]
  (unprefixAndLower "participante_")

data PagoRecord = PagoRecord
  { pago_id :: ULID
  , pago_grupo_id :: ULID
  , pago_nombre :: Text
  , pago_monto_numerador :: Int
  , pago_monto_denominador :: Int
  } deriving (Show, Eq, Generic)

instance SqlRow PagoRecord

pagoTable :: Table PagoRecord
pagoTable = tableFieldMod "pagos"
  [ #pago_id :- primary
  , #pago_grupo_id :- foreignKey grupoTable #grupo_id
  ]
  (unprefixAndLower "pago_")

data ParteRecord = ParteRecord
  { parte_pago_id :: ULID
  , parte_participante_id :: ULID
  , parte_tipo :: Text
  , parte_monto_numerador :: Maybe Int
  , parte_monto_denominador :: Maybe Int
  , parte_cuota :: Maybe Int
  } deriving (Show, Eq, Generic)

instance SqlRow ParteRecord

parteDeudorTable :: Table ParteRecord
parteDeudorTable = tableFieldMod "partes_deudores"
  [ #parte_pago_id :- foreignKey pagoTable #pago_id
  , #parte_participante_id :- foreignKey participanteTable #participante_id
  ]
  (unprefixAndLower "parte_")

partePagadorTable :: Table ParteRecord
partePagadorTable = tableFieldMod "partes_pagadores"
  [ #parte_pago_id :- foreignKey pagoTable #pago_id
  , #parte_participante_id :- foreignKey participanteTable #participante_id
  ]
  (unprefixAndLower "parte_")

updatePago :: (MonadSelda m, MonadMask m) => ULID -> ULID -> M.Pago -> m M.Pago
updatePago grupoId pagoId pago = do
  let realPago = pago {M.pagoId = pagoId}
  transaction $ do
    update_ pagoTable (\p -> p ! #pago_id .== literal pagoId) (\_p -> row $ pago2PagoRecord grupoId realPago)
    deletePartesFromPago pagoId
    savePartes pagoId pago
  pure realPago

fetchGrupo :: MonadSelda m => ULID -> m (Maybe M.Grupo)
fetchGrupo ulid = do
  grupos :: [GrupoRecord] <- query $ do
    select grupoTable `suchThat` (\grupo -> grupo ! #grupo_id .== literal ulid)
  case grupos of
    [GrupoRecord {grupo_id, grupo_nombre}] -> do
      participantes <- fetchParticipantes ulid
      pagos <- fetchPagos ulid
      pure $ Just $ M.Grupo
        { M.grupoId = grupo_id
        , M.grupoNombre = grupo_nombre
        , M.pagos = pagos
        , M.participantes = participantes
        }
    [] -> pure Nothing
    _ -> error $ "multiple grupos returned from db with id: " <> show ulid

fetchPagos :: MonadSelda m => ULID -> m [M.Pago]
fetchPagos grupoId = do
  pagosR :: [PagoRecord] <- query $ do
    pago <- select pagoTable
    restrict (pago ! #pago_grupo_id .== literal grupoId)
    order (pago ! #pago_id) descending
    pure pago
  let pagosIds = fmap pago_id pagosR
  partesDeudoresR <- query $ do
    parte <- select parteDeudorTable
    restrict (parte ! #parte_pago_id `isIn` (literal <$> pagosIds))
    order (parte ! #parte_pago_id) ascending
    pure parte
  partesPagadoresR <- query $ do
    parte <- select partePagadorTable
    restrict (parte ! #parte_pago_id `isIn` (literal <$> pagosIds))
    order (parte ! #parte_pago_id) ascending
    pure parte
  let deudoresMap = partesDeudoresR & fmap parteRecord2Parte & Map.fromListWith (++)
  let pagadoresMap = partesPagadoresR & fmap parteRecord2Parte & Map.fromListWith (++)
  pagosR
    & fmap (\pagoR ->
        let monto = M.Monto $ Money.dense' $ fromIntegral (pago_monto_numerador pagoR) % fromIntegral (pago_monto_denominador pagoR)
        in M.Pago
        { M.pagoId = pago_id pagoR
        , M.monto = monto
        , M.nombre = pago_nombre pagoR
        , M.deudores = Map.lookup (pago_id pagoR) deudoresMap & fromMaybe []
        , M.pagadores = Map.lookup (pago_id pagoR) pagadoresMap & fromMaybe []
        }
      )
    & pure
  where
    parteRecord2Parte :: ParteRecord -> (ULID, [M.Parte])
    parteRecord2Parte parteR =
      case parte_tipo parteR of
        "Ponderado" ->
          case parte_cuota parteR of
            (Just cuota) ->
              (parte_pago_id parteR, [M.Ponderado (fromIntegral cuota) (M.ParticipanteId $ parte_participante_id parteR)])
            _ -> undefined
        "MontoFijo" ->
          case (parte_monto_denominador parteR, parte_monto_numerador parteR) of
            (Just denominador, Just numerador) ->
              let monto = M.Monto $ Money.dense' $ fromIntegral numerador % fromIntegral denominador
              in (parte_pago_id parteR, [M.MontoFijo monto $ M.ParticipanteId $ parte_participante_id parteR])
            _ -> undefined
        _ -> undefined

parte2ParteRecord :: ULID -> M.Parte -> ParteRecord
parte2ParteRecord pagoId parte =
  case parte of
    M.Ponderado cuota (M.ParticipanteId participanteId) ->
      ParteRecord
        { parte_pago_id = pagoId
        , parte_participante_id = participanteId
        , parte_tipo = "Ponderado"
        , parte_monto_denominador = Nothing
        , parte_monto_numerador = Nothing
        , parte_cuota = Just $ fromInteger cuota
        }
    M.MontoFijo monto (M.ParticipanteId participanteId) ->
      let (numerador, denominador) = deconstructMonto monto
      in ParteRecord
        { parte_pago_id = pagoId
        , parte_participante_id = participanteId
        , parte_tipo = "MontoFijo"
        , parte_monto_denominador = Just denominador
        , parte_monto_numerador = Just numerador
        , parte_cuota = Nothing
        }
deletePartesFromPago :: MonadSelda m => ULID -> m ()
deletePartesFromPago pagoId = do
  deleteFrom_ parteDeudorTable (\p -> p ! #parte_pago_id .== literal pagoId)
  deleteFrom_ partePagadorTable (\p -> p ! #parte_pago_id .== literal pagoId)

deletePago :: (MonadSelda m, MonadMask m) => ULID -> ULID -> m ()
deletePago grupoId pagoId = do
  transaction $ do
    deletePartesFromPago pagoId
    deleteShallowPago grupoId pagoId

-- TODO(ludat): check if the pago belongs to the grupo
deleteShallowPago :: MonadSelda m => ULID -> ULID -> m ()
deleteShallowPago grupoId pagoId =
  deleteFrom_ pagoTable (\p -> p ! #pago_id .== literal pagoId)

pago2PagoRecord :: ULID -> M.Pago -> PagoRecord
pago2PagoRecord grupoId pago =
  let (numerador, denominador) = deconstructMonto $ M.monto pago
  in PagoRecord
    { pago_id = M.pagoId pago
    , pago_grupo_id = grupoId
    , pago_nombre = M.nombre pago
    , pago_monto_denominador = denominador
    , pago_monto_numerador = numerador
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

savePago :: (MonadSelda m, MonadMask m) => ULID -> M.Pago -> m M.Pago
savePago grupoId pagoWihtoutId = do
  pagoId <- liftIO ULID.getULID
  let pago = pagoWihtoutId { M.pagoId = pagoId }
  transaction $ do
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
    restrict (participante ! #participante_grupo_id .== literal ulid)
    order (participante ! #participante_id) ascending
    return participante
  pure $ fmap record2Participante participantesR
  where
    record2Participante :: ParticipanteRecord -> M.Participante
    record2Participante participante =
      M.Participante
      { M.participanteId = participante.participante_id
      , M.participanteNombre = participante.participante_nombre
      }

createGrupo :: MonadSelda m => Text -> m M.Grupo
createGrupo nombre = do
  ulid <- liftIO ULID.getULID
  insert_ grupoTable
    [ GrupoRecord { grupo_id = ulid, grupo_nombre = nombre }
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
      { participante_id = ulid
      , participante_nombre = nombre
      , participante_grupo_id = grupoId
      }
    ]
  pure $ Right $ M.Participante
    { M.participanteId = ulid
    , M.participanteNombre = nombre
    }

deleteShallowParticipante :: MonadSelda m => ULID -> ULID -> m ()
deleteShallowParticipante grupoId participanteId =
    deleteFrom_ participanteTable (\p -> p ! #participante_id .== literal participanteId .&& p ! #participante_grupo_id .== literal grupoId)

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
