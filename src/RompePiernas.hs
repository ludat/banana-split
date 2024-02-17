{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RompePiernas
  ( Grupo(..)
  , Participante(..)
  , ParticipanteId
  , Pago(..)
  , Parte(..)
  , Transaccion(..)
  , Monto(..)
  , mkGrupo
  , agregarParticipante
  , agregarPago
  , calcularDeudasTotales
  , parteParticipante
  , calcularDeudasPago
  , deudasToPairs
  , resolverDeudasNaif
  , buscarParticipante
  , monto2Text
  , monto2Dense
  , nullUlid
  , text2Monto
  ) where

import Data.Text (Text, stripPrefix, pack, unpack)
import GHC.Records (HasField (..))
import Web.FormUrlEncoded
import GHC.Generics (Generic)
import Data.Maybe (mapMaybe)
import Data.Function
import Data.String.Interpolate
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.HashMap.Strict qualified as HashMap
import Servant
import Data.Either (partitionEithers)
import Data.List (sortOn, maximumBy)
import Data.Ord (Down(..))
import Lucid (ToHtml)
import Lucid.Base (ToHtml(..))
import Money qualified
import Data.ULID (ULID, getULID, ulidFromInteger)
import Text.Read (readEither, readMaybe)
import Control.Arrow (ArrowChoice(left))
import qualified Data.List as List
import qualified Data.Maybe as Maybe

-- data WithId a = WithId ULID a
--   deriving (Show, Eq)

data Grupo = Grupo
  { grupoId :: ULID
  , pagos :: [Pago]
  , participantes :: [Participante]
  } deriving (Show, Eq, Generic)

newtype Monto = Monto (Money.Dense "ARS")
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num)

monto2Text :: Monto -> Text
monto2Text (Monto m) = 
    (Money.denseToDecimal Money.defaultDecimalConf Money.Round m)

text2Monto :: Text -> Maybe Monto
text2Monto rawNumber = 
  Money.denseFromDecimal Money.defaultDecimalConf rawNumber
  & fmap Monto

monto2Dense :: Monto -> Money.Dense "ARS"
monto2Dense (Monto m) = 
    m

instance ToHtml Monto where
  toHtml m = 
    toHtml $ "$" <> monto2Text m

data Participante = Participante
  { participanteId :: ULID
  , participanteNombre :: Text
  }
  deriving (Show, Eq)

instance ToHttpApiData ULID where
  toQueryParam :: ULID -> Text
  toQueryParam ulid = pack . show $ ulid

instance FromHttpApiData ULID where
  parseUrlPiece :: Text -> Either Text ULID
  parseUrlPiece t = do
    case readMaybe @ULID $ unpack t of
      Just ulid -> pure ulid
      Nothing -> Left $ "cant parse a ulid from: " <> pack (show t) <> ""

type ParticipanteId = ULID

data Pago = Pago
  { pagoId :: ULID
  , monto :: Monto 
  , nombre :: Text
  , deudores :: [Parte]
  , pagadores :: [Parte]
  } deriving (Show, Eq, Generic)

data Parte
  = MontoFijo Monto ParticipanteId
  | Ponderado Integer ParticipanteId
  deriving (Show, Eq)

parteParticipante :: Parte -> ParticipanteId
parteParticipante (Ponderado _ p) = p
parteParticipante (MontoFijo _ p) = p

parsePartes :: Form -> Either Text [Parte]
parsePartes f = do
  deudoresKeys <- parseAll @Text "indices" f
  deudoresKeys
    & mapM (\k -> do
      let deudorForm = narrowForm (k <> ".") f 
      p <- parseUnique @String "participante" deudorForm
      participanteId <- readEither p
        & left (const $ "Failed reading participante id")
      t <- parseUnique @Text "tipo" deudorForm
      case t of
        "ponderado" -> do
          m <- parseUnique "monto" deudorForm
          pure $ Ponderado m participanteId
        "fijo" -> do
          m <- parseUnique "monto" deudorForm
          pure $ MontoFijo m participanteId
        _ -> Left [i|invalid tipo for parte: '#{t}'|]
      )

instance FromHttpApiData Monto where
  parseUrlPiece :: Text -> Either Text Monto
  parseUrlPiece t = do
    rawNumber <- parseUrlPiece @Text t
    case Money.denseFromDecimal Money.defaultDecimalConf rawNumber of
      Just n -> pure $ Monto n
      Nothing -> Left $ "monto invalido: " <> pack (show t)
  parseQueryParam :: Text -> Either Text Monto
  parseQueryParam t = do
    rawNumber <- parseQueryParam @Text t
    case Money.denseFromDecimal Money.defaultDecimalConf rawNumber of
      Just n -> pure $ Monto n
      Nothing -> Left $ "monto invalido: " <> pack (show t)

buscarParticipante :: Grupo -> ParticipanteId -> Participante
buscarParticipante grupo pId =
  grupo.participantes
  & List.find (\p -> participanteId p == pId )
  & Maybe.fromMaybe (error $ "participante " <> show pId <> "not found")

narrowForm :: Text -> Form -> Form
narrowForm prefix form =
  form
  & unForm
  & HashMap.toList
  & mapMaybe (\(k, v) ->
    case stripPrefix prefix k of
      Just unprefixed -> Just (unprefixed, v)
      Nothing -> Nothing
  )
  & HashMap.fromList
  & Form



instance FromForm Pago where
  fromForm f = do
    ulid <- parseUnique @String "id" f
      & (\case 
          Left e -> Left $ unpack e
          Right a -> Right a
        )
      & (>>= readEither @ULID)
      & (\case 
          Left _e -> ulidFromInteger 0
          Right a -> Right a
        )
    monto <- parseUnique "monto" f
    nombre <- parseUnique "nombre" f
    deudores <- parsePartes $ narrowForm "deudores." f
    pagadores <- parsePartes $ narrowForm "pagadores." f
    
    Right $
      Pago { pagoId = ulid, monto = monto, nombre = nombre, pagadores = pagadores, deudores = deudores }

-- >>> ulidFromInteger 0
-- Right 00000000000000000000000000

nullUlid :: ULID
nullUlid = read @ULID "00000000000000000000000000"

-- >>> read @ULID "00000000000000000000000000"
-- 00000000000000000000000000

mkGrupo :: Grupo
-- mkGrupo = cosa 
-- mkGrupo = cosa { _pagos = []}
mkGrupo = Grupo { pagos = [], participantes = []}

newtype Deudas a = Deudas
  (Map ParticipanteId a)
  deriving newtype (Show, Eq, Functor)

deudasToPairs :: Ord a => Deudas a -> [(ParticipanteId, a)]
deudasToPairs (Deudas deudasMap) = 
  deudasMap
  & Map.toAscList
  & sortOn (\(p, m) -> (Down m, p))

instance Num a => Monoid (Deudas a) where
  mempty = Deudas Map.empty

instance Num a => Semigroup (Deudas a) where
  Deudas d1 <> Deudas d2 = Deudas $ Map.unionWith (+) d1 d2

totalDeudas :: Num a => Deudas a -> a
totalDeudas (Deudas deudasMap) =
  deudasMap
  & Map.elems
  & sum

calcularDeudasTotales :: Grupo -> Deudas Monto
calcularDeudasTotales grupo =
  grupo.pagos
  & fmap calcularDeudasPago 
  & mconcat

mkDeuda :: ParticipanteId -> a -> Deudas a
mkDeuda participanteId monto =
  Deudas $ Map.singleton participanteId monto

calcularDeudasPago :: Pago -> Deudas Monto
calcularDeudasPago pago =
  calcularDeudas pago.monto pago.pagadores <> (fmap (* -1) $ calcularDeudas pago.monto pago.deudores)
  where
    calcularDeudas montoOriginal partes =
      let
        (ponderados, fijos) =
          partes
          & fmap (\case
            Ponderado parte participante -> Left (participante, parte)
            MontoFijo monto participante -> Right (participante, monto)
          )
          & partitionEithers 
        deudasFijos =
          fijos
          & fmap (uncurry mkDeuda)
          & mconcat
        totalFijo =
          totalDeudas deudasFijos
        deudasPonderados =
          ponderados
          & fmap (uncurry mkDeuda)
          & mconcat
      in deudasFijos <> distribuirEntrePonderados (montoOriginal - totalFijo) deudasPonderados

distribuirEntrePonderados :: Monto -> Deudas Integer -> Deudas Monto
distribuirEntrePonderados (Monto m) deudas =
  let
    parteTotal = totalDeudas deudas
  in deudas
      & fmap (\p -> Monto $ Money.dense' $ fromInteger p * toRational m / toRational parteTotal)

extraerMaximoDeudor :: Deudas Monto -> (ParticipanteId, Monto)
extraerMaximoDeudor (Deudas deudasMap) =
  deudasMap
  & Map.filter (< 0)
  & fmap (* -1)
  & Map.toList
  & maximumBy (compare `on` snd)
  
extraerMaximoPagador :: Deudas Monto -> (ParticipanteId, Monto)
extraerMaximoPagador (Deudas deudasMap) =
  deudasMap
  & Map.filter (> 0)
  & Map.toList
  & maximumBy (compare `on` snd)

removerDeudor :: ParticipanteId -> Deudas m -> Deudas m
removerDeudor participanteId (Deudas deudasMap) =
  Deudas $ Map.delete participanteId deudasMap

data Transaccion =
  Transaccion ParticipanteId ParticipanteId Monto
  deriving (Show, Eq)

resolverDeudasNaif :: Deudas Monto -> [Transaccion]
resolverDeudasNaif deudas
  | deudoresNoNulos deudas == 0 = []
  | deudoresNoNulos deudas == 1 = error $ show deudas
  | otherwise = 
      let
        (mayorDeudor, mayorDeuda) = extraerMaximoDeudor deudas
        deudas' = removerDeudor mayorDeudor deudas
        (mayorPagador, mayorPagado) = extraerMaximoPagador deudas'
        deudas'' = removerDeudor mayorPagador deudas'
      in case compare mayorDeuda mayorPagado of
          LT -> Transaccion mayorDeudor mayorPagador mayorDeuda
            : resolverDeudasNaif (deudas'' <> mkDeuda mayorPagador (mayorPagado - mayorDeuda))
          GT -> Transaccion mayorDeudor mayorPagador mayorPagado
            : resolverDeudasNaif (deudas'' <> mkDeuda mayorDeudor (-mayorDeuda + mayorPagado))
          EQ -> Transaccion mayorDeudor mayorPagador mayorPagado
            : resolverDeudasNaif (deudas'')

deudoresNoNulos :: Deudas Monto -> Int
deudoresNoNulos (Deudas deudasMap) = 
  deudasMap
  & Map.toList
  & filter (\(_p, m) -> m /= 0)
  & length

-- deuda :: Deudas Monto
-- deudaSimple = Deudas $ Map.fromList [("persona 1",Monto 10),("persona 2",Monto -10)]
-- deuda = Deudas $ Map.fromList [("persona 1",Monto 30),("persona 2",Monto -20), ("persona 3",Monto -10)]
-- deudaHdp = Deudas $ Map.fromList
--   [ ("alan", Monto  10)
--   , ("bill", Monto   3)
--   , ("chas", Monto   3)
--   , ("doug", Monto  -6)
--   , ("edie", Monto  -5)
--   , ("fred", Monto  -5)
--   ]

-- >>> resolverDeudasNaif $ deuda
-- ("persona 2",Monto (-10))

-- >>> resolverDeudasNaif $ deuda
-- [Transaccion "persona 2" "persona 1" (Monto 20),Transaccion "persona 3" "persona 1" (Monto 10)]

-- >>> resolverDeudasNaif $ deudaHdp
-- [Transaccion "doug" "alan" (Monto 6),Transaccion "fred" "alan" (Monto 4),Transaccion "edie" "chas" (Monto 3),Transaccion "edie" "bill" (Monto 2),Transaccion "fred" "bill" (Monto 1)]
agregarParticipante :: Text -> Grupo -> IO Grupo
agregarParticipante nombre grupo = do
  ulid <- getULID
  pure $ grupo { participantes = grupo.participantes ++ [Participante ulid nombre] }

agregarPago :: Pago -> Grupo -> Grupo
agregarPago pago grupo =
  grupo { pagos = grupo.pagos ++ [pago] }

-- instance HasField "participantes" Grupo [Participante] where
--   getField (Grupo { _participantes }) = _participantes

-- instance HasField "pagos" Grupo [Pago] where
--   getField (Grupo { _pagos }) = _pagos

-- instance (HasField symbol record return) => HasField symbol (WithId record) return where
--   getField (WithId _ulid a) = getField @symbol @record @return a 

-- instance HasField "unwrap" (WithId record) record where
--   getField (WithId _ulid a) = a 

-- instance HasField "unwraps" ([WithId record]) [record] where
--   getField = fmap (.unwrap)
