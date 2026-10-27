{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module BananaSplit.Participante (
  Participante (..),
  ParticipanteId (..),
  ParticipanteClaimError (..),
  ParticipanteError (..),
  ParticipanteUsage (..),
  participanteId2ULID,
  validarNombre,
) where

import Data.Aeson
import Data.Text qualified as Text
import Elm.Derive qualified as Elm
import Protolude

import BananaSplit.ULID (ULID)
import BananaSplit.ULID qualified as ULID
import BananaSplit.User (User)

data Participante = Participante
  { id :: ULID
  , nombre :: Text
  , user :: Maybe User
  , datosDeCobro :: Maybe Text
  }
  deriving (Show, Eq, Generic)

data ParticipanteError
  = ParticipanteNotFound
  | EmptyName
  | NameAlreadyTaken
  | ParticipanteInUse ParticipanteUsage
  deriving (Show, Eq, Generic)

-- | El nombre de un participante sin los espacios de alrededor. Uno vacío no es
-- un nombre.
validarNombre :: Text -> Either ParticipanteError Text
validarNombre nombre =
  case Text.strip nombre of
    "" -> Left EmptyName
    limpio -> Right limpio

-- | Qué sigue referenciando a un participante que se quiso borrar.
data ParticipanteUsage
  = UsedInGasto
  | UsedInRepartija
  | UsedInTransferencia
  deriving (Show, Eq, Generic)

data ParticipanteClaimError
  = ClaimedByOtherUser
  | AlreadyOwnAnotherParticipante
  | ParticipanteError ParticipanteError
  deriving (Show, Eq, Generic)

newtype ParticipanteId = ParticipanteId ULID
  deriving (Generic)
  deriving newtype (Show, Eq, ToJSONKey, FromJSONKey)

instance Ord ParticipanteId where
  compare (ParticipanteId ulid1) (ParticipanteId ulid2) =
    ULID.ulidToInteger ulid1 `compare` ULID.ulidToInteger ulid2

participanteId2ULID :: ParticipanteId -> ULID
participanteId2ULID (ParticipanteId ulid) = ulid

Elm.deriveBoth Elm.defaultOptions ''ParticipanteId
Elm.deriveBoth Elm.defaultOptions ''Participante
Elm.deriveBoth Elm.defaultOptions ''ParticipanteUsage
Elm.deriveBoth Elm.defaultOptions ''ParticipanteError
Elm.deriveBoth Elm.defaultOptions ''ParticipanteClaimError
