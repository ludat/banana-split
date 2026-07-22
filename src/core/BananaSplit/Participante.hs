{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module BananaSplit.Participante (
  Participante (..),
  ParticipanteId (..),
  ClaimRejection (..),
  participanteId2ULID,
) where

import Data.Aeson
import Elm.Derive qualified as Elm
import Protolude

import BananaSplit.ULID (ULID)
import BananaSplit.ULID qualified as ULID
import BananaSplit.User (User)

data Participante = Participante
  { id :: ULID
  , nombre :: Text
  , user :: Maybe User
  }
  deriving (Show, Eq, Generic)

data ClaimRejection
  = ClaimedByOtherUser
  | AlreadyOwnAnotherParticipante
  | ParticipanteNotFound
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
Elm.deriveBoth Elm.defaultOptions ''ClaimRejection
