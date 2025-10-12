{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
module BananaSplit.Participante
    ( Participante (..)
    , ParticipanteId (..)
    , participanteId2ULID
    ) where

import BananaSplit.ULID (ULID)
import BananaSplit.ULID qualified as ULID

import Data.Aeson

import Elm.Derive qualified as Elm

import Protolude

data Participante = Participante
  { participanteId :: ULID
  , participanteNombre :: Text
  }
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
