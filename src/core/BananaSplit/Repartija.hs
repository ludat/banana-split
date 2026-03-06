{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module BananaSplit.Repartija where

import Elm.Derive qualified as Elm
import Protolude

import BananaSplit.Monto (Monto)
import BananaSplit.Participante (ParticipanteId)
import BananaSplit.ULID (ULID)

data Repartija = Repartija
  { id :: ULID
  , nombre :: Text
  , extra :: Monto
  , items :: [RepartijaItem]
  , claims :: [RepartijaClaim]
  }
  deriving (Show, Eq, Generic)

data ShallowRepartija = ShallowRepartija
  { shallowId :: ULID
  , shallowNombre :: Text
  }
  deriving (Show, Eq, Generic)

data RepartijaItem = RepartijaItem
  { id :: ULID
  , nombre :: Text
  , monto :: Monto
  , cantidad :: Int
  }
  deriving (Show, Eq, Generic)

data RepartijaClaim = RepartijaClaim
  { id :: ULID
  , participante :: ParticipanteId
  , itemId :: ULID
  , cantidad :: Maybe Int
  }
  deriving (Show, Eq, Generic)

Elm.deriveBoth Elm.defaultOptions ''RepartijaItem
Elm.deriveBoth Elm.defaultOptions ''RepartijaClaim
Elm.deriveBoth Elm.defaultOptions ''ShallowRepartija
Elm.deriveBoth Elm.defaultOptions ''Repartija
