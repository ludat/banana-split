{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TemplateHaskell #-}

module BananaSplit.Repartija where

import BananaSplit.Monto (Monto)
import BananaSplit.Participante (ParticipanteId)
import BananaSplit.ULID (ULID)

import Elm.Derive qualified as Elm

import Protolude

data Repartija = Repartija
  { id :: ULID
  , extra :: Monto
  , items :: [RepartijaItem]
  , claims :: [RepartijaClaim]
  } deriving (Show, Eq, Generic)

data ShallowRepartija = ShallowRepartija
  { shallowId :: ULID
  , shallowNombre :: Text
  } deriving (Show, Eq, Generic)

data RepartijaItem = RepartijaItem
  { id :: ULID
  , nombre :: Text
  , monto :: Monto
  , cantidad :: Int
  } deriving (Show, Eq, Generic)

data RepartijaClaim = RepartijaClaim
  { id :: ULID
  , participante :: ParticipanteId
  , itemId :: ULID
  , cantidad :: Maybe Int
  } deriving (Show, Eq, Generic)

Elm.deriveBoth Elm.defaultOptions ''RepartijaItem
Elm.deriveBoth Elm.defaultOptions ''RepartijaClaim
Elm.deriveBoth Elm.defaultOptions ''ShallowRepartija
Elm.deriveBoth Elm.defaultOptions ''Repartija
