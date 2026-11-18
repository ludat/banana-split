{-# LANGUAGE TemplateHaskell #-}

module Repartix.User (
  User (..),
) where

import Elm.Derive qualified as Elm
import Protolude

import Repartix.Email (Email)
import Repartix.ULID (ULID)

data User = User
  { id :: ULID
  , email :: Email
  , nombre :: Text
  , datosDeCobro :: Maybe Text
  }
  deriving (Show, Eq, Generic)

Elm.deriveBoth Elm.defaultOptions ''User
