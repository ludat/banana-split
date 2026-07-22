{-# LANGUAGE TemplateHaskell #-}

module BananaSplit.User (
  User (..),
) where

import Elm.Derive qualified as Elm
import Protolude

import BananaSplit.Email (Email)
import BananaSplit.ULID (ULID)

data User = User
  { id :: ULID
  , email :: Email
  , nombre :: Text
  }
  deriving (Show, Eq, Generic)

Elm.deriveBoth Elm.defaultOptions ''User
