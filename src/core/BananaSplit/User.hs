{-# LANGUAGE TemplateHaskell #-}

module BananaSplit.User (
  User (..),
) where

import Elm.Derive qualified as Elm
import Protolude

import BananaSplit.Email (Email)
import BananaSplit.ULID (ULID)

-- | A user account. Identified by email only (prototype: no password).
-- 'createdAt' lives in the database but is intentionally not part of the
-- domain/API type — the frontend and JWT session only ever need id/email/nombre.
data User = User
  { id :: ULID
  , email :: Email
  , nombre :: Text
  }
  deriving (Show, Eq, Generic)

Elm.deriveBoth Elm.defaultOptions ''User
