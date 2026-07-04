{-# LANGUAGE TemplateHaskell #-}

module BananaSplit.User (
  User (..),
  normalizeEmail,
) where

import Data.Text qualified as Text
import Elm.Derive qualified as Elm
import Protolude

import BananaSplit.ULID (ULID)

-- | A user account. Identified by email only (prototype: no password).
-- 'createdAt' lives in the database but is intentionally not part of the
-- domain/API type — the frontend and JWT session only ever need id/email/nombre.
data User = User
  { id :: ULID
  , email :: Text
  , nombre :: Text
  }
  deriving (Show, Eq, Generic)

-- | The single place where emails get normalized. Keep it that way so a lookup
-- and an insert can never disagree about what \"the same email\" means.
normalizeEmail :: Text -> Text
normalizeEmail = Text.toLower . Text.strip

Elm.deriveBoth Elm.defaultOptions ''User
