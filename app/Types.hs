{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

------------------------------------------------------------------------------

-- | This module defines our application's state type and an alias for its
-- handler monad.
module Types where

------------------------------------------------------------------------------

import Control.Monad.Reader

import Data.ByteString (ByteString)
import Data.Text (Text)

import Database.PostgreSQL.Simple qualified as PG

import Servant
import GHC.Conc (TVar)
import Database.Selda.Backend
import Database.Selda.PostgreSQL

------------------------------------------------------------------------------
data App = App
  { connection :: SeldaConnection PG
  }

------------------------------------------------------------------------------
type AppHandler = ReaderT App Handler
