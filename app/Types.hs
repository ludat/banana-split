{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

------------------------------------------------------------------------------

-- | This module defines our application's state type and an alias for its
-- handler monad.
module Types where

------------------------------------------------------------------------------

import Control.Monad.Reader

import Data.Pool

import Database.Selda.Backend
import Database.Selda.PostgreSQL

import Servant

------------------------------------------------------------------------------
data App = App
  { connection :: Pool (SeldaConnection PG)
  }

------------------------------------------------------------------------------
type AppHandler = ReaderT App Handler
