{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

------------------------------------------------------------------------------

-- | This module defines our application's state type and an alias for its
-- handler monad.
module Types where

------------------------------------------------------------------------------

import Control.Monad.Reader

import Servant
import Database.Selda.Backend
import Database.Selda.PostgreSQL

------------------------------------------------------------------------------
data App = App
  { connection :: SeldaConnection PG
  }

------------------------------------------------------------------------------
type AppHandler = ReaderT App Handler
