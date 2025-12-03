module Site.Types
    ( App (..)
    , AppHandler
    ) where

import BananaSplit.Receipts

import Control.Monad.Reader

import Data.Pool

import Database.Beam.Postgres qualified as Beam

import Servant

data App = App
  { beamConnectionPool :: Pool Beam.Connection
  , receipts :: ReceiptsReaderConfig
  }

type AppHandler = ReaderT App Handler
