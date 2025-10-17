module Site.Types
    ( App (..)
    , AppHandler
    ) where

import Control.Monad.Reader

import Data.Pool

import Database.Beam.Postgres qualified as Beam

import Network.HTTP.Client (Manager)

import Protolude hiding (Handler)

import Servant

data App = App
  { beamConnectionPool :: Pool Beam.Connection
  , openRouterApiKey :: Text
  , httpManager :: Manager
  }

type AppHandler = ReaderT App Handler
