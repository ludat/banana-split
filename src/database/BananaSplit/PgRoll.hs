module BananaSplit.PgRoll
    ( init
    , rawCall
    , rollback
    , start
    , startAndComplete
    ) where
import Conferer

import Data.String

import Protolude

import System.Process (callProcess)

rawCall :: Config -> [String] -> IO ()
rawCall config args = do
  connString <- Conferer.fetchFromConfig "database.url" config
  let connectionArgs = ["--postgres-url", connString ++ "?sslmode=disable"]
  callProcess "pgroll" $ connectionArgs ++ args

init :: Config -> IO ()
init config = do
  rawCall config ["init"]

start :: Config -> IO ()
start config = do
  rawCall config ["migrate", "./migrations"]

rollback :: Config -> IO ()
rollback config = do
  rawCall config ["rollback"]

startAndComplete :: Config -> IO ()
startAndComplete config = do
  rawCall config ["migrate", "--complete", "./migrations"]
