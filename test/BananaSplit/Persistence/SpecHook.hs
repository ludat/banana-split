module BananaSplit.Persistence.SpecHook (
  RunDb (..),
  hook,
) where

import Data.Pool qualified as Pool
import Database.Beam.Postgres (Pg)
import Database.Beam.Postgres qualified as Beam
import Database.PostgreSQL.Simple qualified as Pg
import Protolude
import Test.Hspec

import BananaSplit.Persistence qualified as Persistence
import BaseDePrueba (prepararBase)
import Site.Config qualified as Config

hook :: SpecWith RunDb -> Spec
hook =
  beforeAll_ prepararBase . aroundAll setupDb . aroundWith withTestDbConn

newtype RunDb = RunDb (forall a. Pg a -> IO a)

setupDb :: ActionWith Pg.Connection -> IO ()
setupDb action = do
  config <- Config.createConfig "test"
  pool <- Persistence.makePool config
  Pool.withResource pool $ \conn -> do
    action conn

withTestDbConn :: ActionWith RunDb -> ActionWith Pg.Connection
withTestDbConn action = \conn -> do
  _ <- Pg.execute_ conn "BEGIN"
  (action $ RunDb $ Beam.runBeamPostgres conn)
    `finally` Pg.execute_ conn "ROLLBACK"
