module Repartix.Persistence.SpecHook (
  RunDb (..),
  hook,
) where

import Data.Pool qualified as Pool
import Database.PostgreSQL.Simple qualified as Pg
import Protolude
import Test.Hspec

import Repartix.Persistence qualified as Persistence
import Repartix.Persistence.Pg (Pg)
import Repartix.PgRoll qualified as PgRoll
import Repartix.Telemetry (Telemetry, telemetryFromGlobals)
import Site.Config qualified as Config
import TestRunner (runTest)

hook :: SpecWith RunDb -> Spec
hook =
  aroundAll setupDb . aroundWith withTestDbConn

newtype RunDb = RunDb (forall a. Pg a -> IO a)

setupDb :: ActionWith (Telemetry, Pg.Connection) -> IO ()
setupDb action = do
  config <- Config.createConfig "test"
  -- Los tests no levantan el SDK, así que esto es un no-op: las migraciones
  -- quedan instrumentadas igual pero no sale nada hacia ningún collector.
  telemetry <- telemetryFromGlobals config
  runTest telemetry $ do
    PgRoll.init config
    PgRoll.startAndComplete config
  pool <- Persistence.makePool config
  Pool.withResource pool $ \conn -> do
    action (telemetry, conn)

withTestDbConn :: ActionWith RunDb -> ActionWith (Telemetry, Pg.Connection)
withTestDbConn action = \(telemetry, conn) -> do
  _ <- Pg.execute_ conn "BEGIN"
  -- Sin abrir transacción: la de verdad es el BEGIN/ROLLBACK de acá afuera, que es
  -- lo que hace que un test no le deje nada escrito al siguiente.
  (action $ RunDb $ Persistence.correrEnLaTransaccionDeAfuera telemetry conn)
    `finally` Pg.execute_ conn "ROLLBACK"
