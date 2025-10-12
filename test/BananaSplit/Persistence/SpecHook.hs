module BananaSplit.Persistence.SpecHook
    ( RunDb (..)
    , hook
    ) where


import BananaSplit.PgRoll (init, startAndComplete)

import Conferer qualified

import Database.Beam.Postgres (Pg, runBeamPostgres)
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL, execute_)

import Protolude

import Site.Config (createConfig)

import Test.Hspec

hook :: SpecWith RunDb -> Spec
hook =
  aroundAll_ setupDbAndReset . around withTestDbConn

newtype RunDb = RunDb (forall a. Pg a -> IO a)

setupDbAndReset :: IO () -> IO ()
setupDbAndReset action = do
  config <- createConfig "test"
  init config
  startAndComplete config
  dbUrl <- Conferer.fetchFromConfig "database.url" config
  conn <- connectPostgreSQL dbUrl
  resetTestDb conn
  action

getTestDbUrl :: IO ByteString
getTestDbUrl = do
  config <- createConfig "test"
  Conferer.fetchFromConfig "database.url" config

connectTestDb :: IO Connection
connectTestDb = do
  dbUrl <- getTestDbUrl
  connectPostgreSQL dbUrl

resetTestDb :: Connection -> IO ()
resetTestDb conn = do
  -- void $ execute_ conn "TRUNCATE TABLE repartija_claims CASCADE"
  -- void $ execute_ conn "TRUNCATE TABLE repartija_items CASCADE"
  -- void $ execute_ conn "TRUNCATE TABLE repartijas CASCADE"
  -- void $ execute_ conn "TRUNCATE TABLE distribuciones_montos_especificos_items CASCADE"
  -- void $ execute_ conn "TRUNCATE TABLE distribuciones_montos_especificos CASCADE"
  -- void $ execute_ conn "TRUNCATE TABLE distribuciones_monto_equitativo_items CASCADE"
  -- void $ execute_ conn "TRUNCATE TABLE distribuciones_monto_equitativo CASCADE"
  void $ execute_ conn "TRUNCATE TABLE distribuciones CASCADE"
  -- void $ execute_ conn "TRUNCATE TABLE pagos CASCADE"
  -- void $ execute_ conn "TRUNCATE TABLE participantes CASCADE"
  void $ execute_ conn "TRUNCATE TABLE grupos CASCADE"

-- | Combinator that sets up a test database connection and resets it after the test
withTestDbConn :: ActionWith RunDb -> IO ()
withTestDbConn action = do
  bracket
    (do
      c <- connectTestDb
      _ <- runBeamPostgres c $ liftIO $ execute_ c "BEGIN"
      pure c
    )
    (\c -> do
      _ <- runBeamPostgres c $ liftIO $ execute_ c "ROLLBACK"
      close c
      pure ()
    )
    (\conn -> do
      action $ RunDb $ runBeamPostgres conn
    )
