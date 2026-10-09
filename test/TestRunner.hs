{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TestRunner (
  TestRunner,
  runTest,
) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Katip qualified
import Protolude

import BananaSplit.Telemetry (MonadTelemetry (..), Telemetry (..), inSpanConTracer')

newtype TestRunner a = TestRunner (Katip.KatipContextT (ReaderT Telemetry IO) a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadReader Telemetry
    , Katip.Katip
    , Katip.KatipContext
    )

instance MonadTelemetry TestRunner where
  inSpan' = inSpanConTracer'

runTest :: Telemetry -> TestRunner a -> IO a
runTest telemetry (TestRunner accion) =
  runReaderT (Katip.runKatipContextT telemetry.logEnv () mempty accion) telemetry
