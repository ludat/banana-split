{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TestRunner (
  TestRunner,
  runTest,
) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Katip qualified
import OpenTelemetry.Trace.Core qualified as Otel
import OpenTelemetry.Trace.Monad qualified as OtelMonad
import Protolude

import BananaSplit.Telemetry (MonadTelemetry (..), MonadTracer (..), Telemetry (..), ViaTelemetry (..))

newtype TestRunner a = TestRunner (ReaderT Telemetry IO a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadReader Telemetry
    )
  deriving
    (Katip.Katip, Katip.KatipContext, MonadTracer)
    via (ViaTelemetry (ReaderT Telemetry IO))

instance MonadTelemetry TestRunner where
  inSpan' nombre = OtelMonad.inSpan' nombre Otel.defaultSpanArguments

runTest :: Telemetry -> TestRunner a -> IO a
runTest telemetry (TestRunner accion) =
  runReaderT accion telemetry
