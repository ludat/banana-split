{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | La mónada concreta en la que corren los comandos de consola.
--
-- Vive acá, en el executable, y no en una librería: las librerías piden las
-- capacidades que usan —'MonadTelemetry', 'Katip.KatipContext'— y no se atan a
-- ninguna mónada, así que cada entry point elige la suya. El test suite tiene la
-- propia en "BananaSplit.Persistence.SpecHook".
--
-- Abajo hay 'IO' y no el @ExceptT@ de Servant, así que tiene 'MonadUnliftIO' y la
-- instancia de 'MonadTelemetry' sale de 'inSpanConTracer'' en una línea.
module AppRunner (
  AppRunner,
  runApp,
) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Katip qualified
import Protolude

import BananaSplit.Telemetry (MonadTelemetry (..), Telemetry (..), inSpanConTracer')

newtype AppRunner a = AppRunner (Katip.KatipContextT (ReaderT Telemetry IO) a)
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

instance MonadTelemetry AppRunner where
  inSpan' = inSpanConTracer'

-- | Corre un comando. Se llama una vez, en 'Main.main'.
runApp :: Telemetry -> AppRunner a -> IO a
runApp telemetry (AppRunner accion) =
  runReaderT (Katip.runKatipContextT telemetry.logEnv () mempty accion) telemetry
