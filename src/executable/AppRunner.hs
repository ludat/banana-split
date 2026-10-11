{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | La mónada concreta en la que corren los comandos de consola.
--
-- Vive acá, en el executable, y no en una librería: las librerías piden las
-- capacidades que usan —'MonadTelemetry', 'Katip.KatipContext',
-- 'Persistence.MonadPg'— y no se atan a ninguna mónada, así que cada entry point
-- elige la suya. El test suite tiene la propia en "TestRunner".
--
-- Lleva una conexión y no un pool porque un comando no tiene concurrencia que
-- ganar: las transacciones van una después de la otra.
module AppRunner (
  AppRunner,
  runApp,
) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Database.Beam.Postgres (Connection)
import Katip qualified
import OpenTelemetry.Trace.Core qualified as Otel
import OpenTelemetry.Trace.Monad qualified as OtelMonad
import Protolude

import BananaSplit.Persistence qualified as Persistence
import BananaSplit.Telemetry (MonadTelemetry (..), MonadTracer (..), Telemetry (..))
import BananaSplit.Telemetry qualified as Telemetry

data Entorno = Entorno
  { telemetry :: Telemetry
  , conexion :: Connection
  }

newtype AppRunner a = AppRunner (ReaderT Entorno IO a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadReader Entorno
    )

-- | Corre un comando. Se llama una vez, en 'Main.main'.
runApp :: Telemetry -> Connection -> AppRunner a -> IO a
runApp telemetry conexion (AppRunner accion) =
  runReaderT accion Entorno{telemetry = telemetry, conexion = conexion}

instance Persistence.MonadPg AppRunner where
  runBeamWrite accion = do
    entorno <- ask
    liftIO $
      Persistence.conTransaccionDeEscritura
        entorno.telemetry
        entorno.conexion
        accion

  runBeamFastRead accion = do
    entorno <- ask
    liftIO $
      Persistence.conTransaccionDeLecturaRapida
        entorno.telemetry
        entorno.conexion
        accion

instance MonadTracer AppRunner where
  getTracer = asks (.telemetry.tracer)

-- | Abajo hay 'IO' y no el @ExceptT@ de Servant, así que hay unlift y el span se
-- cierra solo: la que abre el span es la de la librería.
instance MonadTelemetry AppRunner where
  inSpan' nombre = OtelMonad.inSpan' nombre Otel.defaultSpanArguments

-- | A mano y no por @deriving via@ 'Telemetry.ConTelemetria', por lo mismo: el
-- reader es más grande que el 'Telemetry'. Igual que en 'Site.Types.AppHandler'.
instance Katip.Katip AppRunner where
  getLogEnv = asks (.telemetry.logEnv)
  localLogEnv f = sobreTelemetry (Telemetry.sobreLogEnv f)

instance Katip.KatipContext AppRunner where
  getKatipContext = asks (.telemetry.logContexts)
  localKatipContext f = sobreTelemetry (Telemetry.sobreLogContexts f)
  getKatipNamespace = asks (.telemetry.logNamespace)
  localKatipNamespace f = sobreTelemetry (Telemetry.sobreLogNamespace f)

sobreTelemetry :: (Telemetry -> Telemetry) -> AppRunner a -> AppRunner a
sobreTelemetry f = local $ \entorno -> entorno{telemetry = f entorno.telemetry}
