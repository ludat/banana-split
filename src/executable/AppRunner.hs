{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
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

instance Telemetry.HasTelemetry Entorno where
  getTelemetry entorno = entorno.telemetry
  overTelemetry f entorno = entorno{telemetry = f entorno.telemetry}

newtype AppRunner a = AppRunner (ReaderT Entorno IO a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadReader Entorno
    )
  deriving
    (Katip.Katip, Katip.KatipContext, MonadTracer)
    via (Telemetry.ViaTelemetry (ReaderT Entorno IO))

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

-- | Abajo hay 'IO' y no el @ExceptT@ de Servant, así que hay unlift y el span se
-- cierra solo: la que abre el span es la de la librería.
instance MonadTelemetry AppRunner where
  inSpan' nombre = OtelMonad.inSpan' nombre Otel.defaultSpanArguments
