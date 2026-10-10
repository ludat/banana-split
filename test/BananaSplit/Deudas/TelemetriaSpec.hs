{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Lo que 'minimizeTransferencias' deja en su span.
--
-- Vive en su propio módulo porque "BananaSplit.DeudasSpec" es dominio puro y esto
-- necesita un tracer; y porque la versión instrumentada no tiene todavía ningún
-- llamador de producción —'BananaSplit.Persistence.congelarGrupo' corre en 'Pg' y
-- usa la pura— así que sin estos tests no la ejercitaría nada.
module BananaSplit.Deudas.TelemetriaSpec (
  spec,
) where

import Data.IORef (readIORef)
import Katip qualified
import Katip.Monadic qualified as Katip
import OpenTelemetry.Attributes qualified as Attributes
import OpenTelemetry.Exporter.InMemory.Span (inMemoryListExporter)
import OpenTelemetry.Trace.Core (ImmutableSpan (..), Span, SpanHot (..))
import OpenTelemetry.Trace.Core qualified as Otel
import OpenTelemetry.Util (appendOnlyBoundedCollectionValues)
import Protolude
import Test.Hspec

import BananaSplit
import BananaSplit.Telemetry (MonadTelemetry (..), MonadTracer (..), tracerQueNoGraba)
import BananaSplit.TestUtils

newtype ConSpan a = ConSpan (ReaderT Span (Katip.NoLoggingT IO) a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Span
    , Katip.Katip
    , Katip.KatipContext
    )

instance MonadTracer ConSpan where
  getTracer = tracerQueNoGraba

instance MonadTelemetry ConSpan where
  inSpan' _ accion = ask >>= accion

-- | Corre la acción con un span de verdad y devuelve el span ya cerrado.
conUnSpan :: ConSpan a -> IO (a, SpanHot)
conUnSpan (ConSpan accion) = do
  (processor, ref) <- inMemoryListExporter
  provider <- Otel.createTracerProvider [processor] Otel.emptyTracerProviderOptions
  let tracer = Otel.makeTracer provider instrumentationLibrary Otel.tracerOptions
  resultado <-
    Otel.inSpan' tracer "test" Otel.defaultSpanArguments $ \span ->
      Katip.runNoLoggingT (runReaderT accion span)
  spans <- readIORef ref
  case spans of
    [span] -> (resultado,) <$> readIORef span.spanHot
    other -> panic $ "se esperaba un span y salieron " <> show (length other)

instrumentationLibrary :: Otel.InstrumentationLibrary
instrumentationLibrary =
  Otel.InstrumentationLibrary
    { libraryName = "test"
    , libraryVersion = ""
    , librarySchemaUrl = ""
    , libraryAttributes = Attributes.emptyAttributes
    }

textoEn :: SpanHot -> Text -> Maybe Text
textoEn hot clave =
  case Attributes.lookupAttribute hot.hotAttributes clave of
    Just (Attributes.AttributeValue (Attributes.TextAttribute texto)) -> Just texto
    _ -> Nothing

eventosDe :: SpanHot -> [Text]
eventosDe hot =
  toList $ Otel.eventName <$> appendOnlyBoundedCollectionValues hot.hotEvents

spec :: Spec
spec = do
  let u1 = participante 1
      u2 = participante 2

  describe "minimizeTransferencias" $ do
    -- Con dos participantes y una deuda simple el solver llega siempre, así que
    -- esto fija el camino feliz: el span dice con qué algoritmo se resolvió.
    it "deja en el span que resolvió con el solver" $ do
      (transferencias, hot) <-
        conUnSpan $ minimizeTransferencias (netos [(u1, mkMonto 10 5), (u2, mkMonto 10 -5)])

      transferencias `shouldBe` [TransferenciaSugerida u2 u1 (mkMonto 10 5)]
      textoEn hot "app.deudas.algoritmo" `shouldBe` Just "optimo"
      eventosDe hot `shouldNotContain` ["deudas.solver_fallback"]

    -- Sin deudas no hay nada que minimizar, pero el span tiene que salir igual:
    -- si el atributo faltara no se podría medir cada cuánto cae al naíf.
    it "deja el atributo incluso cuando no hay nada que transferir" $ do
      (transferencias, hot) <- conUnSpan $ minimizeTransferencias mempty

      transferencias `shouldBe` []
      textoEn hot "app.deudas.algoritmo" `shouldBe` Just "optimo"

  describe "minimizeTransferenciasPuro" $ do
    -- La razón de existir de la versión pura: mismo resultado, sin pedir nada.
    it "da el mismo resultado que la instrumentada" $ do
      let deudas = netos [(u1, mkMonto 10 5), (u2, mkMonto 10 -5)]
      (instrumentada, _) <- conUnSpan $ minimizeTransferencias deudas

      minimizeTransferenciasPuro deudas `shouldBe` instrumentada
