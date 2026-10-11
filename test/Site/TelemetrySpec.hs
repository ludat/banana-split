module Site.TelemetrySpec (
  spec,
) where

import Network.Wai (defaultRequest, requestHeaders)
import OpenTelemetry.Propagator qualified as Propagator
import Protolude
import Test.Hspec

import Site.Telemetry (propagationCarrier, templateRoute)

spec :: Spec
spec = do
  -- Esto existe porque se rompió: el propagador de 'OTelSignals' viene vacío,
  -- así que el carrier quedaba vacío y todos los spans del backend aparecían
  -- como raíz en lugar de colgar del trace del browser. Ver 'mkTelemetry'.
  describe "propagationCarrier" $ do
    let carrierOf headers =
          sort $
            Propagator.textMapToList $
              propagationCarrier defaultRequest{requestHeaders = headers}

    it "pasa el traceparent" $ do
      carrierOf [("traceparent", "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01")]
        `shouldBe` [("traceparent", "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01")]

    it "baja los nombres de header a minúscula, que es como los buscan los propagadores" $ do
      carrierOf [("TraceParent", "00-abc-def-01")] `shouldBe` [("traceparent", "00-abc-def-01")]

    -- No se filtra por 'propagatorFields' justamente por esto: el propagador de
    -- B3 declara @["B3 Trace Context"]@ en vez de nombres de header, así que
    -- filtrar descartaría el header que hay que leer. Cualquier header que un
    -- propagador de la librería pueda querer tiene que llegarle.
    it "pasa los headers de otros propagadores, no solo los de W3C" $ do
      carrierOf [("b3", "4bf9-00f0-1"), ("x-amzn-trace-id", "Root=1-2-3")]
        `shouldBe` [("b3", "4bf9-00f0-1"), ("x-amzn-trace-id", "Root=1-2-3")]

    it "con un request sin headers queda vacío" $ do
      carrierOf [] `shouldBe` []

  describe "templateRoute" $ do
    it "deja pasar una ruta sin parámetros" $ do
      templateRoute ["api", "auth", "request-code"] `shouldBe` "/api/auth/request-code"

    it "enmascara ULIDs" $ do
      templateRoute ["api", "grupo", "01M3YV6KN5DTKGEPNDPW8AAFGY", "pagos"]
        `shouldBe` "/api/grupo/:id/pagos"

    it "enmascara varios ids en la misma ruta" $ do
      templateRoute ["api", "grupo", "01M3YV6KN5DTKGEPNDPW8AAFGY", "pagos", "01M3YVT1TR1D89NGKCW3WKV5K5"]
        `shouldBe` "/api/grupo/:id/pagos/:id"

    it "enmascara UUIDs" $ do
      templateRoute ["api", "x", "0e5a3f3e-7b1f-4f1a-9a2e-2f0c1d8e4b55"]
        `shouldBe` "/api/x/:id"

    it "enmascara números" $ do
      templateRoute ["api", "grupo", "42"] `shouldBe` "/api/grupo/:id"

    -- Nada de la API tiene un mail en el path hoy, pero si alguna vez lo tiene
    -- no queremos que termine en el nombre de un span.
    it "enmascara cualquier cosa que parezca un mail" $ do
      templateRoute ["api", "u", "a@b.co"] `shouldBe` "/api/u/:id"

    -- Al revés que en el frontend, acá no hay lista de palabras conocidas: una
    -- ruta nueva tiene que aparecer con su nombre sin tocar código.
    it "deja pasar un segmento desconocido que no parece id" $ do
      templateRoute ["api", "ruta-nueva"] `shouldBe` "/api/ruta-nueva"

    it "conserva el slash final de una ruta de directorio" $ do
      templateRoute ["api", "grupos", ""] `shouldBe` "/api/grupos/"
