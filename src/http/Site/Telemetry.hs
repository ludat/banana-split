-- | Lo de OpenTelemetry que es específico de HTTP: el middleware que abre un
-- span por request y el 'Mailer' instrumentado.
--
-- El setup del SDK y los helpers que valen en cualquier parte (incluidos los
-- comandos de consola) están en "BananaSplit.Telemetry". Los handlers usan los
-- de "Site.Handler.Utils", que son los mismos adaptados a 'AppHandler'.
module Site.Telemetry (
  traceApiMiddleware,
  tracedMailer,

  -- * Expuesto para los tests
  propagationCarrier,
  templateRoute,
) where

import Data.CaseInsensitive qualified as CI
import Data.Text qualified as Text
import GHC.Clock (getMonotonicTime)
import Network.HTTP.Types.Status (statusCode)
import Network.Wai (
  Middleware,
  Request,
  pathInfo,
  rawPathInfo,
  requestHeaders,
  requestMethod,
  responseStatus,
 )
import OpenTelemetry.Attributes (Attributes, AttrsBuilder, attr, emptyAttributes)
import OpenTelemetry.Attributes qualified as Attributes
import OpenTelemetry.Context qualified as Context
import OpenTelemetry.Context.ThreadLocal qualified as ThreadLocal
import OpenTelemetry.Metric qualified as Metric
import OpenTelemetry.Propagator qualified as Propagator
import OpenTelemetry.Trace.Core (SpanArguments (..), SpanKind (..), SpanStatus (..))
import OpenTelemetry.Trace.Core qualified as Otel
import Protolude

import BananaSplit.Email (Email, unEmail)
import BananaSplit.Telemetry (Telemetry (..))
import Site.Mailer (Mailer (..))

-- | Un span de servidor más una medición de @http.server.request.duration@ por
-- request a @\/api@, continuando el trace del frontend cuando el request trae
-- @traceparent@ (el SDK del browser lo manda porque @\/api@ es same-origin, ver
-- @ui\/TELEMETRY.md@).
--
-- Devuelve el middleware en 'IO' porque el instrumento de métrica se crea una
-- sola vez acá, no por request.
--
-- Deja el span en el contexto thread-local, que es de dónde lo saca todo lo
-- demás: los spans de base de datos, los de mailer y los log records que emiten
-- los handlers cuelgan de este sin que haya que pasarlo a mano.
--
-- Deliberadamente /solo/ @\/api@: el resto lo sirve el handler de archivos
-- estáticos, y un span por @\/assets\/index-\<hash\>.js@ es un nombre de ruta
-- nuevo por cada build, que es justo lo que no querés como cardinalidad.
traceApiMiddleware :: Telemetry -> IO Middleware
traceApiMiddleware telemetry = do
  requestDuration <-
    Metric.meterCreateHistogram
      telemetry.meter
      "http.server.request.duration"
      (Just "s")
      (Just "Duración de los requests a /api")
      Metric.defaultAdvisoryParameters

  pure $ \app req respond ->
    if pathInfo req & take 1 & (/= ["api"])
      then app req respond
      else do
        parent <- Propagator.extract telemetry.propagators (propagationCarrier req) Context.empty
        _ <- ThreadLocal.attachContext parent

        let method = decodeUtf8 (requestMethod req)
            route = templateRoute (pathInfo req)

        start <- getMonotonicTime
        Otel.inSpan' telemetry.tracer (method <> " " <> route) serverSpanArguments $ \requestSpan -> do
          Otel.addAttributes requestSpan $
            Attributes.buildAttrs $
              attr "http.request.method" method
                <> attr "http.route" route
                <> attr "url.path" (decodeUtf8 (rawPathInfo req))
          app req $ \response -> do
            elapsed <- (\end -> end - start) <$> getMonotonicTime
            let status = statusCode (responseStatus response)
            Otel.addAttribute requestSpan "http.response.status_code" (fromIntegral status :: Int64)
            Metric.histogramRecord requestDuration elapsed $
              buildAttributes $
                attr "http.request.method" method
                  <> attr "http.route" route
                  <> attr "http.response.status_code" (fromIntegral status :: Int64)
            -- Solo 5xx es error del server. Un 401 o un 404 es el server
            -- haciendo su trabajo, y marcarlos como error deja el trace lleno
            -- de rojo que no hay que mirar.
            when (status >= 500) $
              Otel.setStatus requestSpan (Error $ "HTTP " <> show status)
            respond response
  where
    serverSpanArguments = Otel.defaultSpanArguments{kind = Server}

-- | Los headers del request como carrier, con los nombres en minúscula (son
-- case-insensitive en HTTP, y los propagadores buscan la forma minúscula).
--
-- Van /todos/, y el propagador lee los suyos. Filtrar por
-- 'Propagator.propagatorFields' parece lo prolijo — no copiar cookies ni
-- @Authorization@ a una estructura intermedia — pero no se puede confiar en esa
-- lista: el propagador de W3C devuelve nombres de header
-- (@["traceparent","tracestate","baggage"]@) pero el de B3 devuelve su propio
-- nombre (@["B3 Trace Context"]@), así que el filtro descartaría justo el header
-- que hay que leer. Y como el carrier es un 'HashMap' efímero del que nada sale
-- hacia la telemetría, lo que se gana filtrando es higiene, no privacidad; no
-- vale pagarlo con que la continuidad del trace dependa de qué propagador
-- eligió alguien.
propagationCarrier :: Request -> Propagator.TextMap
propagationCarrier req =
  Propagator.textMapFromList
    [ (decodeUtf8 (CI.foldedCase name), decodeUtf8 value)
    | (name, value) <- requestHeaders req
    ]

buildAttributes :: AttrsBuilder -> Attributes
buildAttributes =
  Attributes.addAttributesFromBuilder Attributes.defaultAttributeLimits emptyAttributes

-- | Ruta de baja cardinalidad para un path: cada segmento que parezca un id se
-- reemplaza por @:id@.
--
-- Al revés que en el frontend (que tiene una lista de palabras de ruta
-- conocidas), acá enmascaramos solo lo que parece id y dejamos pasar lo demás:
-- así una ruta nueva aparece con su nombre en Grafana sin tener que tocar esta
-- función. Los ids de esta app son ULIDs (26 caracteres), así que el largo
-- alcanza para agarrarlos a todos, UUIDs incluidos.
templateRoute :: [Text] -> Text
templateRoute segments =
  "/" <> Text.intercalate "/" (fmap maskSegment segments)

maskSegment :: Text -> Text
maskSegment segment
  | Text.null segment = segment
  | Text.all isDigit segment = ":id"
  | Text.length segment >= 20 = ":id"
  -- Nada de la API tiene un mail en el path hoy, pero si alguna vez lo tiene
  -- no queremos que termine en el nombre de un span.
  | Text.any (`elem` ['@', '%']) segment = ":id"
  | otherwise = segment

-- | Envuelve un 'Mailer' para que cada envío sea un span. Va envolviendo en vez
-- de instrumentar "Site.Mailer" por adentro para que valga igual para el mailer
-- de consola y el de SMTP, y para que la lógica de SMTP no sepa de telemetría.
--
-- Esto es lo que da visibilidad de los mails, que de otra forma no tienen
-- ninguna: un SMTP que tarda 8 segundos o que falla queda como un span con
-- error colgando del request que lo disparó.
tracedMailer :: Telemetry -> Mailer -> Mailer
tracedMailer telemetry mailer =
  Mailer
    { sendLoginCode = \email code ->
        sending "login_code" email $ mailer.sendLoginCode email code
    , sendNotice = \email subject body ->
        -- El subject no se registra: en las respuestas a mails entrantes es
        -- "Re: " + lo que escribió la persona.
        sending "notice" email $ mailer.sendNotice email subject body
    }
  where
    sending :: Text -> Email -> IO () -> IO ()
    sending emailKind email action =
      Otel.inSpan' telemetry.tracer ("mailer.send " <> emailKind) clientSpanArguments $ \mailSpan -> do
        Otel.addAttributes mailSpan $
          Attributes.buildAttrs $
            attr "app.email.kind" emailKind
              <> attr "app.email.to" (unEmail email)
        action
    clientSpanArguments = Otel.defaultSpanArguments{kind = Client}
