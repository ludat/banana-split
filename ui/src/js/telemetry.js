import {
  DiagConsoleLogger,
  DiagLogLevel,
  SeverityNumber,
  context,
  diag,
  logs,
  metrics,
  propagation,
  trace,
} from "@opentelemetry/api";
import {
  CompositePropagator,
  W3CBaggagePropagator,
  W3CTraceContextPropagator,
} from "@opentelemetry/core";
import { BatchLogRecordProcessor, LoggerProvider } from "@opentelemetry/sdk-logs";
import { OTLPLogExporter } from "@opentelemetry/exporter-logs-otlp-http";
import { defaultResource, resourceFromAttributes } from "@opentelemetry/resources";
import { BatchSpanProcessor, StackContextManager, TracerProvider } from "@opentelemetry/sdk-trace";
import {
  AggregationType,
  MeterProvider,
  PeriodicExportingMetricReader,
} from "@opentelemetry/sdk-metrics";
import { OTLPTraceExporter } from "@opentelemetry/exporter-trace-otlp-http";
import {
  AggregationTemporalityPreference,
  OTLPMetricExporter,
} from "@opentelemetry/exporter-metrics-otlp-http";
import { registerInstrumentations } from "@opentelemetry/instrumentation";
import { DocumentLoadInstrumentation } from "@opentelemetry/instrumentation-document-load";
import { XMLHttpRequestInstrumentation } from "@opentelemetry/instrumentation-xml-http-request";
import { FetchInstrumentation } from "@opentelemetry/instrumentation-fetch";
import { UserInteractionInstrumentation } from "@opentelemetry/instrumentation-user-interaction";
import { BrowserNavigationInstrumentation } from "@opentelemetry/instrumentation-browser-navigation";
import { ExceptionInstrumentation } from "@opentelemetry/instrumentation-web-exception";
import {
  createDefaultSessionIdGenerator,
  createSessionLogRecordProcessor,
  createSessionManager,
  createSessionSpanProcessor,
} from "@opentelemetry/browser-sdk/session";
import {
  ATTR_DEPLOYMENT_ENVIRONMENT_NAME,
  ATTR_ERROR_TYPE,
  ATTR_HTTP_REQUEST_METHOD,
  ATTR_HTTP_RESPONSE_STATUS_CODE,
  ATTR_SERVICE_NAME,
  ATTR_SERVICE_NAMESPACE,
  ATTR_SERVICE_VERSION,
  ATTR_URL_FULL,
} from "@opentelemetry/semantic-conventions";
import { onCLS, onFCP, onINP, onLCP, onTTFB } from "web-vitals";
import Bowser from "bowser";

// In development, with no endpoint set, report to the collector on the host
// that served the page, so a phone on the network that opens
// http://192.168.x.x:1234 also sends its telemetry (localhost would be the
// phone itself). A production build still needs it set explicitly.
const OTLP_ENDPOINT =
  import.meta.env?.ELM_LAND_OTLP_ENDPOINT ||
  (import.meta.env?.PROD || !globalThis.location
    ? undefined
    : `${globalThis.location.protocol}//${globalThis.location.hostname}:4318`);

const SERVICE_NAME = "ui";
const SERVICE_NAMESPACE = "banana-split";

const PRODUCTION_HOST = "split.ludat.io";
const NAMED_ENVIRONMENTS = ["dev", "stg"];

function buildId() {
  const match = /\/index-([A-Za-z0-9_-]+)\.js/.exec(import.meta.url ?? "");
  return match ? match[1] : "dev";
}

const ATTR_URL_TEMPLATE = "url.template";

const DEVELOPMENT = !import.meta.env?.PROD;
const METRIC_EXPORT_INTERVAL_MS = DEVELOPMENT ? 500 : 30_000;
const BATCH_EXPORT_DELAY_MS = DEVELOPMENT ? 500 : 5_000;

// ---------------------------------------------------------------------------
// URL / route templating
// ---------------------------------------------------------------------------

const ULID = /^[0-9A-HJKMNP-TV-Z]{26}$/i;
const UUID = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i;
const NUMERIC = /^\d+$/;

function isIdSegment(segment) {
  return (
    ULID.test(segment) ||
    UUID.test(segment) ||
    NUMERIC.test(segment) ||
    segment.length > 20 ||
    segment.includes("@") ||
    segment.includes("%")
  );
}

export function templatePath(pathname) {
  const templated = pathname
    .split("/")
    .map((segment) => (segment && isIdSegment(segment) ? ":id" : segment))
    .join("/");
  return templated || "/";
}

export function scrubUrl(raw) {
  try {
    const url = new URL(raw, window.location.origin);
    return `${url.origin}${templatePath(url.pathname)}`;
  } catch {
    return "unparseable";
  }
}

export function scrubUrlTemplate(raw) {
  try {
    return templatePath(new URL(raw, window.location.origin).pathname);
  } catch {
    return "unparseable";
  }
}

function currentRoute() {
  return templatePath(window.location.pathname);
}

const ATTR_DISPLAY_MODE = "app.display_mode";
const STANDALONE_DISPLAY_MODES = ["window-controls-overlay", "fullscreen", "standalone", "minimal-ui"];

export function displayMode() {
  if (globalThis.navigator?.standalone === true) return "standalone";
  if (typeof window.matchMedia !== "function") return "browser";
  return (
    STANDALONE_DISPLAY_MODES.find((mode) => window.matchMedia(`(display-mode: ${mode})`).matches) ??
    "browser"
  );
}

function contextAttributes() {
  return {
    "app.page.route": currentRoute(),
    [ATTR_DISPLAY_MODE]: displayMode(),
  };
}

export function browserAttributes(nav = globalThis.navigator) {
  const userAgent = nav?.userAgent;
  const parser =
    typeof userAgent === "string" && userAgent !== ""
      ? Bowser.getParser(userAgent, nav.userAgentData ?? null)
      : null;

  return {
    "user_agent.name": parser?.getBrowserName() || "unknown",
    "os.name": parser?.getOSName() || "unknown",
    // `getPlatformType(true)` no: si no reconoce la plataforma devuelve el
    // string "undefined", y los tipos ya vienen en minúscula.
    "app.device_type": parser?.getPlatformType() || "unknown",
    "browser.language": nav?.language || "unknown",
  };
}

// ---------------------------------------------------------------------------
// Click names
// ---------------------------------------------------------------------------

const ATTR_CLICK_TARGET = "app.click.target";

// Los botones llevan `data-track` (ver `Utils.Telemetry.click` del lado Elm)
// con un nombre estable, que acá pasa al span del click. Se busca con
// `closest` porque el target del evento es lo que se tocó de verdad, que suele
// ser el ícono o el texto de adentro del botón.
export function nameClickSpan(element, span) {
  const name = element?.closest?.("[data-track]")?.getAttribute("data-track");
  if (!name) return;
  span.setAttribute(ATTR_CLICK_TARGET, name);
  span.updateName(`click ${name}`);
}

// ---------------------------------------------------------------------------
// Span scrubbing
// ---------------------------------------------------------------------------

const HTTP_DURATION_BUCKETS = [
  0.005, 0.01, 0.025, 0.05, 0.075, 0.1, 0.25, 0.5, 0.75, 1, 2.5, 5, 7.5, 10,
];

const CLS_BUCKETS = [0.01, 0.05, 0.1, 0.15, 0.25, 0.5, 1];

export class PrivacySpanProcessor {
  constructor(recordHttpDuration) {
    this._recordHttpDuration = recordHttpDuration;
  }

  onStart() {}

  onEnding(span) {
    const url = span.attributes[ATTR_URL_FULL];
    const method = span.attributes[ATTR_HTTP_REQUEST_METHOD];
    let template;
    if (typeof url === "string") {
      template = scrubUrlTemplate(url);
      span.setAttribute(ATTR_URL_FULL, scrubUrl(url));
      span.setAttribute(ATTR_URL_TEMPLATE, template);
    }

    if (typeof method === "string" && typeof template === "string") {
      span.updateName(`${method} ${template}`);
    } else if (span.name.includes("/")) {
      span.updateName(templatePath(span.name));
    }

    span.setAttributes(contextAttributes());
  }

  onEnd(span) {
    const method = span.attributes[ATTR_HTTP_REQUEST_METHOD];
    if (typeof method !== "string") return;

    // Attributes are copied only when present: an `undefined` value would
    // still create its own time series.
    const attributes = { [ATTR_HTTP_REQUEST_METHOD]: method };
    for (const key of [
      ATTR_HTTP_RESPONSE_STATUS_CODE,
      ATTR_URL_TEMPLATE,
      ATTR_ERROR_TYPE,
      ATTR_DISPLAY_MODE,
    ]) {
      const value = span.attributes[key];
      if (value !== undefined) attributes[key] = value;
    }

    const [seconds, nanos] = span.duration;
    this._recordHttpDuration(seconds + nanos / 1e9, attributes);
  }

  forceFlush() {
    return Promise.resolve();
  }

  shutdown() {
    return Promise.resolve();
  }
}

// ---------------------------------------------------------------------------
// Session
// ---------------------------------------------------------------------------

// A session ends after 30 minutes without activity or 4 hours in total,
// whichever comes first. Every span and log carries its `session.id`.
//
// The session lives only in memory: nothing is written to the browser, so a
// reload or a new tab starts a new session. Navigating inside the app does not,
// since it is a single page.
const SESSION_INACTIVITY_TIMEOUT_S = 30 * 60;
const SESSION_MAX_DURATION_S = 4 * 60 * 60;

// The manager requires a store, and the SDK's only one is localStorage.
const NO_SESSION_STORE = {
  save: () => Promise.resolve(),
  get: () => Promise.resolve(null),
};

// The manager is the `SessionProvider` the span and log processors ask for the
// id, so it has to exist before them; `startSession` runs once they are in.
export function createSession() {
  const manager = createSessionManager({
    sessionIdGenerator: createDefaultSessionIdGenerator(),
    sessionStore: NO_SESSION_STORE,
    inactivityTimeout: SESSION_INACTIVITY_TIMEOUT_S,
    maxDuration: SESSION_MAX_DURATION_S,
  });

  manager.addObserver({
    onSessionStarted(session, previous) {
      window.__telemetry.session = session.id;
      recordEvent("session.start", previous ? { "session.previous_id": previous.id } : {});
    },
    onSessionEnded() {
      recordEvent("session.end");
    },
  });

  return manager;
}

// Not `manager.start()`: all it adds is reading the store, which is empty, and
// it does so asynchronously, so a span created in between (Elm's `init` runs in
// this same tick) would start a session that `start()` then replaces. Asking
// for the id starts the session synchronously instead, and with the logger
// already installed, so `session.start` is not lost.
export function startSession(manager) {
  manager.getSessionId();
}

// ---------------------------------------------------------------------------
// Setup
// ---------------------------------------------------------------------------

function telemetryDisabledReason() {
  if (!OTLP_ENDPOINT) return "OTLP_ENDPOINT was not set when the bundle was built";

  if (import.meta.env?.PROD) {
    if (navigator.globalPrivacyControl === true) return "globalPrivacyControl is on";
  }

  return null;
}

export function deploymentEnvironment(hostname) {
  if (hostname === "localhost" || hostname === "127.0.0.1" || hostname === "[::1]") {
    return "local";
  }
  // Another device on the network opening the dev server by IP.
  if (/^(10\.|192\.168\.|172\.(1[6-9]|2\d|3[01])\.)/.test(hostname)) {
    return "local";
  }
  if (hostname === PRODUCTION_HOST) {
    return "prod";
  }
  const label = hostname.split(".")[0].toLowerCase().replace(/[^a-z0-9-]/g, "-");
  return NAMED_ENVIRONMENTS.includes(label) ? label : `review-${label}`;
}

function setUpMetrics(resource) {
  const reader = new PeriodicExportingMetricReader({
    exporter: new OTLPMetricExporter({
      url: `${OTLP_ENDPOINT}/v1/metrics`,
      temporalityPreference: AggregationTemporalityPreference.DELTA,
    }),
    exportIntervalMillis: METRIC_EXPORT_INTERVAL_MS,
  });

  const meterProvider = new MeterProvider({
    resource,
    readers: [reader],
    views: [
      {
        instrumentName: "http.client.request.duration",
        aggregation: {
          type: AggregationType.EXPLICIT_BUCKET_HISTOGRAM,
          options: { boundaries: HTTP_DURATION_BUCKETS },
        },
      },
      {
        instrumentName: "web_vitals.cls",
        aggregation: {
          type: AggregationType.EXPLICIT_BUCKET_HISTOGRAM,
          options: { boundaries: CLS_BUCKETS },
        },
      },
    ],
  });

  metrics.setGlobalMeterProvider(meterProvider);
  return meterProvider;
}

function createInstruments(meter) {
  return {
    httpDuration: meter.createHistogram("http.client.request.duration", {
      description: "Duration of HTTP requests made by the browser.",
      unit: "s",
    }),
    webVitals: {
      LCP: meter.createHistogram("web_vitals.lcp", { unit: "ms" }),
      INP: meter.createHistogram("web_vitals.inp", { unit: "ms" }),
      CLS: meter.createHistogram("web_vitals.cls", { unit: "1" }),
      FCP: meter.createHistogram("web_vitals.fcp", { unit: "ms" }),
      TTFB: meter.createHistogram("web_vitals.ttfb", { unit: "ms" }),
    },
  };
}

// ---------------------------------------------------------------------------
// Events
// ---------------------------------------------------------------------------

// La severidad va por nombre y no como `SeverityNumber` para que los call sites
// —acá y del otro lado del port— no tengan que conocer la enumeración de OTel.
const SEVERITIES = {
  debug: SeverityNumber.DEBUG,
  info: SeverityNumber.INFO,
  warn: SeverityNumber.WARN,
  error: SeverityNumber.ERROR,
};

export function recordEvent(name, attributes = {}, severity = "info") {
  if (!appLogger) return; // telemetry is off

  appLogger.emit({
    severityNumber: SEVERITIES[severity] ?? SeverityNumber.INFO,
    severityText: SEVERITIES[severity] ? severity : "info",
    body: name,
    attributes: {
      ...attributes,
      "event.name": name,
      ...contextAttributes(),
    },
  });
}

const FEEDBACK_MAX_LENGTH = 2000;

let appLogger = null;
let logProvider = null;

export function recordFeedback(message, name = "app.feedback", attributes = {}) {
  if (!appLogger) return false;
  if (typeof message !== "string") return false;

  const text = message.trim();
  if (text === "") return false;

  appLogger.emit({
    severityNumber: SeverityNumber.INFO,
    severityText: "INFO",
    body: text.slice(0, FEEDBACK_MAX_LENGTH),
    attributes: {
      ...attributes,
      "event.name": name,
      ...contextAttributes(),
    },
  });

  logProvider?.forceFlush().catch(() => {});
  return true;
}

export class PrivacyLogRecordProcessor {
  onEmit(record) {
    const url = record.attributes[ATTR_URL_FULL];
    if (typeof url === "string") {
      record.setAttribute(ATTR_URL_FULL, scrubUrl(url));
    }
  }

  shutdown() {
    return Promise.resolve();
  }

  forceFlush() {
    return Promise.resolve();
  }
}

export function createLogProvider(resource, exporter, sessionProvider = null) {
  return new LoggerProvider({
    resource,
    processors: [
      ...(sessionProvider ? [createSessionLogRecordProcessor(sessionProvider)] : []),
      new PrivacyLogRecordProcessor(),
      new BatchLogRecordProcessor({ exporter, scheduledDelayMillis: BATCH_EXPORT_DELAY_MS }),
    ],
  });
}

export function installLogProvider(provider) {
  logProvider = provider;
  appLogger = provider.getLogger(SERVICE_NAME);
  logs.setGlobalLoggerProvider(provider);
}

function setUpLogs(resource, session) {
  installLogProvider(
    createLogProvider(
      resource,
      new OTLPLogExporter({ url: `${OTLP_ENDPOINT}/v1/logs` }),
      session
    )
  );
}

function trackWebVitals(webVitals) {
  const record = (metric) => {
    const histogram = webVitals[metric.name];
    if (!histogram) return;
    histogram.record(metric.value, {
      "web_vitals.rating": metric.rating,
      [ATTR_URL_TEMPLATE]: currentRoute(),
      [ATTR_DISPLAY_MODE]: displayMode(),
    });
  };

  onLCP(record);
  onINP(record);
  onCLS(record);
  onFCP(record);
  onTTFB(record);
}

export function initTelemetry() {
  const disabledReason = telemetryDisabledReason();

  if (disabledReason) {
    console.info(`[telemetry] off: ${disabledReason}`);
    window.__telemetry = { on: false, reason: disabledReason };
    return;
  }

  console.info(`[telemetry] on, exporting to ${OTLP_ENDPOINT}`);
  window.__telemetry = {
    on: true,
    endpoint: OTLP_ENDPOINT,
    service: `${SERVICE_NAMESPACE}/${SERVICE_NAME}`,
    environment: deploymentEnvironment(window.location.hostname),
    version: buildId(),
  };

  diag.setLogger(new DiagConsoleLogger(), DiagLogLevel.WARN);

  const resource = defaultResource().merge(
    resourceFromAttributes({
      [ATTR_SERVICE_NAME]: SERVICE_NAME,
      [ATTR_SERVICE_NAMESPACE]: SERVICE_NAMESPACE,
      [ATTR_SERVICE_VERSION]: buildId(),
      [ATTR_DEPLOYMENT_ENVIRONMENT_NAME]: deploymentEnvironment(window.location.hostname),
      ...browserAttributes(),
    })
  );

  const meterProvider = setUpMetrics(resource);
  const meter = metrics.getMeter(SERVICE_NAME);
  const instruments = createInstruments(meter);
  const session = createSession();
  setUpLogs(resource, session);

  const tracerProvider = new TracerProvider({
    resource,
    spanProcessors: [
      createSessionSpanProcessor(session),
      new PrivacySpanProcessor((duration, attributes) =>
        instruments.httpDuration.record(duration, attributes)
      ),
      new BatchSpanProcessor({
        exporter: new OTLPTraceExporter({ url: `${OTLP_ENDPOINT}/v1/traces` }),
        scheduledDelayMillis: BATCH_EXPORT_DELAY_MS,
      }),
    ],
  });
  // Lo que antes hacía `WebTracerProvider.register()`. El propagator es lo que
  // manda el `traceparent` que el backend usa para colgar sus spans de los del
  // navegador.
  context.setGlobalContextManager(new StackContextManager().enable());
  propagation.setGlobalPropagator(
    new CompositePropagator({
      propagators: [new W3CTraceContextPropagator(), new W3CBaggagePropagator()],
    })
  );
  trace.setGlobalTracerProvider(tracerProvider);
  startSession(session);

  const ignoreUrls = [new RegExp(`^${OTLP_ENDPOINT.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}`)];

  registerInstrumentations({
    instrumentations: [
      new DocumentLoadInstrumentation({
        ignoreNetworkEvents: false,
      }),
      new XMLHttpRequestInstrumentation({
        ignoreUrls,
        ignoreNetworkEvents: true,
        clearTimingResources: true,
      }),
      new FetchInstrumentation({
        ignoreUrls,
        ignoreNetworkEvents: true,
      }),
      new UserInteractionInstrumentation({
        eventNames: ["click"],
        // Despite its name, this hook is also where the library lets you add
        // to the span; returning false keeps it.
        shouldPreventSpanCreation: (_eventType, element, span) => {
          nameClickSpan(element, span);
          return false;
        },
      }),
      new BrowserNavigationInstrumentation({
        sanitizeUrl: scrubUrl,
      }),
      new ExceptionInstrumentation({
        applyCustomAttributes: contextAttributes,
      }),
    ],
  });

  trackWebVitals(instruments.webVitals);

  document.addEventListener("visibilitychange", () => {
    if (document.visibilityState === "hidden") {
      meterProvider.forceFlush().catch(() => {});
    }
  });
}
