// OpenTelemetry browser instrumentation.
//
// Privacy contract: this must stay anonymous enough that no consent banner is
// required. Concretely that means:
//
//   - nothing is written to or read from the device (no cookie, no
//     localStorage, no sessionStorage, no IndexedDB), so there is nothing to
//     ask permission for under the ePrivacy "terminal equipment" rules;
//   - no session id, device id, visitor id or user id, not even an in-memory
//     one, so separate page loads cannot be stitched into a profile;
//   - no user agent string, screen size or locale, which are the usual
//     fingerprinting ingredients;
//   - no free text leaves the browser on its own: no error messages, no stack
//     traces, no request or response bodies, no headers, no URL query strings
//     and no URL fragments. The single exception is feedback the person typed
//     and pressed a button to send, which is consent in the ordinary sense;
//     see the Feedback section below for why that does not reopen the question;
//   - every URL path is templated before it is attached to anything, so ids
//     like grupo/pago ULIDs become `:id`.
//
// See ui/TELEMETRY.md for the whole story, including the collector-side
// settings that this file cannot enforce (dropping the client IP, above all).

import { DiagConsoleLogger, DiagLogLevel, diag, metrics } from "@opentelemetry/api";
import { SeverityNumber } from "@opentelemetry/api-logs";
import { BatchLogRecordProcessor, LoggerProvider } from "@opentelemetry/sdk-logs";
import { OTLPLogExporter } from "@opentelemetry/exporter-logs-otlp-http";
import { defaultResource, resourceFromAttributes } from "@opentelemetry/resources";
import { BatchSpanProcessor, WebTracerProvider } from "@opentelemetry/sdk-trace-web";
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
import {
  ATTR_DEPLOYMENT_ENVIRONMENT_NAME,
  ATTR_ERROR_TYPE,
  ATTR_HTTP_REQUEST_METHOD,
  ATTR_HTTP_RESPONSE_STATUS_CODE,
  ATTR_SERVICE_NAME,
  ATTR_SERVICE_VERSION,
  ATTR_URL_FULL,
  ATTR_USER_AGENT_ORIGINAL,
} from "@opentelemetry/semantic-conventions";
import { onCLS, onFCP, onINP, onLCP, onTTFB } from "web-vitals";

// A dónde se exporta. Se resuelve al compilar: Vite sustituye el valor en el
// bundle, así que no hay forma de cambiarlo sin rebuild. Lo declara
// elm-land.json, que es lo que hace que llegue acá con el prefijo ELM_LAND_.
// Lo setean process-compose (desarrollo) y la derivación `elm-ui` de flake.nix
// (producción) — ver ui/TELEMETRY.md.
//
// No hay default: sin endpoint no se instrumenta nada, y eso vale igual en
// producción. Un fallback hardcodeado haría que un build al que no le llegó la
// variable reportara igual, a un destino que nadie eligió, y en silencio.
//
// The `?.` on import.meta.env keeps this module importable from plain node,
// which is what the tests in tests/telemetry.test.mjs rely on.
const OTLP_ENDPOINT = import.meta.env?.ELM_LAND_OTLP_ENDPOINT;
const SERVICE_NAME = "banana-split-ui";

// Vite content-hashes the bundle, so the module's own URL already identifies
// the build exactly: /assets/index-C10GL13Y.js. That makes a decode error
// attributable to a specific frontend version — which is the whole point of
// recording them, since the usual cause is a client running an old bundle
// against a newer API. In the dev server there is no hash, hence "dev".
function buildId() {
  const match = /\/index-([A-Za-z0-9_-]+)\.js/.exec(import.meta.url ?? "");
  return match ? match[1] : "dev";
}

// `url.template` is still incubating in the semantic conventions, so importing
// it would pull in the whole incubating bundle for one string.
const ATTR_URL_TEMPLATE = "url.template";

const METRIC_EXPORT_INTERVAL_MS = 30_000;

// ---------------------------------------------------------------------------
// URL / route templating
// ---------------------------------------------------------------------------

const ULID = /^[0-9A-HJKMNP-TV-Z]{26}$/i;
const UUID = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i;
const NUMERIC = /^\d+$/;

// Anything that could carry an identifier becomes `:id`. The checks are
// deliberately greedy: a segment we fail to recognise as a known route word is
// better masked than leaked, and the app has no routes with free-text
// segments.
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

// Keeps the origin (useful to tell our API from a third party) and the
// templated path. Query string and fragment are dropped entirely.
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

// ---------------------------------------------------------------------------
// Span scrubbing
// ---------------------------------------------------------------------------

// Attributes that some instrumentation sets and we never want to send.
// `user_agent.original` is set by the document-load instrumentation and is the
// single biggest fingerprinting vector in the default setup.
const FORBIDDEN_SPAN_ATTRIBUTES = [ATTR_USER_AGENT_ORIGINAL];

const HTTP_DURATION_BUCKETS = [
  0.005, 0.01, 0.025, 0.05, 0.075, 0.1, 0.25, 0.5, 0.75, 1, 2.5, 5, 7.5, 10,
];

const CLS_BUCKETS = [0.01, 0.05, 0.1, 0.15, 0.25, 0.5, 1];

/**
 * A span processor that does two jobs, in this order:
 *
 *   - `onEnding` runs while the span is still writable and is where every span
 *     gets scrubbed. Doing it here rather than in each instrumentation's
 *     `applyCustomAttributesOnSpan` hook means a future instrumentation cannot
 *     leak a raw URL by being added without a hook.
 *   - `onEnd` derives the HTTP client duration metric from the (already
 *     scrubbed) span, so there is exactly one place deciding what an API call
 *     looks like in telemetry.
 *
 * The SDK calls `onEnding` on every processor before calling `onEnd` on any of
 * them, so the batch processor always sees scrubbed spans.
 */
export class PrivacySpanProcessor {
  constructor(recordHttpDuration) {
    this._recordHttpDuration = recordHttpDuration;
  }

  onStart() {}

  onEnding(span) {
    for (const attribute of FORBIDDEN_SPAN_ATTRIBUTES) {
      delete span.attributes[attribute];
    }

    const url = span.attributes[ATTR_URL_FULL];
    if (typeof url === "string") {
      span.setAttribute(ATTR_URL_FULL, scrubUrl(url));
      span.setAttribute(ATTR_URL_TEMPLATE, scrubUrlTemplate(url));
    }

    // Span names from the instrumentation we enable are already safe ("GET",
    // "documentLoad", ...). This guards against a name that is a URL path.
    if (span.name.includes("/")) {
      span.updateName(templatePath(span.name));
    }

    // Which page the request was made from. Bounded cardinality, no identity.
    span.setAttribute("app.page.route", currentRoute());
  }

  onEnd(span) {
    const method = span.attributes[ATTR_HTTP_REQUEST_METHOD];
    if (typeof method !== "string") return;

    // Attributes are copied only when present: an `undefined` value would
    // still create its own time series.
    const attributes = { [ATTR_HTTP_REQUEST_METHOD]: method };
    for (const key of [ATTR_HTTP_RESPONSE_STATUS_CODE, ATTR_URL_TEMPLATE, ATTR_ERROR_TYPE]) {
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
// Setup
// ---------------------------------------------------------------------------

// Returns null when telemetry should run, or the reason it should not. The
// reason matters: every path here fails silently and closed, which is right in
// production but impossible to debug locally, so the caller logs it in dev.
function telemetryDisabledReason() {
  // Sin lugar a dónde exportar no hay nada que hacer, y esto va primero porque
  // vale en los dos ambientes: un `pnpm start` suelto se queda callado en lugar
  // de llenar la consola de exports fallidos, y un build de producción al que
  // no le llegó la variable no reporta en vez de reportar a cualquier lado.
  if (!OTLP_ENDPOINT) return "OTLP_ENDPOINT was not set when the bundle was built";

  // Respect the browser-level signal even though we believe we do not need
  // consent: alguien que lo manda está pidiendo que no se lo mida. Solo en
  // producción, igual — localmente la data va a tu propia máquina, y que tu
  // propia preferencia te esconda tus propias trazas solo genera confusión.
  //
  // Solo GPC. `navigator.doNotTrack` está deprecado: lo saque la especificación,
  // Safari lo removió hace años y Firefox le sacó la UI en la 135, así que no
  // quedaba quién lo mandara.
  if (import.meta.env?.PROD) {
    if (navigator.globalPrivacyControl === true) return "globalPrivacyControl is on";
  }

  return null;
}

function setUpMetrics(resource) {
  const reader = new PeriodicExportingMetricReader({
    exporter: new OTLPMetricExporter({
      url: `${OTLP_ENDPOINT}/v1/metrics`,
      // A browser counter restarts on every page load, so cumulative values
      // are meaningless here.
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
    pageViews: meter.createCounter("app.page.views", {
      description: "Page views, counted per templated route.",
      unit: "1",
    }),
    httpDuration: meter.createHistogram("http.client.request.duration", {
      description: "Duration of HTTP requests made by the browser.",
      unit: "s",
    }),
    errors: meter.createCounter("app.errors", {
      description: "Uncaught JavaScript errors and unhandled rejections.",
      unit: "1",
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
// App events
// ---------------------------------------------------------------------------

// The closed vocabulary of events the app may report. This is what keeps the
// privacy contract true over time instead of by discipline: an attribute key
// that is not declared here is dropped, and a value outside its declared set
// takes the whole event down with it. There is no path through which a group
// name, an amount or an id can reach the exporter, however a future call site
// is written.
//
// Value specs: an array is a closed set of allowed values; "route" means the
// value is a URL path and gets templated; "identifier" means a bare code
// identifier (an operation name from the generated API client), which cannot
// carry prose.
export const APP_EVENTS = {
  // Which share mechanism the browser actually used. Unknowable from the
  // backend, and the only way to tell whether the feature works on the
  // browsers people really use.
  share: {
    severity: SeverityNumber.INFO,
    description: "Share attempts by the mechanism that handled them.",
    attributes: { outcome: ["native", "clipboard", "dismissed", "unsupported", "failed"] },
  },
  // The backend answered 200 and the frontend could not decode it. Nobody sees
  // this today: the server logged a success. Usually means a client on an old
  // bundle against a newer API, which is why service.version carries the build.
  "api.decode_error": {
    severity: SeverityNumber.WARN,
    description: "API responses the frontend could not decode.",
    attributes: { "app.api.operation": "identifier" },
  },
  // Someone filled part of the gasto form and closed it without saving. The
  // backend only ever sees the gastos that got saved, so this is the only
  // place the abandoned ones show up — and which step they gave up on is the
  // actionable part.
  "gasto.edit_abandoned": {
    severity: SeverityNumber.INFO,
    description: "The gasto form was closed with unsaved changes.",
    attributes: {
      mode: ["nuevo", "existente"],
      section: ["basico", "pagadores", "deudores"],
    },
  },
};

const IDENTIFIER = /^[A-Za-z][A-Za-z0-9_]{0,63}$/;

export function validateEventAttributes(spec, input) {
  const attributes = {};
  for (const [key, valueSpec] of Object.entries(spec)) {
    const value = input[key];
    if (valueSpec === "route") {
      if (typeof value !== "string") return null;
      attributes[key] = templatePath(value);
    } else if (valueSpec === "identifier") {
      if (typeof value !== "string" || !IDENTIFIER.test(value)) return null;
      attributes[key] = value;
    } else if (Array.isArray(valueSpec)) {
      if (!valueSpec.includes(value)) return null;
      attributes[key] = value;
    }
  }
  return attributes;
}

/**
 * Report one app event. Unknown names and attributes that do not match the
 * declared vocabulary are dropped rather than sent; in development the reason
 * is logged, because a silently ignored event is worse than no event.
 */
export function recordAppEvent(name, input = {}) {
  const event = APP_EVENTS[name];
  if (!event) {
    if (!import.meta.env?.PROD) console.warn(`[telemetry] unknown app event: ${name}`);
    return;
  }

  if (!appLogger) return; // telemetry is off

  const attributes = validateEventAttributes(event.attributes, input);
  if (!attributes) {
    if (!import.meta.env?.PROD) {
      console.warn(`[telemetry] dropped ${name}: attributes outside the declared vocabulary`);
    }
    return;
  }

  appLogger.emit({
    severityNumber: event.severity,
    // The body is the event name, never prose: the detail lives in the
    // validated attributes, which is what keeps these records safe.
    body: name,
    attributes: {
      ...attributes,
      "event.name": `app.${name}`,
      "app.page.route": currentRoute(),
    },
  });
}

// ---------------------------------------------------------------------------
// Logs: app events and feedback
// ---------------------------------------------------------------------------

// Two things emit log records, and only two: the app events declared above,
// and user feedback.
//
// The dividing line against metrics is volume and what you want to ask. A page
// view or a web vital is a rate, so it is a metric. A share or a decode error
// is a discrete occurrence you want to look at one by one, and at this app's
// volume Loki can count them anyway with count_over_time, while a counter can
// never be un-aggregated back into occurrences.
//
// App event records carry no free text: the body is the event name and the
// attributes went through the vocabulary above. Feedback is the one deliberate
// exception to "no free text leaves the browser", and does not reopen the
// consent question because:
//
//   - the person wrote it and pressed a button to send it, which is consent in
//     the ordinary sense of the word, not passive collection. The form says so
//     next to the button, which is what keeps the no-banner story intact;
//   - no identifier is attached, so it stays a one-way channel: there is
//     nothing to reply to and nothing to join it against;
//   - it will contain third-party personal data anyway (the names of the
//     people in someone's group), so the Loki stream it lands in wants shorter
//     retention than the rest. That is a collector-side setting, documented in
//     ui/TELEMETRY.md.
//
// Do not add a general-purpose logging facility on top of this. One available
// to the whole codebase is how free text starts leaking.
const FEEDBACK_MAX_LENGTH = 2000;

let appLogger = null;
let logProvider = null;

/**
 * Send one piece of user-written feedback. Returns whether it was accepted, so
 * the UI can tell the difference between "sent" and "telemetry is off".
 */
export function recordFeedback(message) {
  if (!appLogger) return false;
  if (typeof message !== "string") return false;

  const text = message.trim();
  if (text === "") return false;

  appLogger.emit({
    severityNumber: SeverityNumber.INFO,
    severityText: "INFO",
    body: text.slice(0, FEEDBACK_MAX_LENGTH),
    attributes: {
      "event.name": "app.feedback",
      "app.page.route": currentRoute(),
    },
  });

  // Feedback is worth a round trip of its own: batching it would lose it if
  // the person closes the tab right after sending.
  logProvider?.forceFlush().catch(() => {});
  return true;
}

/** Exported so the wiring itself is covered by tests, with an in-memory
 * exporter standing in for the OTLP one. */
export function createLogProvider(resource, exporter) {
  return new LoggerProvider({
    resource,
    // Note the options object: BatchLogRecordProcessor takes `{ exporter }`,
    // unlike BatchSpanProcessor, which takes the exporter positionally.
    processors: [new BatchLogRecordProcessor({ exporter })],
  });
}

export function installLogProvider(provider) {
  logProvider = provider;
  appLogger = provider.getLogger(SERVICE_NAME);
}

function setUpLogs(resource) {
  installLogProvider(
    createLogProvider(resource, new OTLPLogExporter({ url: `${OTLP_ENDPOINT}/v1/logs` }))
  );
}

function trackPageViews(pageViews) {
  let lastRoute = null;

  const record = () => {
    const route = currentRoute();
    // elm-land replaces the history entry on things that are not navigations,
    // so only count an actual route change.
    if (route === lastRoute) return;
    lastRoute = route;
    pageViews.add(1, { [ATTR_URL_TEMPLATE]: route });
  };

  // elm-land routes on the client, so there is no document load to hook into
  // after the first page. Wrapping the history methods catches every
  // navigation without the Elm side having to know telemetry exists.
  for (const method of ["pushState", "replaceState"]) {
    const original = history[method];
    history[method] = function (...args) {
      const result = original.apply(this, args);
      record();
      return result;
    };
  }
  window.addEventListener("popstate", record);

  record();
}

function trackWebVitals(webVitals) {
  const record = (metric) => {
    const histogram = webVitals[metric.name];
    if (!histogram) return;
    histogram.record(metric.value, {
      "web_vitals.rating": metric.rating,
      // The route as of when the vital was reported. For LCP/FCP/TTFB that is
      // the landing route; for INP/CLS it is wherever the user was.
      [ATTR_URL_TEMPLATE]: currentRoute(),
    });
  };

  onLCP(record);
  onINP(record);
  onCLS(record);
  onFCP(record);
  onTTFB(record);
}

function trackErrors(errors) {
  // Only the error *type* is recorded. Messages and stacks can contain group
  // names, amounts or anything else the user typed, so they never leave.
  window.addEventListener("error", (event) => {
    errors.add(1, {
      [ATTR_ERROR_TYPE]: event.error?.name ?? "Error",
      "app.page.route": currentRoute(),
    });
  });

  window.addEventListener("unhandledrejection", (event) => {
    errors.add(1, {
      [ATTR_ERROR_TYPE]: event.reason?.name ?? "UnhandledRejection",
      "app.page.route": currentRoute(),
    });
  });
}

export function initTelemetry() {
  const disabledReason = telemetryDisabledReason();

  // El estado se reporta SIEMPRE, también en producción. Antes esto estaba
  // detrás de `!PROD` y el resultado era que desde el browser no había ninguna
  // forma de saber si la telemetría estaba activa: ni log, ni nada que mirar.
  // Una línea no es ruido, y nada de esto sale del dispositivo.
  //
  // Va además a `window.__telemetry` porque el log se pierde: se limpia la
  // consola, o te enganchás con la página abierta desde hace una hora. Son
  // strings y un booleano, nada que dependa del resto del módulo.
  if (disabledReason) {
    console.info(`[telemetry] off: ${disabledReason}`);
    window.__telemetry = { on: false, reason: disabledReason };
    return;
  }

  console.info(`[telemetry] on, exporting to ${OTLP_ENDPOINT}`);
  window.__telemetry = { on: true, endpoint: OTLP_ENDPOINT, version: buildId() };

  // Without this the SDK swallows export failures entirely, so a collector that
  // is down or rejecting looks exactly like one that is working. Va prendido en
  // todos los ambientes: solo habla cuando algo falla, y el único costo es ruido
  // en una consola que los usuarios no abren — mucho más barato que no poder
  // distinguir "no se manda" de "se manda y se pierde".
  diag.setLogger(new DiagConsoleLogger(), DiagLogLevel.WARN);

  const resource = defaultResource().merge(
    resourceFromAttributes({
      [ATTR_SERVICE_NAME]: SERVICE_NAME,
      [ATTR_SERVICE_VERSION]: buildId(),
      [ATTR_DEPLOYMENT_ENVIRONMENT_NAME]: import.meta.env?.PROD ? "production" : "development",
    })
  );

  const meterProvider = setUpMetrics(resource);
  const meter = metrics.getMeter(SERVICE_NAME);
  const instruments = createInstruments(meter);
  setUpLogs(resource);

  const tracerProvider = new WebTracerProvider({
    resource,
    spanProcessors: [
      new PrivacySpanProcessor((duration, attributes) =>
        instruments.httpDuration.record(duration, attributes)
      ),
      new BatchSpanProcessor(
        new OTLPTraceExporter({ url: `${OTLP_ENDPOINT}/v1/traces` })
      ),
    ],
  });
  tracerProvider.register();

  // Our own exports go out over fetch; tracing them would feed itself.
  const ignoreUrls = [new RegExp(`^${OTLP_ENDPOINT.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}`)];

  registerInstrumentations({
    instrumentations: [
      new DocumentLoadInstrumentation({
        // The navigation timing events are the whole point of this one.
        ignoreNetworkEvents: false,
      }),
      new XMLHttpRequestInstrumentation({
        // Elm's Http module goes through XMLHttpRequest, so this covers the
        // entire generated API client.
        ignoreUrls,
        ignoreNetworkEvents: true,
        clearTimingResources: true,
      }),
      new FetchInstrumentation({
        ignoreUrls,
        ignoreNetworkEvents: true,
      }),
    ],
  });

  trackPageViews(instruments.pageViews);
  trackWebVitals(instruments.webVitals);
  trackErrors(instruments.errors);

  // The batch span processor flushes itself when the document is hidden; the
  // metric reader does not, and a visit shorter than the export interval would
  // otherwise report nothing.
  //
  // This listener MUST stay registered after trackWebVitals() above: LCP, INP
  // and CLS are only reported by web-vitals once the page is hidden, from their
  // own visibilitychange listener. Listeners fire in registration order, so
  // theirs records the value and ours then flushes it. Register this earlier and
  // those three vitals miss the flush and are lost with the page.
  document.addEventListener("visibilitychange", () => {
    if (document.visibilityState === "hidden") {
      meterProvider.forceFlush().catch(() => {});
    }
  });
}

// Deliberately NOT instrumented:
//
//   - @opentelemetry/instrumentation-user-interaction: it names spans after the
//     DOM target of every click, which pulls element ids and text into
//     telemetry, and it is the main thing that would turn this into behavioural
//     tracking.
//   - any session or visitor id (see the file header).
//   - @opentelemetry/context-zone: the Elm app does not nest async work inside
//     our spans, so the default StackContextManager is enough and zone.js stays
//     out of the bundle.
