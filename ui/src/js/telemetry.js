import { DiagConsoleLogger, DiagLogLevel, diag, metrics } from "@opentelemetry/api";
import { SeverityNumber, logs } from "@opentelemetry/api-logs";
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
import { UserInteractionInstrumentation } from "@opentelemetry/instrumentation-user-interaction";
import { BrowserNavigationInstrumentation } from "@opentelemetry/instrumentation-browser-navigation";
import { ExceptionInstrumentation } from "@opentelemetry/instrumentation-web-exception";
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

const OTLP_ENDPOINT = import.meta.env?.ELM_LAND_OTLP_ENDPOINT;

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
// App events
// ---------------------------------------------------------------------------

// La severidad va por nombre y no como `SeverityNumber` para que los call sites
// —acá y del otro lado del port— no tengan que conocer la enumeración de OTel.
const SEVERITIES = {
  debug: SeverityNumber.DEBUG,
  info: SeverityNumber.INFO,
  warn: SeverityNumber.WARN,
  error: SeverityNumber.ERROR,
};

export function recordAppEvent(name, attributes = {}, severity = "info") {
  if (!appLogger) return; // telemetry is off

  appLogger.emit({
    severityNumber: SEVERITIES[severity] ?? SeverityNumber.INFO,
    severityText: SEVERITIES[severity] ? severity : "info",
    body: name,
    attributes: {
      ...attributes,
      "event.name": `app.${name}`,
      "app.page.route": currentRoute(),
    },
  });
}

const FEEDBACK_MAX_LENGTH = 2000;

let appLogger = null;
let logProvider = null;

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

export function createLogProvider(resource, exporter) {
  return new LoggerProvider({
    resource,
    processors: [
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

function setUpLogs(resource) {
  installLogProvider(
    createLogProvider(resource, new OTLPLogExporter({ url: `${OTLP_ENDPOINT}/v1/logs` }))
  );
}

function trackWebVitals(webVitals) {
  const record = (metric) => {
    const histogram = webVitals[metric.name];
    if (!histogram) return;
    histogram.record(metric.value, {
      "web_vitals.rating": metric.rating,
      [ATTR_URL_TEMPLATE]: currentRoute(),
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
      new BatchSpanProcessor(new OTLPTraceExporter({ url: `${OTLP_ENDPOINT}/v1/traces` }), {
        scheduledDelayMillis: BATCH_EXPORT_DELAY_MS,
      }),
    ],
  });
  tracerProvider.register();

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
      }),
      new BrowserNavigationInstrumentation({
        sanitizeUrl: scrubUrl,
      }),
      new ExceptionInstrumentation({
        applyCustomAttributes: () => ({ "app.page.route": currentRoute() }),
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
