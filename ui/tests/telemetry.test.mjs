// Tests for the privacy guarantees of src/js/telemetry.js.
//
// Run with `pnpm test:js`. These run in plain node (no DOM), so they cover the
// scrubbing and the metric pipeline — the parts where a mistake leaks data —
// rather than the browser instrumentation itself.

import assert from "node:assert/strict";
import test from "node:test";

import { trace } from "@opentelemetry/api";
import {
  InMemorySpanExporter,
  SimpleSpanProcessor,
  BasicTracerProvider,
} from "@opentelemetry/sdk-trace-base";
import {
  ATTR_HTTP_REQUEST_METHOD,
  ATTR_HTTP_RESPONSE_STATUS_CODE,
  ATTR_URL_FULL,
} from "@opentelemetry/semantic-conventions";

import { SeverityNumber } from "@opentelemetry/api-logs";
import { InMemoryLogRecordExporter } from "@opentelemetry/sdk-logs";
import { resourceFromAttributes } from "@opentelemetry/resources";

import {
  createLogProvider,
  deploymentEnvironment,
  installLogProvider,
  recordAppEvent,
  recordFeedback,
  PrivacySpanProcessor,
  scrubUrl,
  scrubUrlTemplate,
  templatePath,
} from "../src/js/telemetry.js";

// `currentRoute()` reads window.location; the module only touches it inside
// functions, so a stub is enough.
globalThis.window = { location: { origin: "https://split.ludat.io", pathname: "/grupos/01JQZ3K9XY4T5V6W7X8Y9Z0ABC" } };

const ULID = "01JQZ3K9XY4T5V6W7X8Y9Z0ABC";

test("templatePath masks ULIDs, uuids and numbers", () => {
  assert.equal(templatePath(`/grupos/${ULID}/gastos`), "/grupos/:id/gastos");
  assert.equal(
    templatePath(`/grupos/${ULID}/pagos/${ULID}`),
    "/grupos/:id/pagos/:id"
  );
  assert.equal(
    templatePath("/grupos/3f2504e0-4f89-11d3-9a0c-0305e82c3301"),
    "/grupos/:id"
  );
  assert.equal(templatePath("/grupos/12345"), "/grupos/:id");
  assert.equal(templatePath("/api/grupo/" + ULID + "/participantes"), "/api/grupo/:id/participantes");
});

test("templatePath keeps real route words", () => {
  assert.equal(templatePath("/"), "/");
  assert.equal(templatePath("/login"), "/login");
  assert.equal(templatePath("/cuenta"), "/cuenta");
  assert.equal(
    templatePath(`/grupos/${ULID}/transferencias`),
    "/grupos/:id/transferencias"
  );
  assert.equal(templatePath("/api/health"), "/api/health");
});

test("templatePath masks anything long or encoded, as a backstop", () => {
  assert.equal(templatePath("/g/lucas%40gmail.com"), "/g/:id");
  assert.equal(templatePath("/g/" + "a".repeat(21)), "/g/:id");
});

// El ambiente sale de la URL y no de una variable de build porque el bundle es
// uno solo para todos los ambientes: algo horneado al compilar diría
// "production" también en un review app.
test("deploymentEnvironment derives the environment from the hostname", () => {
  assert.equal(deploymentEnvironment("split.ludat.io"), "prod");
  assert.equal(deploymentEnvironment("localhost"), "local");
  assert.equal(deploymentEnvironment("127.0.0.1"), "local");
});

// El prefijo es lo que deja agrupar o descartar todos los reviews de una sin
// saber qué ramas existen.
test("deploymentEnvironment prefixes review apps", () => {
  assert.equal(deploymentEnvironment("lele.split.ludat.io"), "review-lele");
});

// `dev` y `stg` son ambientes con nombre propio, no reviews.
test("deploymentEnvironment leaves the named environments alone", () => {
  assert.equal(deploymentEnvironment("dev.split.ludat.io"), "dev");
  assert.equal(deploymentEnvironment("stg.split.ludat.io"), "stg");
});

test("deploymentEnvironment keeps the environment usable as a label", () => {
  assert.equal(deploymentEnvironment("Lele_ABC.split.ludat.io"), "review-lele-abc");
});

test("scrubUrl drops query strings and fragments", () => {
  assert.equal(
    scrubUrl(`https://split.ludat.io/api/grupo/${ULID}?token=secreto#nombre`),
    "https://split.ludat.io/api/grupo/:id"
  );
  assert.equal(scrubUrlTemplate(`/api/grupo/${ULID}?token=secreto`), "/api/grupo/:id");
});

test("scrubUrl keeps the origin so third parties stay distinguishable", () => {
  assert.equal(scrubUrl("https://otra.cosa/x/99"), "https://otra.cosa/x/:id");
});

test("scrubUrl never throws on junk", () => {
  // A URL that not even a base can rescue.
  assert.equal(scrubUrl("http://["), "unparseable");
  assert.equal(scrubUrlTemplate("http://["), "unparseable");
  assert.equal(scrubUrlTemplate(""), "/");
});

// --- the span processor ----------------------------------------------------

function tracerWith(recordHttpDuration = () => {}) {
  const exporter = new InMemorySpanExporter();
  const provider = new BasicTracerProvider({
    spanProcessors: [
      new PrivacySpanProcessor(recordHttpDuration),
      new SimpleSpanProcessor(exporter),
    ],
  });
  return { tracer: provider.getTracer("test"), exporter };
}

test("the exported span has no raw url", () => {
  const { tracer, exporter } = tracerWith();

  const span = tracer.startSpan("GET");
  span.setAttribute(ATTR_URL_FULL, `https://split.ludat.io/api/grupo/${ULID}?token=secreto`);
  span.setAttribute(ATTR_HTTP_REQUEST_METHOD, "GET");
  span.end();

  const [exported] = exporter.getFinishedSpans();
  assert.equal(
    exported.attributes[ATTR_URL_FULL],
    "https://split.ludat.io/api/grupo/:id"
  );
  assert.equal(exported.attributes["url.template"], "/api/grupo/:id");

  // Nothing anywhere in the payload still carries the id or the token.
  const serialized = JSON.stringify(exported.attributes);
  assert.ok(!serialized.includes(ULID), serialized);
  assert.ok(!serialized.includes("secreto"), serialized);
});

test("the page route is attached and is itself templated", () => {
  const { tracer, exporter } = tracerWith();
  tracer.startSpan("documentLoad").end();

  const [exported] = exporter.getFinishedSpans();
  assert.equal(exported.attributes["app.page.route"], "/grupos/:id");
});

test("a span named like a url path gets templated too", () => {
  const { tracer, exporter } = tracerWith();
  tracer.startSpan(`/grupos/${ULID}/gastos`).end();

  assert.equal(exporter.getFinishedSpans()[0].name, "/grupos/:id/gastos");
});

// The instrumentations name every request span "GET", so without this a trace is
// a list of indistinguishable "GET"s.
test("a request span is named after its method and template", () => {
  const { tracer, exporter } = tracerWith();

  const span = tracer.startSpan("GET");
  span.setAttribute(ATTR_HTTP_REQUEST_METHOD, "GET");
  span.setAttribute(ATTR_URL_FULL, `https://split.ludat.io/api/grupo/${ULID}/resumen`);
  span.end();

  const [exported] = exporter.getFinishedSpans();
  assert.equal(exported.name, "GET /api/grupo/:id/resumen");
  // The id is what makes a name unbounded, and it is masked in the name too.
  assert.ok(!exported.name.includes(ULID), exported.name);
});

test("a request span name carries no query string", () => {
  const { tracer, exporter } = tracerWith();

  const span = tracer.startSpan("POST");
  span.setAttribute(ATTR_HTTP_REQUEST_METHOD, "POST");
  span.setAttribute(ATTR_URL_FULL, "https://split.ludat.io/api/login?token=secreto");
  span.end();

  const [exported] = exporter.getFinishedSpans();
  assert.equal(exported.name, "POST /api/login");
});

// Un span de click trae la URL de la página, que es la que lleva el ULID del
// grupo. Es el único dato sensible que agrega la instrumentación de
// interacciones: el resto es la estructura del DOM.
test("a click span keeps its name and gets its page url scrubbed", () => {
  const { tracer, exporter } = tracerWith();

  const span = tracer.startSpan("click");
  span.setAttribute("event_type", "click");
  span.setAttribute("target_element", "BUTTON");
  span.setAttribute("target_xpath", "/html/body/div[2]/button[1]");
  span.setAttribute(ATTR_URL_FULL, `https://split.ludat.io/grupos/${ULID}/gastos`);
  span.end();

  const [exported] = exporter.getFinishedSpans();
  assert.equal(exported.name, "click");
  assert.equal(exported.attributes[ATTR_URL_FULL], "https://split.ludat.io/grupos/:id/gastos");
  assert.ok(!JSON.stringify(exported.attributes).includes(ULID));
});

test("a span with a method but no url keeps its name", () => {
  const { tracer, exporter } = tracerWith();

  const span = tracer.startSpan("GET");
  span.setAttribute(ATTR_HTTP_REQUEST_METHOD, "GET");
  span.end();

  assert.equal(exporter.getFinishedSpans()[0].name, "GET");
});

test("http client spans produce a duration metric with scrubbed attributes", () => {
  const recorded = [];
  const { tracer } = tracerWith((duration, attributes) =>
    recorded.push({ duration, attributes })
  );

  const span = tracer.startSpan("GET");
  span.setAttribute(ATTR_HTTP_REQUEST_METHOD, "GET");
  span.setAttribute(ATTR_HTTP_RESPONSE_STATUS_CODE, 200);
  span.setAttribute(ATTR_URL_FULL, `https://split.ludat.io/api/grupo/${ULID}`);
  span.end();

  assert.equal(recorded.length, 1);
  assert.equal(recorded[0].attributes[ATTR_URL_FULL], undefined);
  assert.equal(recorded[0].attributes["url.template"], "/api/grupo/:id");
  assert.equal(recorded[0].attributes[ATTR_HTTP_RESPONSE_STATUS_CODE], 200);
  assert.ok(recorded[0].duration >= 0 && recorded[0].duration < 1);

  // An absent attribute must be absent, not present-and-undefined, or it
  // becomes its own time series.
  assert.ok(!("error.type" in recorded[0].attributes));
});

test("non-http spans do not produce a duration metric", () => {
  const recorded = [];
  const { tracer } = tracerWith((d, a) => recorded.push({ d, a }));
  tracer.startSpan("documentLoad").end();
  assert.equal(recorded.length, 0);
});

test("trace api is untouched by the processor", () => {
  assert.equal(typeof trace.getTracer, "function");
});

// --- app events and feedback as log records ----------------------------------

// Exercises the real wiring from telemetry.js, not a copy of it: passing the
// exporter positionally used to leave the processor with no exporter at all,
// and the only symptom was a TypeError in the browser console.
function logsWith() {
  const exporter = new InMemoryLogRecordExporter();
  const provider = createLogProvider(
    resourceFromAttributes({ "service.name": "test" }),
    exporter
  );
  installLogProvider(provider);
  // The processor batches, so the test has to wait for the flush that
  // recordFeedback fires off.
  return { exporter, provider, flushed: () => provider.forceFlush() };
}

test("feedback is emitted as a log record with the text intact", async () => {
  const { exporter, flushed } = logsWith();

  assert.equal(recordFeedback("  el boton de saldar no se ve en el celular  "), true);
  await flushed();

  const [record] = exporter.getFinishedLogRecords();
  assert.equal(record.body, "el boton de saldar no se ve en el celular");
  assert.equal(record.attributes["event.name"], "app.feedback");
  assert.equal(record.attributes["app.page.route"], "/grupos/:id");
});

// Las instrumentaciones event-based (navigation, web-exception) emiten log
// records, así que el PrivacySpanProcessor no las ve. Lo que las cubre es
// PrivacyLogRecordProcessor, y está cableado adentro de createLogProvider, que es
// lo que estos dos tests ejercitan.
test("a log record with a page url gets it templated", async () => {
  const { exporter, provider, flushed } = logsWith();

  provider.getLogger("test").emit({
    eventName: "browser.navigation",
    attributes: {
      [ATTR_URL_FULL]: `https://split.ludat.io/grupos/${ULID}/gastos?x=y`,
      "browser.navigation.type": "push",
    },
  });
  await flushed();

  const [record] = exporter.getFinishedLogRecords();
  assert.equal(record.attributes[ATTR_URL_FULL], "https://split.ludat.io/grupos/:id/gastos");
  assert.equal(record.attributes["browser.navigation.type"], "push");
});

// Decisión explícita: de una excepción salen el mensaje y el stack enteros. Si
// alguna vez se vuelve atrás, este test es el que hay que invertir.
test("an exception record keeps its message and stacktrace", async () => {
  const { exporter, provider, flushed } = logsWith();

  provider.getLogger("test").emit({
    eventName: "exception",
    attributes: {
      "exception.type": "TypeError",
      "exception.message": "no se pudo guardar el gasto de Asado",
      "exception.stacktrace": "at Grupos.update (Main.elm:120)",
    },
  });
  await flushed();

  const [record] = exporter.getFinishedLogRecords();
  assert.equal(record.attributes["exception.type"], "TypeError");
  assert.equal(record.attributes["exception.message"], "no se pudo guardar el gasto de Asado");
  assert.equal(record.attributes["exception.stacktrace"], "at Grupos.update (Main.elm:120)");
});

test("feedback carries no identifier beyond the route", async () => {
  const { exporter, flushed } = logsWith();
  recordFeedback("hola");
  await flushed();

  const [record] = exporter.getFinishedLogRecords();
  assert.deepEqual(Object.keys(record.attributes).sort(), ["app.page.route", "event.name"]);
});

test("empty feedback is not sent", async () => {
  const { exporter, flushed } = logsWith();

  assert.equal(recordFeedback("   "), false);
  assert.equal(recordFeedback(""), false);
  assert.equal(recordFeedback(null), false);
  await flushed();
  assert.equal(exporter.getFinishedLogRecords().length, 0);
});

test("feedback is capped so it cannot be used to ship a payload", async () => {
  const { exporter, flushed } = logsWith();
  recordFeedback("a".repeat(5000));
  await flushed();

  assert.equal(exporter.getFinishedLogRecords()[0].body.length, 2000);
});

test("an app event is emitted as a log record with no free text", async () => {
  const { exporter, flushed } = logsWith();

  recordAppEvent("share", { outcome: "clipboard" });
  await flushed();

  const [record] = exporter.getFinishedLogRecords();
  assert.equal(record.body, "share");
  assert.equal(record.attributes["event.name"], "app.share");
  assert.equal(record.attributes.outcome, "clipboard");
  assert.equal(record.attributes["app.page.route"], "/grupos/:id");
});

test("a decode error names the operation and nothing else", async () => {
  const { exporter, flushed } = logsWith();

  recordAppEvent("api.decode_error", { "app.api.operation": "getGrupoByIdPagos" });
  await flushed();

  const [record] = exporter.getFinishedLogRecords();
  assert.equal(record.attributes["app.api.operation"], "getGrupoByIdPagos");
  assert.deepEqual(Object.keys(record.attributes).sort(), [
    "app.api.operation",
    "app.page.route",
    "event.name",
  ]);
});

// Los atributos pasan tal cual: no hay vocabulario que los filtre. Lo que los
// acota es el lado Elm, donde `Section` es un custom type y los nombres de
// operación son literales.
test("the severity travels by name and defaults to info", async () => {
  const { exporter, flushed } = logsWith();

  recordAppEvent("api.decode_error", {}, "error");
  recordAppEvent("share", {});
  // Una severidad que el mapa no conoce no tira el evento: cae en info.
  recordAppEvent("share", {}, "inventada");
  await flushed();

  const [unError, sinSeveridad, desconocida] = exporter.getFinishedLogRecords();
  assert.equal(unError.severityText, "error");
  assert.equal(unError.severityNumber, SeverityNumber.ERROR);
  assert.equal(sinSeveridad.severityText, "info");
  assert.equal(desconocida.severityText, "info");
  assert.equal(desconocida.severityNumber, SeverityNumber.INFO);
});

test("the attributes are forwarded as given", async () => {
  const { exporter, flushed } = logsWith();

  recordAppEvent("share", { outcome: "native", cualquiera: "cosa" });
  await flushed();

  const [record] = exporter.getFinishedLogRecords();
  assert.equal(record.attributes.outcome, "native");
  assert.equal(record.attributes.cualquiera, "cosa");
});

test("an abandoned gasto edit records the mode and the step", async () => {
  const { exporter, flushed } = logsWith();

  recordAppEvent("gasto.edit_abandoned", { mode: "nuevo", section: "deudores" });
  await flushed();

  const [record] = exporter.getFinishedLogRecords();
  assert.equal(record.attributes["event.name"], "app.gasto.edit_abandoned");
  assert.equal(record.attributes.mode, "nuevo");
  assert.equal(record.attributes.section, "deudores");
});

