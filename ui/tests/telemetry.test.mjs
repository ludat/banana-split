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
  ATTR_USER_AGENT_ORIGINAL,
} from "@opentelemetry/semantic-conventions";

import { InMemoryLogRecordExporter } from "@opentelemetry/sdk-logs";
import { resourceFromAttributes } from "@opentelemetry/resources";

import {
  APP_EVENTS,
  createLogProvider,
  installLogProvider,
  recordAppEvent,
  recordFeedback,
  PrivacySpanProcessor,
  scrubUrl,
  scrubUrlTemplate,
  templatePath,
  validateEventAttributes,
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

test("the exported span has no user agent and no raw url", () => {
  const { tracer, exporter } = tracerWith();

  const span = tracer.startSpan("GET");
  span.setAttribute(ATTR_USER_AGENT_ORIGINAL, "Mozilla/5.0 (very identifying)");
  span.setAttribute(ATTR_URL_FULL, `https://split.ludat.io/api/grupo/${ULID}?token=secreto`);
  span.setAttribute(ATTR_HTTP_REQUEST_METHOD, "GET");
  span.end();

  const [exported] = exporter.getFinishedSpans();
  assert.equal(exported.attributes[ATTR_USER_AGENT_ORIGINAL], undefined);
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

// --- the app event vocabulary -----------------------------------------------

test("undeclared attribute keys are dropped, not forwarded", () => {
  const attributes = validateEventAttributes(APP_EVENTS.share.attributes, {
    outcome: "native",
    // Everything a careless call site might pass along:
    grupoNombre: "Viaje a Bariloche",
    monto: "15000",
    participanteId: "01JQZ3K9XY4T5V6W7X8Y9Z0ABC",
  });

  assert.deepEqual(attributes, { outcome: "native" });
});

test("a value outside the declared set takes the whole event down", () => {
  assert.equal(
    validateEventAttributes(APP_EVENTS.share.attributes, { outcome: "whatever" }),
    null
  );
  assert.equal(validateEventAttributes(APP_EVENTS.share.attributes, {}), null);
});

test("every declared share outcome is accepted", () => {
  for (const outcome of APP_EVENTS.share.attributes.outcome) {
    assert.deepEqual(
      validateEventAttributes(APP_EVENTS.share.attributes, { outcome }),
      { outcome }
    );
  }
});

test("identifier attributes cannot carry prose", () => {
  const spec = APP_EVENTS["api.decode_error"].attributes;

  assert.deepEqual(
    validateEventAttributes(spec, { "app.api.operation": "getGrupoById" }),
    { "app.api.operation": "getGrupoById" }
  );

  // This is the shape of an Elm decoder error, which embeds the payload. It
  // must not be representable as an operation name.
  const decoderMessage =
    'Problem with the value at json.pagos[0].monto: expected an INT, got "Asado con Lucas"';
  assert.equal(validateEventAttributes(spec, { "app.api.operation": decoderMessage }), null);
  assert.equal(validateEventAttributes(spec, { "app.api.operation": "a b" }), null);
  assert.equal(validateEventAttributes(spec, { "app.api.operation": "x".repeat(65) }), null);
  assert.equal(validateEventAttributes(spec, { "app.api.operation": "" }), null);
});

test("route attributes are templated", () => {
  const attributes = validateEventAttributes(
    { "url.template": "route" },
    { "url.template": `/grupos/${ULID}/gastos` }
  );
  assert.deepEqual(attributes, { "url.template": "/grupos/:id/gastos" });
});

// --- app events and feedback as log records ---------------------------------------------------------------

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
  return { exporter, flushed: () => provider.forceFlush() };
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

test("an event outside the vocabulary emits nothing at all", async () => {
  const { exporter, flushed } = logsWith();

  recordAppEvent("inventado", { outcome: "native" });
  recordAppEvent("share", { outcome: "no-existe" });
  recordAppEvent("share", { outcome: "native", grupoNombre: "Bariloche" });
  await flushed();

  // The third one is valid: the stray attribute is dropped, not the event.
  const records = exporter.getFinishedLogRecords();
  assert.equal(records.length, 1);
  assert.equal(records[0].attributes.outcome, "native");
  assert.ok(!("grupoNombre" in records[0].attributes));
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

test("every gasto form step is a declared section value", async () => {
  // These must stay in step with Models.PagoForm.Section, which the Elm helper
  // maps onto them. A new step there with no value here drops the event.
  assert.deepEqual(APP_EVENTS["gasto.edit_abandoned"].attributes.section, [
    "basico",
    "pagadores",
    "deudores",
  ]);
});
