import "bootstrap/dist/css/bootstrap.min.css";
import "bootstrap-icons/font/bootstrap-icons.css";
import "bootstrap";
import "./styles.css";
import "./stepper.css";
import "./js/MontoInput";
import {
  initTelemetry,
  isSectionSeen,
  recordEvent,
  recordFeedback,
  SECTION_THRESHOLDS,
} from "./js/telemetry";

initTelemetry();

const darkModeQuery = window.matchMedia("(prefers-color-scheme: dark)");
const applyTheme = (dark) => {
  document.documentElement.setAttribute("data-bs-theme", dark ? "dark" : "light");
};
applyTheme(darkModeQuery.matches);
darkModeQuery.addEventListener("change", (e) => applyTheme(e.matches));

// Cada preferencia de cada grupo es una clave aparte, con el valor como string:
// "repartix:grupo:{grupoId}:preferencia:{campo}". Escribir una no toca las demás,
// así que las que esta versión no conoce no se pierden, y qué significa cada
// valor lo decide Elm.
const PREFERENCIA_KEY = /^repartix:grupo:([^:]+):preferencia:(.+)$/;
const preferenciaKey = (grupoId, campo) => `repartix:grupo:${grupoId}:preferencia:${campo}`;

const LAST_READ_CHANGELOG_KEY = "repartix:lastReadChangelog";
const REPARTIJA_INTRO_DISMISSED_KEY = "repartix:repartijaIntroDismissed";

// Las claves de cuando la app se llamaba Banana Split.
const CLAVES_RENOMBRADAS = [
  [/^split:(grupo:[^:]+:preferencia:.+)$/, (resto) => `repartix:${resto}`],
  [/^banana-split:(lastReadChangelog|repartijaIntroDismissed)$/, (resto) => `repartix:${resto}`],
];

// La clave de antes, que se escribía con cualquier cambio de participante.
// Se migra una vez a su preferencia y se borra.
const CURRENT_USER_KEY = /^banana-split:grupo:([^:]+):currentUser$/;

// Se juntan antes de procesarlas porque procesar puede agregar o borrar claves
// y correr los índices.
const clavesQueMatchean = (regex) => {
  const claves = [];
  for (let i = 0; i < localStorage.length; i++) {
    const match = localStorage.key(i).match(regex);
    if (match) claves.push(match);
  }
  return claves;
};

const migrarCurrentUser = () => {
  const viejas = clavesQueMatchean(CURRENT_USER_KEY);
  if (viejas.length === 0) return;
  let migradas = 0;
  for (const [vieja, grupoId] of viejas) {
    const nueva = preferenciaKey(grupoId, "participanteId");
    if (localStorage.getItem(nueva) === null) {
      localStorage.setItem(nueva, localStorage.getItem(vieja));
      migradas++;
    }
    localStorage.removeItem(vieja);
  }
  // Las descartadas son las que ya tenían la preferencia nueva.
  recordEvent("app.preferencias.migrated", {
    "app.preferencias.migrated_count": migradas,
    "app.preferencias.discarded_count": viejas.length - migradas,
  });
};

// Si la nueva ya existe gana la nueva; la vieja se borra igual.
const migrarClavesRenombradas = () => {
  for (const [regex, claveNueva] of CLAVES_RENOMBRADAS) {
    for (const [vieja, resto] of clavesQueMatchean(regex)) {
      const nueva = claveNueva(resto);
      if (localStorage.getItem(nueva) === null) {
        localStorage.setItem(nueva, localStorage.getItem(vieja));
      }
      localStorage.removeItem(vieja);
    }
  }
};

// { grupoId: { campo: valor } }
const leerPreferencias = () => {
  const todas = {};
  for (const [key, grupoId, campo] of clavesQueMatchean(PREFERENCIA_KEY)) {
    todas[grupoId] = { ...todas[grupoId], [campo]: localStorage.getItem(key) };
  }
  return todas;
};

// null es borrarla.
const guardarPreferencia = (grupoId, campo, valor) => {
  if (valor === null) {
    localStorage.removeItem(preferenciaKey(grupoId, campo));
  } else {
    localStorage.setItem(preferenciaKey(grupoId, campo), valor);
  }
};

const beforeUnloadHandler = (event) => {
  event.preventDefault();
};

customElements.define(
  "pwa-manifest",
  class extends HTMLElement {
    static observedAttributes = ["grupo-id", "nombre"];

    connectedCallback() {
      this.updateManifest();
    }

    attributeChangedCallback() {
      this.updateManifest();
    }

    disconnectedCallback() {
      const link = document.querySelector('link[rel="manifest"]');
      if (link) link.removeAttribute("href");
    }

    updateManifest() {
      const grupoId = this.getAttribute("grupo-id");
      const nombre = this.getAttribute("nombre");
      if (!grupoId || !nombre) return;
      const origin = window.location.origin;
      const manifest = {
        name: `Repartix - ${nombre}`,
        short_name: nombre,
        start_url: `${origin}/grupos/${grupoId}`,
        scope: `${origin}/grupos/${grupoId}`,
        display: "standalone",
        background_color: "#ffffff",
        theme_color: "#0070f2",
        icons: [
          { src: `${origin}/icon-192.png?v=2`, sizes: "192x192", type: "image/png" },
          { src: `${origin}/icon-512.png?v=2`, sizes: "512x512", type: "image/png" },
        ],
      };
      const dataUrl = `data:application/manifest+json,${encodeURIComponent(JSON.stringify(manifest))}`;
      let link = document.querySelector('link[rel="manifest"]');
      if (!link) {
        link = document.createElement("link");
        link.rel = "manifest";
        document.head.appendChild(link);
      }
      link.href = dataUrl;
    }
  }
);

export const flags = ({ env }) => {
  const now = new Date();
  migrarClavesRenombradas();
  migrarCurrentUser();
  const lastReadRaw = localStorage.getItem(LAST_READ_CHANGELOG_KEY);
  return {
    now: now.getTime(),
    offset: - now.getTimezoneOffset(),
    timeZone: Intl.DateTimeFormat().resolvedOptions().timeZone,
    lastReadChangelog: lastReadRaw ? parseInt(lastReadRaw, 10) : null,
    repartijaIntroDismissed: localStorage.getItem(REPARTIJA_INTRO_DISMISSED_KEY) !== null,
    preferenciasDeGrupos: leerPreferencias(),
    origin: window.location.origin,
    initialData: readInitialData(),
  };
};

// Lo que el backend inyecta en el index.html (ver Site.Frontend). El dev server
// de elm-land sirve su propio HTML, así que ahí llega por /api/initial-data.js
// (el <script defer> que agrega elm-land.json), que corre antes que este módulo.
// Si no está por ningún lado, la app espera a la API.
const readInitialData = () => {
  const element = document.querySelector('meta[name="initial-data"]');
  if (element) return JSON.parse(element.getAttribute("content"));
  return window.initialData ?? null;
};

// `<track-view name="landing.hero">` (ver `Utils.Telemetry.seccionVista` del
// lado Elm) manda `app.section_viewed` la primera vez que se ve, y después deja
// de mirar: una vez por cada vez que se monta, que en Elm es una por visita a
// la pantalla.
customElements.define(
  "track-view",
  class extends HTMLElement {
    connectedCallback() {
      this.observer = new IntersectionObserver(
        (entries) => {
          if (!entries.some((entry) => isSectionSeen(entry, window.innerHeight))) return;
          recordEvent("app.section_viewed", { "app.section": this.getAttribute("name") });
          this.stop();
        },
        { threshold: SECTION_THRESHOLDS }
      );
      this.observer.observe(this);
    }

    disconnectedCallback() {
      this.stop();
    }

    stop() {
      this.observer?.disconnect();
      this.observer = null;
    }
  }
);

// Mientras está en la página, cada pedido de cierre (el botón atrás de
// Android, Escape en desktop) se convierte en un evento `close` sobre el
// elemento, sin tocar el historial. Qué cerrar lo decide quien lo escucha.
//
// Un CloseWatcher sirve para un solo pedido, así que después de cada uno se
// arma otro mientras el elemento siga montado: sacarlo de la página es lo que
// devuelve el "atrás" a la navegación. Donde no existe la API no hace nada.
customElements.define(
  "close-watcher",
  class extends HTMLElement {
    connectedCallback() {
      this.watch();
    }

    disconnectedCallback() {
      if (this.watcher) this.watcher.destroy();
      this.watcher = null;
    }

    watch() {
      if (typeof CloseWatcher === "undefined") return;
      const watcher = new CloseWatcher();
      watcher.onclose = () => {
        if (this.watcher !== watcher) return;
        this.watcher = null;
        this.dispatchEvent(new CustomEvent("close"));
        if (this.isConnected) this.watch();
      };
      this.watcher = watcher;
    }
  }
);

export const onReady = ({ app, env }) => {
  if (app.ports && app.ports.outgoing) {
    app.ports.outgoing.subscribe(({ tag, data }) => {
      console.log(tag, data);

      switch (tag) {
        case "SAVE_PREFERENCIA_DE_GRUPO":
          // data: { grupoId: string, campo: string, valor: string | null }
          if (data.grupoId) {
            guardarPreferencia(data.grupoId, data.campo, data.valor);
          }
          break;
        case "SET_UNSAVED_CHANGES_WARNING":
          // data: { enabled: bool }
          if (data.enabled) {
            window.addEventListener("beforeunload", beforeUnloadHandler);
          } else {
            window.removeEventListener("beforeunload", beforeUnloadHandler);
          }
          break;

        case "SAVE_LAST_READ_CHANGELOG":
          // data: null
          localStorage.setItem(LAST_READ_CHANGELOG_KEY, new Date().getTime().toString());
          break;

        case "SAVE_REPARTIJA_INTRO_DISMISSED":
          // data: null
          localStorage.setItem(REPARTIJA_INTRO_DISMISSED_KEY, "true");
          break;

        case "SHARE":
          // data: { title: string, url: string }
          {
            if (navigator.share) {
              navigator.share({ title: data.title, url: data.url }).then(
                () => recordEvent("app.share", { outcome: "native" }),
                (err) => {
                  // AbortError happens when the user dismisses the share sheet.
                  // On Android it's also what some share targets answer when
                  // they fail, hence the message.
                  if (err && err.name === "AbortError") {
                    recordEvent("app.share", {
                      outcome: "dismissed",
                      "error.message": String(err.message),
                    });
                    return;
                  }
                  recordEvent("app.share", {
                    outcome: "failed",
                    "error.type": String(err && err.name),
                    "error.message": String(err && err.message),
                  });
                  console.warn("Share failed:", err);
                },
              );
            } else if (navigator.clipboard) {
              navigator.clipboard.writeText(data.url).then(
                () => {
                  recordEvent("app.share", { outcome: "clipboard" });
                  app.ports.incoming.send({
                    tag: "SHARE_LINK_COPIED",
                    data: null,
                  });
                },
                (err) => {
                  recordEvent("app.share", {
                    outcome: "failed",
                    method: "clipboard",
                    "error.type": String(err && err.name),
                    "error.message": String(err && err.message),
                  });
                  console.warn("Clipboard write failed:", err);
                },
              );
            } else {
              recordEvent("app.share", { outcome: "unsupported" });
            }
          }
          break;

        case "SEND_FEEDBACK":
          // data: { message: string, name: string, attributes: { [k: string]: string } }
          // El único texto libre que sale del browser, y porque la persona lo
          // escribió y apretó un botón. Ver js/telemetry.js.
          app.ports.incoming.send({
            tag: "FEEDBACK_SENT",
            data: { accepted: recordFeedback(data.message, data.name, data.attributes) },
          });
          break;

        case "TELEMETRY_EVENT":
          // data: { name: string, attributes: { [k: string]: string }, severity: string }
          recordEvent(data.name, data.attributes, data.severity);
          break;

        case "COPY":
          // data: { text: string }
          {
            if (navigator.clipboard) {
              navigator.clipboard.writeText(data.text).then(
                () =>
                  app.ports.incoming.send({
                    tag: "TEXT_COPIED",
                    data: null,
                  }),
                (err) => console.warn("Clipboard write failed:", err),
              );
            }
          }
          break;

        default:
          console.warn("Unknown port message tag:", tag);
      }
    });
  }
};
