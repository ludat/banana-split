import "bootstrap/dist/css/bootstrap.min.css";
import "bootstrap-icons/font/bootstrap-icons.css";
import "bootstrap";
import "./styles.css";
import "./js/MontoInput";

const darkModeQuery = window.matchMedia("(prefers-color-scheme: dark)");
const applyTheme = (dark) => {
  document.documentElement.setAttribute("data-bs-theme", dark ? "dark" : "light");
};
applyTheme(darkModeQuery.matches);
darkModeQuery.addEventListener("change", (e) => applyTheme(e.matches));

// Storage key format: "banana-split:grupo:{grupoId}:currentUser"
const makeStorageKey = (grupoId) => `banana-split:grupo:${grupoId}:currentUser`;

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
        name: `Banana Split - ${nombre}`,
        short_name: nombre,
        start_url: `${origin}/grupos/${grupoId}`,
        scope: `${origin}/grupos/${grupoId}`,
        display: "standalone",
        background_color: "#ffffff",
        theme_color: "#0070f2",
        icons: [{ src: `${origin}/favicon.png`, sizes: "any", type: "image/png" }],
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
  const lastReadRaw = localStorage.getItem("banana-split:lastReadChangelog");
  return {
    now: now.getTime(),
    offset: now.getTimezoneOffset(),
    timeZone: Intl.DateTimeFormat().resolvedOptions().timeZone,
    lastReadChangelog: lastReadRaw ? parseInt(lastReadRaw, 10) : null,
  };
};

export const onReady = ({ app, env }) => {
  if (app.ports && app.ports.outgoing) {
    app.ports.outgoing.subscribe(({ tag, data }) => {
      console.log(tag, data);

      switch (tag) {
        case "SAVE_CURRENT_USER":
          // data: { grupoId: string, userId: string }
          if (data.grupoId && data.userId) {
            localStorage.setItem(makeStorageKey(data.grupoId), data.userId);
          }
          break;

        case "CLEAR_CURRENT_USER":
          // data: { grupoId: string }
          if (data.grupoId) {
            localStorage.removeItem(makeStorageKey(data.grupoId));
          }
          break;

        case "GET_CURRENT_USER":
          // data: { grupoId: string }
          if (data.grupoId) {
            const userId = localStorage.getItem(makeStorageKey(data.grupoId));
            app.ports.incoming.send({
              tag: "CURRENT_USER_LOADED",
              data: {
                grupoId: data.grupoId,
                userId: userId,
              },
            });
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
          localStorage.setItem("banana-split:lastReadChangelog", new Date().getTime().toString());
          break;

        default:
          console.warn("Unknown port message tag:", tag);
      }
    });
  }
};
