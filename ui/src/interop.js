import "./styles.css";

import { setTheme } from "@ui5/webcomponents-base/dist/config/Theme.js";
import { setLanguage } from "@ui5/webcomponents-base/dist/config/Language.js";

import "@ui5/webcomponents/dist/Assets.js";
import "@ui5/webcomponents-fiori/dist/Assets.js";

import "@ui5/webcomponents-icons/dist/accept.js";
import "@ui5/webcomponents-icons/dist/activity-2.js";
import "@ui5/webcomponents-icons/dist/add.js";
import "@ui5/webcomponents-icons/dist/alert.js";
import "@ui5/webcomponents-icons/dist/arrow-right.js";
import "@ui5/webcomponents-icons/dist/Assets.js";
import "@ui5/webcomponents-icons/dist/bell.js"
import "@ui5/webcomponents-icons/dist/decline.js";
import "@ui5/webcomponents-icons/dist/delete.js";
import "@ui5/webcomponents-icons/dist/edit.js";
import "@ui5/webcomponents-icons/dist/home.js";
import "@ui5/webcomponents-icons/dist/menu.js";
import "@ui5/webcomponents-icons/dist/money-bills.js";
import "@ui5/webcomponents-icons/dist/sys-minus.js";
import "@ui5/webcomponents-icons/dist/upload.js";
import "@ui5/webcomponents-icons/dist/user-edit.js";

import "@ui5/webcomponents/dist/Bar.js";
import "@ui5/webcomponents/dist/BusyIndicator.js";
import "@ui5/webcomponents/dist/Button.js";
import "@ui5/webcomponents/dist/CheckBox.js";
import "@ui5/webcomponents/dist/Dialog.js";
import "@ui5/webcomponents/dist/FileUploader.js";
import "@ui5/webcomponents/dist/FormItem.js";
import "@ui5/webcomponents/dist/Form.js";
import "@ui5/webcomponents/dist/Input.js";
import "@ui5/webcomponents/dist/Label.js";
import "@ui5/webcomponents/dist/Link.js";
import "@ui5/webcomponents/dist/ListItemStandard.js";
import "@ui5/webcomponents/dist/List.js";
import "@ui5/webcomponents/dist/MessageStrip.js";
import "@ui5/webcomponents/dist/Option.js";
import "@ui5/webcomponents/dist/ResponsivePopover.js";
import "@ui5/webcomponents/dist/SegmentedButton.js";
import "@ui5/webcomponents/dist/Select.js";
import "@ui5/webcomponents/dist/TableCell.js";
import "@ui5/webcomponents/dist/TableHeaderCell.js";
import "@ui5/webcomponents/dist/TableHeaderRow.js";
import "@ui5/webcomponents/dist/Table.js";
import "@ui5/webcomponents/dist/TableRowAction.js";
import "@ui5/webcomponents/dist/TableRow.js";
import "@ui5/webcomponents/dist/Text.js";
import "@ui5/webcomponents/dist/Title.js";
import "@ui5/webcomponents-fiori/dist/NavigationLayout.js";
import "@ui5/webcomponents-fiori/dist/Page.js";
import "@ui5/webcomponents-fiori/dist/ShellBarBranding.js";
import "@ui5/webcomponents-fiori/dist/ShellBarItem.js";
import "@ui5/webcomponents-fiori/dist/ShellBar.js";
import "@ui5/webcomponents-fiori/dist/SideNavigationGroup.js";
import "@ui5/webcomponents-fiori/dist/SideNavigationItem.js";
import "@ui5/webcomponents-fiori/dist/SideNavigation.js";
import "@ui5/webcomponents-fiori/dist/SideNavigationSubItem.js";
import "@ui5/webcomponents-fiori/dist/Wizard.js";

setLanguage("es");

const defaultTheme = "sap_horizon";
// const defaultTheme = "sap_fiori_3"
const defaultDarkTheme = defaultTheme + "_dark";

const darkModeQuery = window.matchMedia("(prefers-color-scheme: dark)");
console.log(darkModeQuery);
setTheme(darkModeQuery.matches ? defaultDarkTheme : defaultTheme);
darkModeQuery.addEventListener("change", (e) => {
  // setTheme(e.matches ? "sap_horizon_dark" : "sap_horizon");
  setTheme(e.matches ? defaultDarkTheme : defaultTheme);
});

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
          localStorage.setItem("banana-split:lastReadChangelog", new Date().getTime());
          break;

        default:
          console.warn("Unknown port message tag:", tag);
      }
    });
  }
};
