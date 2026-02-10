import "./styles.css";


import { setTheme } from "@ui5/webcomponents-base/dist/config/Theme.js";
import { setLanguage } from "@ui5/webcomponents-base/dist/config/Language.js";

import "@ui5/webcomponents/dist/Assets.js";
import "@ui5/webcomponents-fiori/dist/Assets.js";
import "@ui5/webcomponents-icons/dist/Assets.js"
import "@ui5/webcomponents-icons/dist/delete.js"
import "@ui5/webcomponents-icons/dist/add.js"

import "@ui5/webcomponents/dist/Button.js";
import "@ui5/webcomponents/dist/List.js";
import "@ui5/webcomponents/dist/ListItemStandard.js";
import "@ui5/webcomponents/dist/Form.js";
import "@ui5/webcomponents/dist/FormItem.js";
import "@ui5/webcomponents/dist/Input.js";
import "@ui5/webcomponents/dist/Label.js";
import "@ui5/webcomponents/dist/Link.js";
import "@ui5/webcomponents/dist/Option.js";
import "@ui5/webcomponents/dist/Select.js";
import "@ui5/webcomponents/dist/Bar.js";
import "@ui5/webcomponents/dist/Title.js";
import "@ui5/webcomponents-fiori/dist/Page.js";

setLanguage("es");

const darkModeQuery = window.matchMedia("(prefers-color-scheme: dark)");
console.log(darkModeQuery);
setTheme(darkModeQuery.matches ? "sap_horizon_dark" : "sap_horizon");
darkModeQuery.addEventListener("change", (e) => {
  setTheme(e.matches ? "sap_horizon_dark" : "sap_horizon");
});

// Storage key format: "banana-split:grupo:{grupoId}:currentUser"
const makeStorageKey = (grupoId) => `banana-split:grupo:${grupoId}:currentUser`;

const beforeUnloadHandler = (event) => {
  event.preventDefault();
};

export const onReady = ({ app, env }) => {
  if (app.ports && app.ports.outgoing) {
    app.ports.outgoing.subscribe(({ tag, data }) => {
      console.log(tag, data);

      switch (tag) {
        case "SAVE_CURRENT_USER":
          // data: { grupoId: string, userId: string }
          if (data.grupoId && data.userId) {
            localStorage.setItem(
              makeStorageKey(data.grupoId),
              data.userId
            );
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
                userId: userId
              }
            });
          }
          break;
        case "SET_UNSAVED_CHANGES_WARNING":
          // data: { enabled: bool }
          if (data.enabled) {
            window.addEventListener('beforeunload', beforeUnloadHandler);
          } else {
            window.removeEventListener('beforeunload', beforeUnloadHandler);
          }
          break;

        default:
          console.warn("Unknown port message tag:", tag);
      }
    });
  }
};
