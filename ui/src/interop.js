import "./styles.css";

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
