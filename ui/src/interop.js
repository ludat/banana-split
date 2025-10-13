import "./styles.css";

// Storage key format: "banana-split:grupo:{grupoId}:currentUser"
const makeStorageKey = (grupoId) => `banana-split:grupo:${grupoId}:currentUser`;

export const onReady = ({ app, env }) => {
  if (app.ports && app.ports.outgoing) {
    app.ports.outgoing.subscribe(({ tag, data }) => {
      console.log(tag, data);

      switch (tag) {
        case "SAVE_CURRENT_USER":
          // data: { grupoId: string, userId: string }
          if (data.grupoId && data.userId) {
            sessionStorage.setItem(
              makeStorageKey(data.grupoId),
              data.userId
            );
          }
          break;

        case "CLEAR_CURRENT_USER":
          // data: { grupoId: string }
          if (data.grupoId) {
            sessionStorage.removeItem(makeStorageKey(data.grupoId));
          }
          break;

        case "GET_CURRENT_USER":
          // data: { grupoId: string }
          if (data.grupoId && app.ports.incoming) {
            const userId = sessionStorage.getItem(makeStorageKey(data.grupoId));
            app.ports.incoming.send({
              tag: "CURRENT_USER_LOADED",
              data: {
                grupoId: data.grupoId,
                userId: userId
              }
            });
          }
          break;

        default:
          console.warn("Unknown port message tag:", tag);
      }
    });
  }
};
