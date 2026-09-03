module Site.Handler.Transacciones (
  handleCrearTransaccion,
  handleDesmarcarTransaccion,
  handleSaldarTransaccion,
) where

import Protolude
import Servant (err404, err409)

import BananaSplit
import BananaSplit.Persistence (
  crearTransaccionSaldada,
  desmarcarTransaccionSaldada,
  fetchGrupo,
  fetchTransacciones,
  marcarTransaccionSaldada,
  transaccionesHechasDesde,
 )
import Site.Api
import Site.Handler.Utils (err423, orElseMay, runBeam, throwJsonError)
import Site.Types

-- | Marca como hecha una de las transacciones que dejó el congelamiento. No
-- crea un pago: la transacción hecha ya mueve los netos por sí sola, y además
-- sobrevive al descongelar, así que la plata transferida no se pierde.
handleSaldarTransaccion :: ULID -> ULID -> AppHandler ULID
handleSaldarTransaccion grupoId transaccionId = do
  _ <-
    runBeam (fetchGrupo grupoId)
      `orElseMay` throwJsonError err404 "Grupo no encontrado"
  runBeam $ marcarTransaccionSaldada grupoId transaccionId
  pure transaccionId

-- | La vuelve a dejar pendiente, para cuando alguien la marcó por error. Solo
-- se puede con las transacciones de este congelamiento: una pendiente vive
-- únicamente mientras el grupo está congelado, así que desmarcar una de un
-- congelamiento anterior la dejaría en un limbo que el próximo congelar borra.
handleDesmarcarTransaccion :: ULID -> ULID -> AppHandler ULID
handleDesmarcarTransaccion grupoId transaccionId = do
  shallowGrupo <-
    runBeam (fetchGrupo grupoId)
      `orElseMay` throwJsonError err404 "Grupo no encontrado"
  unless (estaCongelado shallowGrupo) $
    throwJsonError err423 "El grupo no está congelado"

  guardadas <- runBeam $ fetchTransacciones grupoId
  let deEsteCongelamiento =
        maybe mempty (`transaccionesHechasDesde` guardadas) shallowGrupo.congeladoAt
  unless (any ((== Just transaccionId) . (.id)) (fold deEsteCongelamiento)) $
    throwJsonError err409 "Esa transferencia no es de este congelamiento"

  runBeam $ desmarcarTransaccionSaldada grupoId transaccionId
  pure transaccionId

-- | Registrar una transferencia en un grupo que no está congelado, donde no hay
-- transacciones guardadas que marcar. Nace hecha.
handleCrearTransaccion :: ULID -> NuevaTransaccionParams -> AppHandler Transaccion
handleCrearTransaccion grupoId params = do
  shallowGrupo <-
    runBeam (fetchGrupo grupoId)
      `orElseMay` throwJsonError err404 "Grupo no encontrado"
  -- Con el grupo congelado las transacciones ya están decididas: hay que marcar
  -- una de esas, no inventar otra.
  when (estaCongelado shallowGrupo) $
    throwJsonError err423 "El grupo está congelado"
  runBeam $
    crearTransaccionSaldada
      grupoId
      params.moneda
      Transaccion
        { id = Nothing
        , from = params.from
        , to = params.to
        , monto = params.monto
        }
