module Site.Handler.Transacciones (
  handleBorrarTransaccion,
  handleCrearTransaccion,
  handleDesmarcarTransaccion,
  handleSaldarTransaccion,
) where

import Protolude
import Servant

import BananaSplit
import BananaSplit.Persistence (
  borrarTransaccion,
  crearTransaccionSaldada,
  desmarcarTransaccionSaldada,
  fetchGrupo,
  marcarTransaccionSaldada,
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

handleDesmarcarTransaccion :: ULID -> ULID -> AppHandler ULID
handleDesmarcarTransaccion grupoId transaccionId = do
  shallowGrupo <-
    runBeam (fetchGrupo grupoId)
      `orElseMay` throwJsonError err404 "Grupo no encontrado"
  unless (estaCongelado shallowGrupo) $
    throwJsonError err423 "El grupo no está congelado"

  runBeam $ desmarcarTransaccionSaldada grupoId transaccionId
  pure transaccionId

handleCrearTransaccion :: ULID -> NuevaTransaccionParams -> AppHandler Transaccion
handleCrearTransaccion grupoId params = do
  shallowGrupo <-
    runBeam (fetchGrupo grupoId)
      `orElseMay` throwJsonError err404 "Grupo no encontrado"
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

handleBorrarTransaccion :: ULID -> ULID -> AppHandler ULID
handleBorrarTransaccion grupoId transaccionId = do
  shallowGrupo <-
    runBeam (fetchGrupo grupoId)
      `orElseMay` throwJsonError err404 "Grupo no encontrado"
  when (estaCongelado shallowGrupo) $
    throwJsonError err423 "El grupo está congelado"

  runBeam $ borrarTransaccion grupoId transaccionId
  pure transaccionId
