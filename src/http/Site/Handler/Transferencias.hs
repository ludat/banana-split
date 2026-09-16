module Site.Handler.Transferencias (
  handleBorrarTransferencia,
  handleCrearTransferencia,
  handleDesmarcarTransferencia,
  handleSaldarTransferencia,
) where

import Protolude
import Servant

import BananaSplit
import BananaSplit.Persistence (
  borrarTransferencia,
  crearTransferenciaSaldada,
  desmarcarTransferenciaSaldada,
  fetchGrupo,
  marcarTransferenciaSaldada,
 )
import Site.Api
import Site.Handler.Utils (err423, orElseMay, runBeamFastRead, runBeamWrite, throwJsonError)
import Site.Types

-- | Marca como hecha una de las transferencias que dejó el congelamiento. No
-- crea un pago: la transferencia hecha ya mueve los netos por sí sola, y además
-- sobrevive al descongelar, así que la plata transferida no se pierde.
handleSaldarTransferencia :: ULID -> ULID -> AppHandler ULID
handleSaldarTransferencia grupoId transferenciaId = do
  _ <-
    runBeamFastRead (fetchGrupo grupoId)
      `orElseMay` throwJsonError err404 "Grupo no encontrado"
  runBeamWrite $ marcarTransferenciaSaldada grupoId transferenciaId
  pure transferenciaId

handleDesmarcarTransferencia :: ULID -> ULID -> AppHandler ULID
handleDesmarcarTransferencia grupoId transferenciaId = do
  shallowGrupo <-
    runBeamFastRead (fetchGrupo grupoId)
      `orElseMay` throwJsonError err404 "Grupo no encontrado"
  unless (estaCongelado shallowGrupo) $
    throwJsonError err423 "El grupo no está congelado"

  runBeamWrite $ desmarcarTransferenciaSaldada grupoId transferenciaId
  pure transferenciaId

handleCrearTransferencia :: ULID -> NuevaTransferenciaParams -> AppHandler Transferencia
handleCrearTransferencia grupoId params = do
  shallowGrupo <-
    runBeamFastRead (fetchGrupo grupoId)
      `orElseMay` throwJsonError err404 "Grupo no encontrado"
  when (estaCongelado shallowGrupo) $
    throwJsonError err423 "El grupo está congelado"

  runBeamWrite $
    crearTransferenciaSaldada
      grupoId
      params.moneda
      Transferencia
        { id = Nothing
        , from = params.from
        , to = params.to
        , monto = params.monto
        }

handleBorrarTransferencia :: ULID -> ULID -> AppHandler ULID
handleBorrarTransferencia grupoId transferenciaId = do
  shallowGrupo <-
    runBeamFastRead (fetchGrupo grupoId)
      `orElseMay` throwJsonError err404 "Grupo no encontrado"
  when (estaCongelado shallowGrupo) $
    throwJsonError err423 "El grupo está congelado"

  runBeamWrite $ borrarTransferencia grupoId transferenciaId
  pure transferenciaId
