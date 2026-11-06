module Site.Handler.Transferencias (
  handleBorrarTransferencia,
  handleCrearTransferencia,
  handleDesmarcarTransferencia,
  handleEditarTransferencia,
  handleGetTransferencias,
  handleSaldarTransferencia,
) where

import Protolude
import Servant

import BananaSplit
import BananaSplit.Persistence (
  borrarTransferencia,
  crearTransferenciaSaldada,
  desmarcarTransferenciaSaldada,
  editarTransferencia,
  fetchGrupo,
  fetchTransferencias,
  marcarTransferenciaSaldada,
 )
import Site.Api
import Site.Handler.Utils (err423, orElseMay, runBeamFastRead, runBeamWrite, throwJsonError)
import Site.Types

handleGetTransferencias :: ULID -> AppHandler [Transferencia]
handleGetTransferencias grupoId = do
  _ <-
    runBeamFastRead (fetchGrupo grupoId)
      `orElseMay` throwJsonError err404 "Grupo no encontrado"
  runBeamFastRead $ fetchTransferencias grupoId

-- | Marca como hecha una de las transferencias que dejó el congelamiento. No
-- crea un pago: la transferencia hecha ya mueve los netos por sí sola, y además
-- sobrevive al descongelar, así que la plata transferida no se pierde.
handleSaldarTransferencia :: ULID -> ULID -> AppHandler Transferencia
handleSaldarTransferencia grupoId transferenciaId = do
  _ <-
    runBeamFastRead (fetchGrupo grupoId)
      `orElseMay` throwJsonError err404 "Grupo no encontrado"
  runBeamWrite (marcarTransferenciaSaldada grupoId transferenciaId)
    `orElseMay` throwJsonError err404 "Transferencia no encontrada"

handleDesmarcarTransferencia :: ULID -> ULID -> AppHandler Transferencia
handleDesmarcarTransferencia grupoId transferenciaId = do
  grupo <-
    runBeamFastRead (fetchGrupo grupoId)
      `orElseMay` throwJsonError err404 "Grupo no encontrado"
  unless (estaCongelado grupo) $
    throwJsonError err423 "El grupo no está congelado"

  runBeamWrite (desmarcarTransferenciaSaldada grupoId transferenciaId)
    `orElseMay` throwJsonError err404 "Transferencia no encontrada"

handleCrearTransferencia :: ULID -> NuevaTransferenciaParams -> AppHandler Transferencia
handleCrearTransferencia grupoId params = do
  grupo <-
    runBeamFastRead (fetchGrupo grupoId)
      `orElseMay` throwJsonError err404 "Grupo no encontrado"
  when (estaCongelado grupo) $
    throwJsonError err423 "El grupo está congelado"

  runBeamWrite $
    crearTransferenciaSaldada grupoId params.from params.to params.monto params.moneda

-- | Como crear y borrar: congelado, las transferencias son el plan que se
-- calculó, y tocarlas a mano lo desarmaría.
handleEditarTransferencia :: ULID -> ULID -> NuevaTransferenciaParams -> AppHandler Transferencia
handleEditarTransferencia grupoId transferenciaId params = do
  grupo <-
    runBeamFastRead (fetchGrupo grupoId)
      `orElseMay` throwJsonError err404 "Grupo no encontrado"
  when (estaCongelado grupo) $
    throwJsonError err423 "El grupo está congelado"

  runBeamWrite
    (editarTransferencia grupoId transferenciaId params.from params.to params.monto params.moneda)
    `orElseMay` throwJsonError err404 "Transferencia no encontrada"

handleBorrarTransferencia :: ULID -> ULID -> AppHandler ULID
handleBorrarTransferencia grupoId transferenciaId = do
  grupo <-
    runBeamFastRead (fetchGrupo grupoId)
      `orElseMay` throwJsonError err404 "Grupo no encontrado"
  when (estaCongelado grupo) $
    throwJsonError err423 "El grupo está congelado"

  runBeamWrite $ borrarTransferencia grupoId transferenciaId
  pure transferenciaId
