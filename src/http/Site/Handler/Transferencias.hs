module Site.Handler.Transferencias (
  handleBorrarTransferencia,
  handleCrearTransferencia,
  handleDesmarcarTransferencia,
  handleEditarTransferencia,
  handleGetTransferencias,
  handleSaldarTransferencia,
) where

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
  withGrupoDescongelado,
 )
import Preludat
import Site.Api
import Site.Handler.Utils (
  err423,
  runBeamFastRead,
  runBeamWrite,
  throwGrupoNoEditable,
  throwJsonError,
 )
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
  -- Al revés que las demás: necesita el grupo congelado. Se chequea en la misma
  -- transacción para que un descongelamiento concurrente, que borra las
  -- pendientes, no deje una pendiente nueva en un grupo descongelado.
  runBeamWrite
    ( fetchGrupo grupoId >>= \case
        Nothing -> pure $ Left (err404, "Grupo no encontrado")
        Just grupo
          | not (estaCongelado grupo) -> pure $ Left (err423, "El grupo no está congelado")
          | otherwise ->
              desmarcarTransferenciaSaldada grupoId transferenciaId
                <&> maybeToRight (err404, "Transferencia no encontrada")
    )
    `orElse` uncurry throwJsonError

handleCrearTransferencia :: ULID -> NuevaTransferenciaParams -> AppHandler Transferencia
handleCrearTransferencia grupoId params = do
  runBeamWrite
    ( withGrupoDescongelado grupoId
        $ crearTransferenciaSaldada grupoId params.from params.to params.monto params.moneda
    )
    `orElse` throwGrupoNoEditable

-- | Como crear y borrar: congelado, las transferencias son el plan que se
-- calculó, y tocarlas a mano lo desarmaría.
handleEditarTransferencia :: ULID -> ULID -> NuevaTransferenciaParams -> AppHandler Transferencia
handleEditarTransferencia grupoId transferenciaId params =
  ( runBeamWrite
      ( withGrupoDescongelado grupoId
          $ editarTransferencia grupoId transferenciaId params.from params.to params.monto params.moneda
      )
      `orElse` throwGrupoNoEditable
  )
    `orElseMay` throwJsonError err404 "Transferencia no encontrada"

handleBorrarTransferencia :: ULID -> ULID -> AppHandler ULID
handleBorrarTransferencia grupoId transferenciaId = do
  runBeamWrite (withGrupoDescongelado grupoId $ borrarTransferencia grupoId transferenciaId)
    `orElse` throwGrupoNoEditable
  pure transferenciaId
