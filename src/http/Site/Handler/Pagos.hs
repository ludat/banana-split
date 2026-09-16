{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Site.Handler.Pagos (
  handleDeletePago,
  handlePagoGet,
  handlePagoPost,
  handlePagoResumenPost,
  handlePagoUpdate,
  handlePagosGet,
) where

import Protolude
import Servant (err404)

import BananaSplit
import BananaSplit.Persistence (
  deletePago,
  fetchGrupo,
  fetchPago,
  fetchShallowPagos,
  savePago,
  updatePago,
 )
import Site.Api
import Site.Handler.Utils (err423, orElseMay, runBeamFastRead, runBeamWrite, throwJsonError)
import Site.Types

-- | El listado de gastos es la lectura más pesada de la app y no decide nada
-- que se vaya a escribir después, así que no paga el costo de SERIALIZABLE.
handlePagosGet :: ULID -> Maybe ULID -> AppHandler [ShallowPago]
handlePagosGet grupoId participanteId = do
  runBeamFastRead $ fetchShallowPagos grupoId (fmap ParticipanteId participanteId)

handlePagoGet :: ULID -> ULID -> AppHandler Pago
handlePagoGet grupoId pagoId = do
  runBeamFastRead (fetchPago grupoId pagoId)

handlePagoPost :: ULID -> Pago -> AppHandler Pago
handlePagoPost grupoId pago = do
  grupo <-
    runBeamFastRead (fetchGrupo grupoId)
      `orElseMay` throwJsonError err404 "Grupo no encontrado"
  when (estaCongelado grupo) $ throwJsonError err423 "El grupo está congelado"
  runBeamWrite (savePago grupoId pago)

handlePagoResumenPost :: Pago -> AppHandler ResumenPago
handlePagoResumenPost pago = do
  pure $
    ResumenPago
      { resumen = resumenGastos2ResumenNetos $ getResumenGasto pago
      , resumenPagadores = getResumen pago.monto pago.pagadores
      , resumenDeudores = getResumen pago.monto pago.deudores
      }

handleDeletePago :: ULID -> ULID -> AppHandler ULID
handleDeletePago grupoId pagoId = do
  grupo <-
    runBeamFastRead (fetchGrupo grupoId)
      `orElseMay` throwJsonError err404 "Grupo no encontrado"
  when (estaCongelado grupo) $ throwJsonError err423 "El grupo está congelado"
  runBeamWrite (deletePago pagoId)
  pure pagoId

handlePagoUpdate :: ULID -> ULID -> Pago -> AppHandler Pago
handlePagoUpdate grupoId pagoId pago = do
  grupo <-
    runBeamFastRead (fetchGrupo grupoId)
      `orElseMay` throwJsonError err404 "Grupo no encontrado"
  when (estaCongelado grupo) $ throwJsonError err423 "El grupo está congelado"
  runBeamWrite $ updatePago grupoId pagoId pago
