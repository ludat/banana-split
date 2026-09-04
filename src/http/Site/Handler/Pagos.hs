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
  repararCacheDeGastos,
  savePago,
  updatePago,
 )
import Site.Api
import Site.Handler.Utils (err423, orElseMay, runBeam, throwJsonError)
import Site.Types

handlePagosGet :: ULID -> AppHandler [ShallowPago]
handlePagosGet grupoId = do
  -- Repara antes de listar para que cada gasto venga con su resumen, aunque el
  -- cache esté frío o guardado con un formato viejo.
  runBeam (repararCacheDeGastos grupoId >> fetchShallowPagos grupoId)

handlePagoGet :: ULID -> ULID -> AppHandler Pago
handlePagoGet _grupoId pagoId = do
  runBeam (fetchPago pagoId)

handlePagoPost :: ULID -> Pago -> AppHandler Pago
handlePagoPost grupoId pago = do
  shallowGrupo <-
    runBeam (fetchGrupo grupoId)
      `orElseMay` throwJsonError err404 "Grupo no encontrado"
  when (estaCongelado shallowGrupo) $ throwJsonError err423 "El grupo está congelado"
  runBeam (savePago grupoId pago)

handlePagoResumenPost :: Pago -> AppHandler ResumenPago
handlePagoResumenPost pago = do
  pure $
    ResumenPago
      { resumen = getResumenPago pago
      , resumenPagadores = getResumen pago.monto pago.pagadores
      , resumenDeudores = getResumen pago.monto pago.deudores
      }

handleDeletePago :: ULID -> ULID -> AppHandler ULID
handleDeletePago grupoId pagoId = do
  shallowGrupo <-
    runBeam (fetchGrupo grupoId)
      `orElseMay` throwJsonError err404 "Grupo no encontrado"
  when (estaCongelado shallowGrupo) $ throwJsonError err423 "El grupo está congelado"
  runBeam (deletePago pagoId)
  pure pagoId

handlePagoUpdate :: ULID -> ULID -> Pago -> AppHandler Pago
handlePagoUpdate grupoId pagoId pago = do
  shallowGrupo <-
    runBeam (fetchGrupo grupoId)
      `orElseMay` throwJsonError err404 "Grupo no encontrado"
  when (estaCongelado shallowGrupo) $ throwJsonError err423 "El grupo está congelado"
  runBeam $ updatePago grupoId pagoId pago
