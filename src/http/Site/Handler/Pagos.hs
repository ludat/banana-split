{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Site.Handler.Pagos
    ( handleDeletePago
    , handlePagoGet
    , handlePagoPost
    , handlePagoResumenPost
    , handlePagoUpdate
    , handlePagosGet
    , handleSaldarTransaccion
    ) where

import BananaSplit
import BananaSplit.Persistence (deletePago, deleteTransaccionCongelada, fetchGrupo, fetchPago,
                                fetchShallowPagos, savePago, updatePago)

import Protolude

import Servant (err404)

import Site.Api
import Site.Handler.Utils (err423, orElseMay, runBeam, throwJsonError)
import Site.Types

handlePagosGet :: ULID -> AppHandler [ShallowPago]
handlePagosGet grupoId = do
  runBeam (fetchShallowPagos grupoId)

handlePagoGet :: ULID -> ULID -> AppHandler Pago
handlePagoGet grupoId pagoId = do
  runBeam (fetchPago grupoId pagoId)

handlePagoPost :: ULID -> Pago -> AppHandler Pago
handlePagoPost grupoId pago = do
  shallowGrupo <- runBeam (fetchGrupo grupoId)
    `orElseMay` throwJsonError err404 "Grupo no encontrado"
  when shallowGrupo.isFrozen $ throwJsonError err423 "El grupo está congelado"
  runBeam (savePago grupoId pago)

handlePagoResumenPost :: Pago -> AppHandler ResumenPago
handlePagoResumenPost pago = do
  pure $ ResumenPago
    { resumen = getResumenPago pago
    , resumenPagadores = getResumen pago.monto pago.pagadores
    , resumenDeudores = getResumen pago.monto pago.deudores
    }

handleDeletePago :: ULID -> ULID -> AppHandler ULID
handleDeletePago grupoId pagoId = do
  shallowGrupo <- runBeam (fetchGrupo grupoId)
    `orElseMay` throwJsonError err404 "Grupo no encontrado"
  when shallowGrupo.isFrozen $ throwJsonError err423 "El grupo está congelado"
  runBeam (deletePago pagoId)
  pure pagoId

handlePagoUpdate :: ULID -> ULID -> Pago -> AppHandler Pago
handlePagoUpdate grupoId pagoId pago = do
  shallowGrupo <- runBeam (fetchGrupo grupoId)
    `orElseMay` throwJsonError err404 "Grupo no encontrado"
  when shallowGrupo.isFrozen $ throwJsonError err423 "El grupo está congelado"
  runBeam $ updatePago grupoId pagoId pago

handleSaldarTransaccion :: ULID -> ULID -> Pago -> AppHandler Pago
handleSaldarTransaccion grupoId transaccionId pago = do
  savedPago <- runBeam $ savePago grupoId pago
  runBeam $ deleteTransaccionCongelada transaccionId
  pure savedPago
