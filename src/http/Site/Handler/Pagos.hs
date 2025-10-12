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
    ) where

import BananaSplit
import BananaSplit.Persistence (deletePago, fetchPago, fetchShallowPagos, savePago, updatePago)

import Protolude

import Site.Api
import Site.Handler.Utils (runBeam)
import Site.Types

handlePagosGet :: ULID -> AppHandler [ShallowPago]
handlePagosGet grupoId = do
  runBeam (fetchShallowPagos grupoId)

handlePagoGet :: ULID -> ULID -> AppHandler Pago
handlePagoGet grupoId pagoId = do
  runBeam (fetchPago grupoId pagoId)

handlePagoPost :: ULID -> Pago -> AppHandler Pago
handlePagoPost grupoId pago = do
  runBeam (savePago grupoId pago)

handlePagoResumenPost :: Pago -> AppHandler ResumenPago
handlePagoResumenPost pago = do
  pure $ ResumenPago
    { resumen = getResumenPago pago
    , resumenPagadores = getResumen pago.monto pago.pagadores
    , resumenDeudores = getResumen pago.monto pago.deudores
    }

handleDeletePago :: ULID -> ULID -> AppHandler ULID
handleDeletePago _grupoId pagoId = do
  runBeam (deletePago pagoId)
  pure pagoId

handlePagoUpdate :: ULID -> ULID -> Pago -> AppHandler Pago
handlePagoUpdate grupoId pagoId pago = do
  runBeam $ updatePago grupoId pagoId pago
