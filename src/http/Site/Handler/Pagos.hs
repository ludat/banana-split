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

import BananaSplit
import BananaSplit.Persistence (
  deletePago,
  fetchPago,
  fetchShallowPagos,
  savePago,
  updatePago,
  withGrupoDescongelado,
 )
import Preludat
import Site.Api
import Site.Handler.Utils (runBeamFastRead, runBeamWrite, throwGrupoNoEditable)
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
  runBeamWrite (withGrupoDescongelado grupoId $ savePago grupoId pago)
    `orElse` throwGrupoNoEditable

handlePagoResumenPost :: Pago -> AppHandler ResumenPago
handlePagoResumenPost pago = do
  pure
    $ ResumenPago
      { resumen = resumenGastos2ResumenNetos $ getResumenGasto pago
      , resumenPagadores = getResumen pago.monto pago.pagadores
      , resumenDeudores = getResumen pago.monto pago.deudores
      }

handleDeletePago :: ULID -> ULID -> AppHandler ULID
handleDeletePago grupoId pagoId = do
  runBeamWrite (withGrupoDescongelado grupoId $ deletePago pagoId)
    `orElse` throwGrupoNoEditable
  pure pagoId

handlePagoUpdate :: ULID -> ULID -> Pago -> AppHandler Pago
handlePagoUpdate grupoId pagoId pago = do
  runBeamWrite (withGrupoDescongelado grupoId $ updatePago grupoId pagoId pago)
    `orElse` throwGrupoNoEditable
