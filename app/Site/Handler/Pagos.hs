{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Site.Handler.Pagos
    ( handleDeletePago
    , handlePagoNetosPost
    , handlePagoPost
    , handlePagoUpdate
    ) where

import BananaSplit (Pago (..), calcularDeudasPago)
import BananaSplit.Persistence (savePago, deletePago, updatePago)

import Data.ULID (ULID)

import Site.Api (Netos (Netos, netos, transaccionesParaSaldar))
import Site.Handler.Utils (runBeam)

import Types

handlePagoPost :: ULID -> Pago -> AppHandler Pago
handlePagoPost grupoId pago = do
  runBeam (savePago grupoId pago)

handlePagoNetosPost :: Pago -> AppHandler Netos
handlePagoNetosPost pago = do
  pure $ Netos { transaccionesParaSaldar = [], netos = calcularDeudasPago pago}

handleDeletePago :: ULID -> ULID -> AppHandler ULID
handleDeletePago grupoId pagoId = do
  runBeam (deletePago pagoId)
  pure pagoId

handlePagoUpdate :: ULID -> ULID -> Pago -> AppHandler Pago
handlePagoUpdate grupoId pagoId pago = do
  runBeam $ updatePago grupoId pagoId pago
