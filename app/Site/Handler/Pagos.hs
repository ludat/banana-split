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
import BananaSplit.Persistence (deletePago, savePago, updatePago)

import Data.ULID (ULID)

import Protolude

import Site.Api
import Site.Handler.Utils (runBeam)

import Types

handlePagoPost :: ULID -> Pago -> AppHandler Pago
handlePagoPost grupoId pago = do
  runBeam (savePago grupoId pago)

handlePagoNetosPost :: Pago -> AppHandler Netos
handlePagoNetosPost pago = do
  pure $ Netos $ calcularDeudasPago pago

handleDeletePago :: ULID -> ULID -> AppHandler ULID
handleDeletePago grupoId pagoId = do
  runBeam (deletePago pagoId)
  pure pagoId

handlePagoUpdate :: ULID -> ULID -> Pago -> AppHandler Pago
handlePagoUpdate grupoId pagoId pago = do
  runBeam $ updatePago grupoId pagoId pago
