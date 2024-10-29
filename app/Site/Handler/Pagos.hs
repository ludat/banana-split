{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Site.Handler.Pagos
    ( handleDeletePago
    , handlePagoPost
    , handlePagoUpdate
    ) where

import BananaSplit (Pago (..))
import BananaSplit.Persistence (deletePago, savePago, updatePago)

import Control.Monad.Reader

import Data.Pool qualified as Pool
import Data.Text (Text)
import Data.ULID (ULID)

import Database.Selda.Backend
import Database.Selda.PostgreSQL (PG)

import Types


_pagosUpdatedEvent :: Text
_pagosUpdatedEvent = "pagos-updated"

handlePagoPost :: ULID -> Pago -> AppHandler Pago
handlePagoPost grupoId pago = do
  runSelda (savePago grupoId pago)

handleDeletePago :: ULID -> ULID -> AppHandler ULID
handleDeletePago grupoId pagoId = do
  runSelda (deletePago grupoId pagoId)
  pure pagoId

handlePagoUpdate :: ULID -> ULID -> Pago -> AppHandler Pago
handlePagoUpdate grupoId pagoId pago = do
  runSelda $ updatePago grupoId pagoId pago

runSelda :: SeldaT PG IO a -> AppHandler a
runSelda dbAction = do
  pool <- asks (.connection)

  liftIO $ Pool.withResource pool $ \seldaConn -> do
    runSeldaT dbAction seldaConn
