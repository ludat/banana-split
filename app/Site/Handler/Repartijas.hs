{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Site.Handler.Repartijas
    ( handleRepartijaClaimDelete
    , handleRepartijaClaimPut
    , handleRepartijaGet
    , handleRepartijaPost
    , handleRepartijasGet
    , handleRepartijaToPago
    ) where

import BananaSplit (Repartija (..), RepartijaClaim (..), ShallowRepartija (..), repartija2Pago)
import BananaSplit.Persistence

import Control.Monad (void)

import Data.ULID (ULID)

import Site.Handler.Utils

import Types

handleRepartijaPost :: ULID -> Repartija -> AppHandler Repartija
handleRepartijaPost grupoId repartija = do
  runBeam (saveRepartija grupoId repartija)

handleRepartijasGet :: ULID -> AppHandler [ShallowRepartija]
handleRepartijasGet grupoId = do
  runBeam (fetchRepartijas grupoId)

handleRepartijaGet :: ULID -> AppHandler Repartija
handleRepartijaGet repartijaId = do
  runBeam (fetchRepartija repartijaId)

handleRepartijaClaimPut :: ULID -> RepartijaClaim -> AppHandler RepartijaClaim
handleRepartijaClaimPut repartijaId repartijaClaim = do
  runBeam (saveRepartijaClaim repartijaId repartijaClaim)

handleRepartijaClaimDelete :: ULID -> AppHandler String
handleRepartijaClaimDelete claimId = do
  void $ runBeam (deleteRepartijaClaim claimId)
  pure "ok"

handleRepartijaToPago :: ULID -> AppHandler String
handleRepartijaToPago repartijaId = do
  repartija <- runBeam (fetchRepartija repartijaId)
  let newPago = repartija2Pago repartija
  _ <- runBeam (savePago repartija.repartijaGrupoId newPago)
  pure "ok"
