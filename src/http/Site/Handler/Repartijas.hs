{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Site.Handler.Repartijas
    ( handleRepartijaClaimDelete
    , handleRepartijaClaimPut
    , handleRepartijaGet
    ) where

import BananaSplit (Repartija (..), RepartijaClaim (..))
import BananaSplit.Persistence
import BananaSplit.ULID (ULID)

import Protolude

import Site.Handler.Utils
import Site.Types

handleRepartijaGet :: ULID -> AppHandler Repartija
handleRepartijaGet repartijaId = do
  runBeam (fetchRepartija repartijaId)

handleRepartijaClaimPut :: ULID -> RepartijaClaim -> AppHandler RepartijaClaim
handleRepartijaClaimPut repartijaId repartijaClaim = do
  runBeam (saveRepartijaClaim repartijaId repartijaClaim)

handleRepartijaClaimDelete :: ULID -> AppHandler Text
handleRepartijaClaimDelete claimId = do
  void $ runBeam (deleteRepartijaClaim claimId)
  pure "ok"
