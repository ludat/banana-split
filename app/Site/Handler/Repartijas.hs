{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Site.Handler.Repartijas
    ( handleRepartijaPost
    , handleRepartijasGet
    , handleRepartijaGet
    , handleRepartijaClaimPut
    ) where

import BananaSplit (Repartija (..), ShallowRepartija(..), RepartijaClaim(..))
import BananaSplit.Persistence

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