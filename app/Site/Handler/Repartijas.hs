{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Site.Handler.Repartijas
    ( handleRepartijaPost
    ) where

import BananaSplit (Repartija (..))
import BananaSplit.Persistence

import Data.ULID (ULID)

import Site.Handler.Utils

import Types

handleRepartijaPost :: ULID -> Repartija -> AppHandler Repartija
handleRepartijaPost grupoId repartija = do
  runSelda (saveRepartija grupoId repartija)
