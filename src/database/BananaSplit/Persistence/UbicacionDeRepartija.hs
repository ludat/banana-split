{-# LANGUAGE NoFieldSelectors #-}

module BananaSplit.Persistence.UbicacionDeRepartija (
  UbicacionDeRepartija (..),
) where

import Protolude

import BananaSplit.ULID (ULID)

-- | Es un record y no tres argumentos porque los tres son 'ULID': pasados en
-- otro orden compilarían igual.
data UbicacionDeRepartija = UbicacionDeRepartija
  { repartijaId :: ULID
  , pagoId :: ULID
  , grupoId :: ULID
  }
  deriving (Show, Eq, Generic)
