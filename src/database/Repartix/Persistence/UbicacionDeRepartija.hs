{-# LANGUAGE NoFieldSelectors #-}

module Repartix.Persistence.UbicacionDeRepartija (
  UbicacionDeRepartija (..),
) where

import Protolude

import Repartix.ULID (ULID)

-- | Es un record y no tres argumentos porque los tres son 'ULID': pasados en
-- otro orden compilarían igual.
data UbicacionDeRepartija = UbicacionDeRepartija
  { repartijaId :: ULID
  , pagoId :: ULID
  , grupoId :: ULID
  }
  deriving (Show, Eq, Generic)
