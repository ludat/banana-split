{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Site.Handler.Repartijas (
  handleRepartijaClaimDelete,
  handleRepartijaClaimPut,
  handleRepartijaGet,
) where

import Servant (err404)

import BananaSplit (RepartijaClaim (..), RepartijaForFrontend (..))
import BananaSplit.Persistence
import BananaSplit.ULID (ULID)
import Preludat
import Site.Handler.Utils
import Site.Types

handleRepartijaGet :: ULID -> AppHandler RepartijaForFrontend
handleRepartijaGet repartijaId = do
  runBeamFastRead (fetchRepartija repartijaId)

handleRepartijaClaimPut :: ULID -> RepartijaClaim -> AppHandler RepartijaForFrontend
handleRepartijaClaimPut repartijaId repartijaClaim =
  ( runBeamWrite
      ( fetchGrupoIdFromRepartija repartijaId
          >>= traverse
            (\grupoId -> withGrupoDescongelado grupoId $ saveRepartijaClaim repartijaId repartijaClaim)
      )
      `orElseMay` throwJsonError err404 "Repartija no encontrada"
  )
    `orElse` throwGrupoNoEditable

handleRepartijaClaimDelete :: ULID -> AppHandler RepartijaForFrontend
handleRepartijaClaimDelete claimId =
  ( runBeamWrite
      ( fetchGrupoIdFromClaim claimId
          >>= traverse (\grupoId -> withGrupoDescongelado grupoId $ deleteRepartijaClaim claimId)
      )
      `orElseMay` throwJsonError err404 "Claim no encontrado"
  )
    `orElse` throwGrupoNoEditable
