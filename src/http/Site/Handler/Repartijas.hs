{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Site.Handler.Repartijas
    ( handleRepartijaClaimDelete
    , handleRepartijaClaimPut
    , handleRepartijaGet
    ) where

import BananaSplit (Repartija (..), RepartijaClaim (..), ShallowGrupo (..))
import BananaSplit.Persistence
import BananaSplit.ULID (ULID)

import Protolude

import Servant (err404)

import Site.Handler.Utils
import Site.Types

handleRepartijaGet :: ULID -> AppHandler Repartija
handleRepartijaGet repartijaId = do
  runBeam (fetchRepartija repartijaId)

handleRepartijaClaimPut :: ULID -> RepartijaClaim -> AppHandler RepartijaClaim
handleRepartijaClaimPut repartijaId repartijaClaim = do
  maybeGrupoId <- runBeam (fetchGrupoIdFromRepartija repartijaId)
  case maybeGrupoId of
    Nothing -> throwJsonError err404 "Repartija no encontrada"
    Just grupoId -> do
      shallowGrupo <- runBeam (fetchGrupo grupoId)
        `orElseMay` throwJsonError err404 "Grupo no encontrado"
      when shallowGrupo.isFrozen $ throwJsonError err423 "El grupo está congelado"
  runBeam (saveRepartijaClaim repartijaId repartijaClaim)

handleRepartijaClaimDelete :: ULID -> AppHandler Text
handleRepartijaClaimDelete claimId = do
  maybeGrupoId <- runBeam (fetchGrupoIdFromClaim claimId)
  case maybeGrupoId of
    Nothing -> throwJsonError err404 "Claim no encontrado"
    Just grupoId -> do
      shallowGrupo <- runBeam (fetchGrupo grupoId)
        `orElseMay` throwJsonError err404 "Grupo no encontrado"
      when shallowGrupo.isFrozen $ throwJsonError err423 "El grupo está congelado"
  void $ runBeam (deleteRepartijaClaim claimId)
  pure "ok"
