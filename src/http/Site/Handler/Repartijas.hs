{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Site.Handler.Repartijas (
  handleRepartijaClaimDelete,
  handleRepartijaClaimPut,
  handleRepartijaGet,
) where

import Protolude
import Servant (err404)

import BananaSplit (RepartijaClaim (..), RepartijaForFrontend (..), estaCongelado)
import BananaSplit.Persistence
import BananaSplit.ULID (ULID)
import Site.Handler.Utils
import Site.Types

handleRepartijaGet :: ULID -> AppHandler RepartijaForFrontend
handleRepartijaGet repartijaId = do
  runBeamFastRead (fetchRepartija repartijaId)

handleRepartijaClaimPut :: ULID -> RepartijaClaim -> AppHandler RepartijaForFrontend
handleRepartijaClaimPut repartijaId repartijaClaim = do
  maybeGrupoId <- runBeamFastRead (fetchGrupoIdFromRepartija repartijaId)
  case maybeGrupoId of
    Nothing -> throwJsonError err404 "Repartija no encontrada"
    Just grupoId -> do
      shallowGrupo <-
        runBeamFastRead (fetchGrupo grupoId)
          `orElseMay` throwJsonError err404 "Grupo no encontrado"
      when (estaCongelado shallowGrupo) $ throwJsonError err423 "El grupo está congelado"
  runBeamWrite (saveRepartijaClaim repartijaId repartijaClaim)

handleRepartijaClaimDelete :: ULID -> AppHandler Text
handleRepartijaClaimDelete claimId = do
  maybeGrupoId <- runBeamFastRead (fetchGrupoIdFromClaim claimId)
  case maybeGrupoId of
    Nothing -> throwJsonError err404 "Claim no encontrado"
    Just grupoId -> do
      shallowGrupo <-
        runBeamFastRead (fetchGrupo grupoId)
          `orElseMay` throwJsonError err404 "Grupo no encontrado"
      when (estaCongelado shallowGrupo) $ throwJsonError err423 "El grupo está congelado"
  void $ runBeamWrite (deleteRepartijaClaim claimId)
  pure "ok"
