{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Site.Handler.Repartijas (
  handleRepartijaAccion,
  handleRepartijaAccionPreview,
  handleRepartijaClaimDelete,
  handleRepartijaClaimPut,
  handleRepartijaGet,
  handleRepartijaNoConsumioDelete,
  handleRepartijaNoConsumioPut,
) where

import Servant (err404)

import BananaSplit (
  AccionDeRepartija,
  ParticipanteId (..),
  RepartijaClaim (..),
  RepartijaForFrontend (..),
  detalleRepartija,
 )
import BananaSplit.AccionDeRepartija qualified as AccionDeRepartija
import BananaSplit.Persistence
import BananaSplit.ULID (ULID)
import BananaSplit.ULID qualified as ULID
import Preludat
import Site.Api (
  PedidoDePreviewDeAccion (..),
  PreviewDeAccion (..),
  ResultadoAccionDeRepartija (..),
  ResultadoPreviewDeAccion (..),
 )
import Site.Handler.Utils
import Site.Types

handleRepartijaGet :: ULID -> AppHandler RepartijaForFrontend
handleRepartijaGet repartijaId = do
  runBeamFastRead (fetchRepartija repartijaId)

handleRepartijaClaimPut :: ULID -> RepartijaClaim -> AppHandler RepartijaForFrontend
handleRepartijaClaimPut repartijaId repartijaClaim =
  runBeamWrite
    ( do
        maybeGrupoId <- fetchGrupoIdFromRepartija repartijaId
        case maybeGrupoId of
          Nothing -> pure Nothing
          Just grupoId ->
            Just <$> withGrupoDescongelado grupoId (saveRepartijaClaim repartijaId repartijaClaim)
    )
    & orElseNoEncontradoONoEditable "Repartija no encontrada"

handleRepartijaClaimDelete :: ULID -> AppHandler RepartijaForFrontend
handleRepartijaClaimDelete claimId =
  runBeamWrite
    ( do
        maybeGrupoId <- fetchGrupoIdFromClaim claimId
        case maybeGrupoId of
          Nothing -> pure Nothing
          Just grupoId -> Just <$> withGrupoDescongelado grupoId (deleteRepartijaClaim claimId)
    )
    & orElseNoEncontradoONoEditable "Claim no encontrado"

handleRepartijaNoConsumioPut :: ULID -> ULID -> AppHandler RepartijaForFrontend
handleRepartijaNoConsumioPut repartijaId participanteId =
  runBeamWrite
    ( do
        maybeGrupoId <- fetchGrupoIdFromRepartija repartijaId
        case maybeGrupoId of
          Nothing -> pure Nothing
          Just grupoId ->
            Just <$> withGrupoDescongelado grupoId (marcarNoConsumio repartijaId (ParticipanteId participanteId))
    )
    & orElseNoEncontradoONoEditable "Repartija no encontrada"

handleRepartijaNoConsumioDelete :: ULID -> ULID -> AppHandler RepartijaForFrontend
handleRepartijaNoConsumioDelete repartijaId participanteId =
  runBeamWrite
    ( do
        maybeGrupoId <- fetchGrupoIdFromRepartija repartijaId
        case maybeGrupoId of
          Nothing -> pure Nothing
          Just grupoId ->
            Just <$> withGrupoDescongelado grupoId (desmarcarNoConsumio repartijaId (ParticipanteId participanteId))
    )
    & orElseNoEncontradoONoEditable "Repartija no encontrada"

handleRepartijaAccionPreview :: PedidoDePreviewDeAccion -> AppHandler ResultadoPreviewDeAccion
handleRepartijaAccionPreview pedido = do
  nuevoItemId <- liftIO ULID.getULID
  pure
    $ case AccionDeRepartija.aplicarAccionDeRepartija nuevoItemId pedido.accion pedido.repartija of
      Left motivo -> PreviewDeAccionRechazada motivo
      Right repartija ->
        PreviewDeAccionAceptada
          PreviewDeAccion{repartija = repartija, detalle = detalleRepartija repartija}

handleRepartijaAccion :: ULID -> AccionDeRepartija -> AppHandler ResultadoAccionDeRepartija
handleRepartijaAccion repartijaId accion =
  runBeamWrite
    ( do
        maybeUbicacion <- fetchUbicacionDeRepartija repartijaId
        case maybeUbicacion of
          Nothing -> pure Nothing
          Just ubicacion ->
            Just <$> withGrupoDescongelado ubicacion.grupoId (aplicarAccionDeRepartija ubicacion accion)
    )
    & orElseNoEncontradoONoEditable "Repartija no encontrada"
    <&> either AccionDeRepartijaRechazada AccionDeRepartijaAceptada

orElseNoEncontradoONoEditable ::
  Text -> AppHandler (Maybe (Either GrupoNoEditable a)) -> AppHandler a
orElseNoEncontradoONoEditable mensaje resultado =
  (resultado `orElseMay` throwJsonError err404 mensaje)
    `orElse` throwGrupoNoEditable
