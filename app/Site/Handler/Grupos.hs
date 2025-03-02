module Site.Handler.Grupos
    ( CreateGrupoParams
    , handleCreateGrupo
    , handleCreateParticipante
    , handleDeleteParticipante
    , handleGetNetos
    , handleShowGrupo
    ) where

import BananaSplit (Grupo, Participante, calcularDeudasTotales, resolverDeudasNaif)
import BananaSplit.Persistence (addParticipante, createGrupo, deleteShallowParticipante, fetchGrupo)

import Data.ULID (ULID)

import Servant

import Site.Api
import Site.Handler.Utils

import Types


handleCreateGrupo :: CreateGrupoParams -> AppHandler Grupo
handleCreateGrupo CreateGrupoParams{grupoName} = do
  runBeam $ createGrupo grupoName

handleGetNetos :: ULID -> AppHandler Netos
handleGetNetos grupoId = do
  grupo <- runBeam (fetchGrupo grupoId)
    `orElseMay` throwJsonError err404 "Grupo no encontrado"

  let deudas = calcularDeudasTotales grupo
  pure $ Netos
    { netos = deudas
    , transaccionesParaSaldar = resolverDeudasNaif deudas
    }

handleDeleteParticipante :: ULID -> ULID -> AppHandler ULID
handleDeleteParticipante grupoId participanteId = do
  _ <- runBeam (deleteShallowParticipante grupoId participanteId)
  pure participanteId

handleShowGrupo :: ULID -> AppHandler Grupo
handleShowGrupo grupoId = do
  runBeam (fetchGrupo grupoId)
    `orElseMay` throwJsonError err404 "Grupo no encontrado"

handleCreateParticipante :: ULID -> ParticipanteAddParams -> AppHandler Participante
handleCreateParticipante grupoId ParticipanteAddParams{name} = do
  runBeam (addParticipante grupoId name)
    `orElse` (\_e -> throwJsonError err400 "falle")
