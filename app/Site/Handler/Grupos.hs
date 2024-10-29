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

import Control.Monad.Reader

import Data.Pool qualified as Pool
import Data.ULID (ULID)

import Database.Selda.Backend
import Database.Selda.PostgreSQL (PG)

import Servant

import Site.Api
import Site.Handler.Utils (orElse, orElseMay, throwJsonError)

import Types


handleCreateGrupo :: CreateGrupoParams -> AppHandler Grupo
handleCreateGrupo CreateGrupoParams{grupoName} = do
  runSelda $ createGrupo grupoName

handleGetNetos :: ULID -> AppHandler Netos
handleGetNetos grupoId = do
  grupo <- runSelda (fetchGrupo grupoId)
    `orElseMay` throwJsonError err404 "Grupo no encontrado"

  let deudas = calcularDeudasTotales grupo
  pure $ Netos
    { netos = deudas
    , transaccionesParaSaldar = resolverDeudasNaif deudas
    }

handleDeleteParticipante :: ULID -> ULID -> AppHandler ULID
handleDeleteParticipante grupoId participanteId = do
  runSelda (deleteShallowParticipante grupoId participanteId)
  pure participanteId

handleShowGrupo :: ULID -> AppHandler Grupo
handleShowGrupo grupoId = do
  runSelda (fetchGrupo grupoId)
    `orElseMay` throwJsonError err404 "Grupo no encontrado"

handleCreateParticipante :: ULID -> ParticipanteAddParams -> AppHandler Participante
handleCreateParticipante grupoId ParticipanteAddParams{name} = do
  runSelda (addParticipante grupoId name)
    `orElse` (\_e -> throwJsonError err400 "falle")

runSelda :: SeldaT PG IO a -> AppHandler a
runSelda dbAction = do
  pool <- asks (.connection)

  liftIO $ Pool.withResource pool $ \seldaConn -> do
    runSeldaT dbAction seldaConn
