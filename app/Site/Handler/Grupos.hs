module Site.Handler.Grupos
    ( CreateGrupoParams
    , handleCreateGrupo
    , handleCreateParticipante
    , handleDeleteParticipante
    , handleGetNetos
    , handleShowGrupo
    ) where

import BananaSplit
import BananaSplit.Persistence (addParticipante, createGrupo, deleteShallowParticipante, fetchGrupo,
                                fetchPago, fetchPagos, fetchShallowPagos)

import Protolude

import Servant

import Site.Api
import Site.Handler.Utils

import Types


handleCreateGrupo :: CreateGrupoParams -> AppHandler Grupo
handleCreateGrupo CreateGrupoParams{grupoName} = do
  runBeam $ createGrupo grupoName

handleGetNetos :: ULID -> AppHandler ResumenGrupo
handleGetNetos grupoId = do
  shallowGrupo <- runBeam (fetchGrupo grupoId)
    `orElseMay` throwJsonError err404 "Grupo no encontrado"

  pagos <- runBeam $ do
    shallowPagos <- fetchShallowPagos grupoId
    forM shallowPagos $ \shallowPago -> do
      fetchPago grupoId shallowPago.pagoId

  let grupo = Grupo
        { id = shallowGrupo.id
        , participantes = shallowGrupo.participantes
        , nombre = shallowGrupo.nombre
        , pagos = pagos
        }
  let deudas = calcularDeudasTotales grupo
  pure $ ResumenGrupo
    { netos = Netos deudas
    , cantidadPagosInvalidos = length $ filter (not . (.isValid)) grupo.pagos
    , transaccionesParaSaldar = minimizeTransactions deudas
    }

handleDeleteParticipante :: ULID -> ULID -> AppHandler ULID
handleDeleteParticipante grupoId participanteId = do
  _ <- runBeam (deleteShallowParticipante grupoId participanteId)
  pure participanteId

handleShowGrupo :: ULID -> AppHandler ShallowGrupo
handleShowGrupo grupoId = do
  runBeam (fetchGrupo grupoId)
    `orElseMay` throwJsonError err404 "Grupo no encontrado"

handleCreateParticipante :: ULID -> ParticipanteAddParams -> AppHandler Participante
handleCreateParticipante grupoId ParticipanteAddParams{name} = do
  runBeam (addParticipante grupoId name)
    `Site.Handler.Utils.orElse` (\_e -> throwJsonError err400 "falle")
