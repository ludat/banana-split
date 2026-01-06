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
                                fetchPago, fetchShallowPagos, updateIsValidPago)

import Protolude

import Servant

import Site.Api
import Site.Handler.Utils
import Site.Types


handleCreateGrupo :: CreateGrupoParams -> AppHandler Grupo
handleCreateGrupo CreateGrupoParams{grupoName, grupoParticipante} = do
  runBeam $ createGrupo grupoName grupoParticipante

handleGetNetos :: ULID -> AppHandler ResumenGrupo
handleGetNetos grupoId = do
  shallowGrupo <- runBeam (fetchGrupo grupoId)
    `orElseMay` throwJsonError err404 "Grupo no encontrado"

  pagos <- runBeam $ do
    shallowPagos <- fetchShallowPagos grupoId
    forM shallowPagos $ \shallowPago -> do
      pago <- fetchPago grupoId shallowPago.pagoId
      updateIsValidPago (pago & addIsValidPago)


  let grupo = Grupo
        { id = shallowGrupo.id
        , participantes = shallowGrupo.participantes
        , nombre = shallowGrupo.nombre
        , pagos = pagos
        }
  let netos = calcularNetosTotales grupo
  pure $ ResumenGrupo
    { netos = netos
    , cantidadPagosInvalidos = length $ filter (not . (.isValid)) grupo.pagos
    , cantidadPagos = length grupo.pagos
    , transaccionesParaSaldar = minimizeTransactions netos
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
