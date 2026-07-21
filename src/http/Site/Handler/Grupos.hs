module Site.Handler.Grupos (
  CreateGrupoParams,
  handleClaimParticipante,
  handleCreateGrupo,
  handleCreateGrupoAsUser,
  handleCreateParticipante,
  handleDeleteParticipante,
  handleFreezeGrupo,
  handleGetMisGrupos,
  handleGetNetos,
  handleShowGrupo,
  handleUnclaimParticipante,
  handleUnfreezeGrupo,
  handleUpdateGrupo,
) where

import Protolude
import Servant

import BananaSplit
import BananaSplit.Persistence (
  addParticipante,
  claimParticipante,
  createGrupo,
  createGrupoForUser,
  deleteShallowParticipante,
  fetchGrupo,
  fetchGruposForUser,
  fetchPago,
  fetchShallowPagos,
  fetchTransaccionesCongeladas,
  freezeGrupo,
  unclaimParticipante,
  unfreezeGrupo,
  updateGrupo,
 )
import Site.Api
import Site.Handler.Utils
import Site.Types

handleCreateGrupo :: CreateGrupoParams -> AppHandler Grupo
handleCreateGrupo CreateGrupoParams{grupoName, grupoParticipante} = do
  runBeam $ createGrupo grupoName grupoParticipante

handleCreateGrupoAsUser :: User -> CreateGrupoAsUserParams -> AppHandler Grupo
handleCreateGrupoAsUser user CreateGrupoAsUserParams{grupoName} = do
  runBeam $ createGrupoForUser grupoName user

handleGetNetos :: ULID -> AppHandler ResumenGrupo
handleGetNetos grupoId = do
  shallowGrupo <-
    runBeam (fetchGrupo grupoId)
      `orElseMay` throwJsonError err404 "Grupo no encontrado"

  pagos <- runBeam $ do
    shallowPagos <- fetchShallowPagos grupoId
    forM shallowPagos $ \shallowPago ->
      fetchPago shallowPago.pagoId

  let grupo =
        Grupo
          { id = shallowGrupo.id
          , participantes = shallowGrupo.participantes
          , nombre = shallowGrupo.nombre
          , pagos = pagos
          , monedaPorDefecto = shallowGrupo.monedaPorDefecto
          }
  let netos = calcularNetosTotales grupo

  if shallowGrupo.isFrozen
    then do
      transacciones <- runBeam $ fetchTransaccionesCongeladas grupoId
      pure $
        ResumenGrupo
          { netos = netos
          , cantidadPagosInvalidos = length $ filter (not . (.isValid)) grupo.pagos
          , cantidadPagos = length grupo.pagos
          , transaccionesParaSaldar = transacciones
          , isFrozen = True
          }
    else
      pure $
        ResumenGrupo
          { netos = netos
          , cantidadPagosInvalidos = length $ filter (not . (.isValid)) grupo.pagos
          , cantidadPagos = length grupo.pagos
          , transaccionesParaSaldar = fmap minimizeTransactions netos
          , isFrozen = False
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

handleGetMisGrupos :: User -> AppHandler [ShallowGrupo]
handleGetMisGrupos user = do
  runBeam $ fetchGruposForUser user.id

handleClaimParticipante :: User -> ULID -> ULID -> AppHandler ClaimParticipanteResult
handleClaimParticipante user grupoId participanteId = do
  result <- runBeam $ claimParticipante grupoId participanteId user.id
  pure $ case result of
    Left rejection -> ClaimRejected rejection
    Right participante -> ClaimAccepted participante

handleUnclaimParticipante :: User -> ULID -> ULID -> AppHandler Participante
handleUnclaimParticipante user grupoId participanteId = do
  runBeam $ unclaimParticipante grupoId participanteId user.id

handleFreezeGrupo :: ULID -> AppHandler ShallowGrupo
handleFreezeGrupo grupoId = do
  shallowGrupo <-
    runBeam (fetchGrupo grupoId)
      `orElseMay` throwJsonError err404 "Grupo no encontrado"

  pagos <- runBeam $ do
    shallowPagos <- fetchShallowPagos grupoId
    forM shallowPagos $ \shallowPago ->
      fetchPago shallowPago.pagoId

  let grupo =
        Grupo
          { id = shallowGrupo.id
          , participantes = shallowGrupo.participantes
          , nombre = shallowGrupo.nombre
          , pagos = pagos
          , monedaPorDefecto = shallowGrupo.monedaPorDefecto
          }
  let netos = calcularNetosTotales grupo
  let transaccionesPorMoneda = fmap minimizeTransactions netos

  runBeam
    ( do
        freezeGrupo grupoId transaccionesPorMoneda
        fetchGrupo grupoId
    )
    `orElseMay` throwJsonError err404 "Grupo no encontrado"

handleUnfreezeGrupo :: ULID -> AppHandler ShallowGrupo
handleUnfreezeGrupo grupoId = do
  runBeam
    ( do
        unfreezeGrupo grupoId
        fetchGrupo grupoId
    )
    `orElseMay` throwJsonError err404 "Grupo no encontrado"

handleUpdateGrupo :: ULID -> UpdateGrupoParams -> AppHandler ShallowGrupo
handleUpdateGrupo grupoId params = do
  runBeam
    ( do
        updateGrupo grupoId params.nombre params.monedaPorDefecto
        fetchGrupo grupoId
    )
    `orElseMay` throwJsonError err404 "Grupo no encontrado"
