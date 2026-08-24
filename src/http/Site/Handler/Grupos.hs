module Site.Handler.Grupos (
  CreateGrupoParams,
  handleClaimParticipante,
  handleConsolidacionPreview,
  handleCreateGrupo,
  handleCreateGrupoAsUser,
  handleCreateParticipante,
  handleDeleteParticipante,
  handleFreezeGrupo,
  handleGetMetricas,
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
  fetchCotizacionesCongeladas,
  fetchGrupo,
  fetchGruposForUser,
  fetchPago,
  fetchShallowPagos,
  fetchTransaccionesCongeladas,
  freezeGrupo,
  saveCotizacionesCongeladas,
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

-- | El grupo completo con sus pagos, o 404 si no existe.
fetchGrupoCompleto :: ULID -> AppHandler (ShallowGrupo, Grupo)
fetchGrupoCompleto grupoId = do
  shallowGrupo <-
    runBeam (fetchGrupo grupoId)
      `orElseMay` throwJsonError err404 "Grupo no encontrado"

  pagos <- runBeam $ do
    shallowPagos <- fetchShallowPagos grupoId
    forM shallowPagos $ \shallowPago ->
      fetchPago shallowPago.pagoId

  pure
    ( shallowGrupo
    , Grupo
        { id = shallowGrupo.id
        , participantes = shallowGrupo.participantes
        , nombre = shallowGrupo.nombre
        , pagos = pagos
        , monedaPorDefecto = shallowGrupo.monedaPorDefecto
        }
    )

handleGetNetos :: ULID -> AppHandler ResumenGrupo
handleGetNetos grupoId = do
  (shallowGrupo, grupo) <- fetchGrupoCompleto grupoId
  let netos = calcularNetosTotales grupo

  if shallowGrupo.isFrozen
    then do
      transacciones <- runBeam $ fetchTransaccionesCongeladas grupoId
      cotizaciones <- runBeam $ fetchCotizacionesCongeladas grupoId
      pure $
        ResumenGrupo
          { netos = netos
          , cantidadPagosInvalidos = length $ filter (not . (.isValid)) grupo.pagos
          , cantidadPagos = length grupo.pagos
          , transaccionesParaSaldar = transacciones
          , isFrozen = True
          , consolidacion = resumenConsolidadoCongelado grupo cotizaciones netos transacciones
          }
    else
      pure $
        ResumenGrupo
          { netos = netos
          , cantidadPagosInvalidos = length $ filter (not . (.isValid)) grupo.pagos
          , cantidadPagos = length grupo.pagos
          , transaccionesParaSaldar = fmap minimizeTransactions netos
          , isFrozen = False
          , consolidacion = Nothing
          }

-- | Arma la vista consolidada de un grupo congelado con cotizaciones
-- guardadas. Los netos se recalculan de los pagos vivos usando esas
-- cotizaciones; las transacciones son las congeladas. Si no hay cotizaciones
-- el freeze fue por moneda; si la conversión falla (defensivo: una moneda
-- nueva sin cotización guardada) se muestra la vista por moneda.
resumenConsolidadoCongelado ::
  Grupo ->
  PorMoneda Monto ->
  PorMoneda (Netos Monto) ->
  PorMoneda [Transaccion] ->
  Maybe ResumenConsolidado
resumenConsolidadoCongelado grupo cotizaciones netos transacciones = do
  guard (cotizaciones /= mempty)
  netosConsolidados <- rightToMaybe $ consolidarNetos grupo.monedaPorDefecto cotizaciones netos
  pure $
    ResumenConsolidado
      { moneda = grupo.monedaPorDefecto
      , cotizaciones = cotizaciones
      , netos = netosConsolidados
      , transaccionesParaSaldar = runIdentity $ forMonedaM transacciones $ \_moneda ts -> pure ts
      }

handleConsolidacionPreview :: ULID -> ConsolidacionParams -> AppHandler ResumenConsolidado
handleConsolidacionPreview grupoId params = do
  (_shallowGrupo, grupo) <- fetchGrupoCompleto grupoId
  let netos = calcularNetosTotales grupo
  case consolidarNetos grupo.monedaPorDefecto params.cotizaciones netos of
    Left e -> throwJsonError err400 $ errorConsolidacionMessage e
    Right netosConsolidados ->
      pure $
        ResumenConsolidado
          { moneda = grupo.monedaPorDefecto
          , cotizaciones = params.cotizaciones
          , netos = netosConsolidados
          , transaccionesParaSaldar = minimizeTransactions netosConsolidados
          }

errorConsolidacionMessage :: ErrorConsolidacion -> Text
errorConsolidacionMessage = \case
  CotizacionFaltante moneda ->
    "Falta la cotización para " <> show moneda
  CotizacionInvalida moneda ->
    "La cotización para " <> show moneda <> " tiene que ser mayor a cero"

handleGetMetricas :: ULID -> AppHandler MetricasGrupo
handleGetMetricas grupoId = do
  (_shallowGrupo, grupo) <- fetchGrupoCompleto grupoId
  pure $ calcularMetricas grupo

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

handleFreezeGrupo :: ULID -> FreezeParams -> AppHandler ShallowGrupo
handleFreezeGrupo grupoId params = do
  (_shallowGrupo, grupo) <- fetchGrupoCompleto grupoId
  let netos = calcularNetosTotales grupo

  if params.cotizaciones == mempty
    then
      runBeam
        ( do
            freezeGrupo grupoId (fmap minimizeTransactions netos)
            fetchGrupo grupoId
        )
        `orElseMay` throwJsonError err404 "Grupo no encontrado"
    else
      case consolidarNetos grupo.monedaPorDefecto params.cotizaciones netos of
        Left e -> throwJsonError err400 $ errorConsolidacionMessage e
        Right netosConsolidados ->
          runBeam
            ( do
                freezeGrupo grupoId (minimizeTransactions netosConsolidados `enMoneda` grupo.monedaPorDefecto)
                saveCotizacionesCongeladas grupoId params.cotizaciones
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
