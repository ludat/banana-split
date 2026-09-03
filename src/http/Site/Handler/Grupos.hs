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
  handleGuardarTasasDeCambio,
  handleShowGrupo,
  handleUnclaimParticipante,
  handleUnfreezeGrupo,
  handleUpdateGrupo,
) where

import Data.Text qualified as Text
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
  fetchTasasDeCambio,
  fetchTransferencias,
  freezeGrupo,
  guardarTasasDeCambio,
  transferenciasHechas,
  transferenciasPendientes,
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

netosPendientes :: Grupo -> PorMoneda [Transferencia] -> PorMoneda (Netos Monto)
netosPendientes grupo hechas =
  calcularNetosTotales grupo <> netosDeTransferencias hechas

netosConSaldo :: PorMoneda (Netos Monto) -> PorMoneda (Netos Monto)
netosConSaldo = filterPorMoneda ((> 0) . deudoresNoNulos)

handleGetNetos :: ULID -> AppHandler ResumenGrupo
handleGetNetos grupoId = do
  shallowGrupo <-
    runBeam (fetchGrupo grupoId)
      `orElseMay` throwJsonError err404 "Grupo no encontrado"

  case shallowGrupo.congeladoAt of
    Just _ -> do
      guardadas <- runBeam $ fetchTransferencias grupoId
      pure $
        GrupoCongelado
          ResumenCongelado
            { transferenciasParaSaldar = transferenciasPendientes guardadas
            , transferenciasHechas = transferenciasHechas guardadas
            }
    Nothing -> do
      guardadas <- runBeam $ fetchTransferencias grupoId
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

      let netos =
            netosPendientes grupo (transferenciasHechas guardadas & fmap (fmap (.transferencia)))
      let tabla = tablaDeTasas shallowGrupo.monedaPorDefecto shallowGrupo.tasasDeCambio

      pure $
        GrupoAbierto
          ResumenAbierto
            { netos = netos
            , consolidado = consolidarNetos tabla (netosConSaldo netos)
            , cantidadPagos = length grupo.pagos
            , cantidadPagosInvalidos = length $ filter (not . (.isValid)) grupo.pagos
            , transferenciasHechas = transferenciasHechas guardadas
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

handleGetMisGrupos :: User -> AppHandler [GrupoParaUsuario]
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
  guardadas <- runBeam $ fetchTransferencias grupoId
  tasasDeCambio <- runBeam $ fetchTasasDeCambio grupoId

  let netos = netosPendientes grupo (transferenciasHechas guardadas <&> fmap (.transferencia))
  let consolidado =
        consolidarNetos
          (tablaDeTasas shallowGrupo.monedaPorDefecto tasasDeCambio)
          (netosConSaldo netos)

  -- Congelar deja una sola tanda de transferencias en la moneda por defecto, así
  -- que sin la tasa de alguna de las monedas del grupo no hay nada que congelar.
  -- El error dice cuáles faltan porque el front puede mandar a cargarlas.
  unless (null consolidado.monedasSinTasa) $
    throwJsonError err409 $
      "Faltan las tasas de cambio de: "
        <> Text.intercalate ", " (fmap show consolidado.monedasSinTasa)

  runBeam
    ( do
        freezeGrupo grupoId shallowGrupo.monedaPorDefecto (minimizeTransactions consolidado.netos)
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
  shallowGrupo <-
    runBeam (fetchGrupo grupoId)
      `orElseMay` throwJsonError err404 "Grupo no encontrado"

  -- Las transferencias congeladas están en la moneda por defecto de cuando se
  -- congeló: cambiarla ahora las dejaría hablando de otra moneda. El nombre sí
  -- se puede cambiar.
  when (estaCongelado shallowGrupo && params.monedaPorDefecto /= shallowGrupo.monedaPorDefecto) $
    throwJsonError err423 "El grupo está congelado"

  runBeam
    ( do
        updateGrupo grupoId params.nombre params.monedaPorDefecto
        fetchGrupo grupoId
    )
    `orElseMay` throwJsonError err404 "Grupo no encontrado"

handleGuardarTasasDeCambio :: ULID -> Moneda -> [TasaDeCambio] -> AppHandler [TasaDeCambio]
handleGuardarTasasDeCambio grupoId moneda tasas = do
  shallowGrupo <-
    runBeam (fetchGrupo grupoId)
      `orElseMay` throwJsonError err404 "Grupo no encontrado"

  -- La tasa es lo que fija las deudas al congelar, así que cambiarla después
  -- dejaría las transferencias guardadas hablando de otro tipo de cambio.
  when (estaCongelado shallowGrupo) $
    throwJsonError err423 "El grupo está congelado"

  -- La tabla usa una tasa por moneda y descarta la que no le sirve a 'moneda':
  -- la ajena, la de una moneda consigo misma, la que tiene un lado en cero y la
  -- repetida. Si sobró alguna es que algo de eso pasó; cuál no lo decimos
  -- porque el front no llega a ver el cuerpo del 400.
  unless (cantidadDeTasas (tablaDeTasas moneda tasas) == length tasas) $
    throwJsonError err400 "Alguna de las tasas de cambio no es válida"

  runBeam $ guardarTasasDeCambio grupoId moneda tasas
