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
  ConteoDeGastos (..),
  addParticipante,
  claimParticipante,
  contarGastos,
  createGrupo,
  createGrupoForUser,
  deleteShallowParticipante,
  fetchGrupo,
  fetchGruposForUser,
  fetchTasasDeCambio,
  fetchTransferencias,
  freezeGrupo,
  guardarTasasDeCambio,
  netosDeGrupo,
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
  runBeamWrite $ createGrupo grupoName grupoParticipante

handleCreateGrupoAsUser :: User -> CreateGrupoAsUserParams -> AppHandler Grupo
handleCreateGrupoAsUser user CreateGrupoAsUserParams{grupoName} = do
  runBeamWrite $ createGrupoForUser grupoName user

netosPendientes :: PorMoneda (Netos Monto) -> PorMoneda [Transferencia] -> PorMoneda (Netos Monto)
netosPendientes netosDeGastos hechas =
  netosDeGastos <> netosDeTransferencias hechas

netosConSaldo :: PorMoneda (Netos Monto) -> PorMoneda (Netos Monto)
netosConSaldo = filterPorMoneda ((> 0) . deudoresNoNulos)

-- | Todo lo que hace es leer y sumar para mostrar, así que va en
-- 'SoloLectura': la suma de los netos recorre una fila por gasto y
-- participante, que en un grupo grande es justo lo que menos conviene andar
-- marcando con predicate locks.
handleGetNetos :: ULID -> AppHandler ResumenGrupo
handleGetNetos grupoId = do
  shallowGrupo <-
    runBeamFastRead (fetchGrupo grupoId)
      `orElseMay` throwJsonError err404 "Grupo no encontrado"

  case shallowGrupo.congeladoAt of
    Just _ -> do
      guardadas <- runBeamFastRead $ fetchTransferencias grupoId
      pure $
        GrupoCongelado
          ResumenCongelado
            { transferenciasParaSaldar = transferenciasPendientes guardadas
            , transferenciasHechas = transferenciasHechas guardadas
            }
    Nothing -> do
      guardadas <- runBeamFastRead $ fetchTransferencias grupoId
      netosDeGastos <- runBeamFastRead $ netosDeGrupo grupoId
      conteo <- runBeamFastRead $ contarGastos grupoId

      let netos =
            netosPendientes netosDeGastos (transferenciasHechas guardadas & fmap (fmap (.transferencia)))
      let tabla = tablaDeTasas shallowGrupo.monedaPorDefecto shallowGrupo.tasasDeCambio

      pure $
        GrupoAbierto
          ResumenAbierto
            { netos = netos
            , consolidado = consolidarNetos tabla (netosConSaldo netos)
            , cantidadPagos = conteo.total
            , cantidadPagosInvalidos = conteo.invalidos
            , transferenciasHechas = transferenciasHechas guardadas
            }

handleDeleteParticipante :: ULID -> ULID -> AppHandler ULID
handleDeleteParticipante grupoId participanteId = do
  _ <- runBeamWrite (deleteShallowParticipante grupoId participanteId)
  pure participanteId

handleShowGrupo :: ULID -> AppHandler ShallowGrupo
handleShowGrupo grupoId = do
  runBeamFastRead (fetchGrupo grupoId)
    `orElseMay` throwJsonError err404 "Grupo no encontrado"

handleCreateParticipante :: ULID -> ParticipanteAddParams -> AppHandler Participante
handleCreateParticipante grupoId ParticipanteAddParams{name} = do
  runBeamWrite (addParticipante grupoId name)
    `Site.Handler.Utils.orElse` (\_e -> throwJsonError err400 "falle")

handleGetMisGrupos :: User -> AppHandler [GrupoParaUsuario]
handleGetMisGrupos user = do
  runBeamFastRead $ fetchGruposForUser user.id

handleClaimParticipante :: User -> ULID -> ULID -> AppHandler ClaimParticipanteResult
handleClaimParticipante user grupoId participanteId = do
  result <- runBeamWrite $ claimParticipante grupoId participanteId user.id
  pure $ case result of
    Left rejection -> ClaimRejected rejection
    Right participante -> ClaimAccepted participante

handleUnclaimParticipante :: User -> ULID -> ULID -> AppHandler Participante
handleUnclaimParticipante user grupoId participanteId = do
  runBeamWrite $ unclaimParticipante grupoId participanteId user.id

handleFreezeGrupo :: ULID -> AppHandler ShallowGrupo
handleFreezeGrupo grupoId = do
  shallowGrupo <-
    runBeamFastRead (fetchGrupo grupoId)
      `orElseMay` throwJsonError err404 "Grupo no encontrado"

  netosDeGastos <- runBeamFastRead $ netosDeGrupo grupoId
  guardadas <- runBeamFastRead $ fetchTransferencias grupoId
  tasasDeCambio <- runBeamFastRead $ fetchTasasDeCambio grupoId

  let netos = netosPendientes netosDeGastos (transferenciasHechas guardadas <&> fmap (.transferencia))
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

  runBeamWrite
    ( do
        freezeGrupo grupoId shallowGrupo.monedaPorDefecto (minimizeTransactions consolidado.netos)
        fetchGrupo grupoId
    )
    `orElseMay` throwJsonError err404 "Grupo no encontrado"

handleUnfreezeGrupo :: ULID -> AppHandler ShallowGrupo
handleUnfreezeGrupo grupoId = do
  runBeamWrite
    ( do
        unfreezeGrupo grupoId
        fetchGrupo grupoId
    )
    `orElseMay` throwJsonError err404 "Grupo no encontrado"

handleUpdateGrupo :: ULID -> UpdateGrupoParams -> AppHandler ShallowGrupo
handleUpdateGrupo grupoId params = do
  shallowGrupo <-
    runBeamFastRead (fetchGrupo grupoId)
      `orElseMay` throwJsonError err404 "Grupo no encontrado"

  -- Las transferencias congeladas están en la moneda por defecto de cuando se
  -- congeló: cambiarla ahora las dejaría hablando de otra moneda. El nombre sí
  -- se puede cambiar.
  when (estaCongelado shallowGrupo && params.monedaPorDefecto /= shallowGrupo.monedaPorDefecto) $
    throwJsonError err423 "El grupo está congelado"

  runBeamWrite
    ( do
        updateGrupo grupoId params.nombre params.monedaPorDefecto
        fetchGrupo grupoId
    )
    `orElseMay` throwJsonError err404 "Grupo no encontrado"

handleGuardarTasasDeCambio :: ULID -> Moneda -> [TasaDeCambio] -> AppHandler [TasaDeCambio]
handleGuardarTasasDeCambio grupoId moneda tasas = do
  shallowGrupo <-
    runBeamFastRead (fetchGrupo grupoId)
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

  runBeamWrite $ guardarTasasDeCambio grupoId moneda tasas
