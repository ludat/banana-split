module Site.Handler.Grupos (
  CreateGrupoParams,
  handleAddMeAsParticipante,
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
  handleUpdateParticipante,
) where

import Data.Text qualified as Text
import Protolude
import Servant

import BananaSplit
import BananaSplit.Persistence (
  CongelamientoRechazado (..),
  ConteoDeGastos (..),
  EdicionRechazada (..),
  addParticipante,
  addParticipanteForUser,
  claimParticipante,
  congelarGrupo,
  contarGastos,
  createGrupo,
  createGrupoForUser,
  deleteShallowParticipante,
  editarParticipante,
  fetchGrupo,
  fetchGruposForUser,
  fetchTransferencias,
  guardarTasasDeCambio,
  netosDeGrupo,
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

-- | Todo lo que hace es leer y sumar para mostrar, así que va en
-- 'SoloLectura': la suma de los netos recorre una fila por gasto y
-- participante, que en un grupo grande es justo lo que menos conviene andar
-- marcando con predicate locks.
handleGetNetos :: ULID -> AppHandler ResumenGrupo
handleGetNetos grupoId = do
  grupo <-
    runBeamFastRead (fetchGrupo grupoId)
      `orElseMay` throwJsonError err404 "Grupo no encontrado"

  case grupo.congeladoAt of
    Just _ -> do
      transferencias <- runBeamFastRead $ fetchTransferencias grupoId
      pure $ GrupoCongelado ResumenCongelado{transferencias = transferencias}
    Nothing -> do
      transferencias <- runBeamFastRead $ fetchTransferencias grupoId
      netosDeGastos <- runBeamFastRead $ netosDeGrupo grupoId
      conteo <- runBeamFastRead $ contarGastos grupoId

      let netos = netosDeGastos <> netosDeTransferencias transferencias
      let tabla = tablaDeTasas grupo.monedaPorDefecto grupo.tasasDeCambio

      pure $
        GrupoAbierto
          ResumenAbierto
            { netos = netos
            , consolidado = consolidarNetos tabla (netosConSaldo netos)
            , cantidadPagos = conteo.total
            , cantidadPagosInvalidos = conteo.invalidos
            , transferencias = transferencias
            }

handleDeleteParticipante :: ULID -> ULID -> AppHandler ULID
handleDeleteParticipante grupoId participanteId = do
  _ <-
    runBeamWrite (deleteShallowParticipante grupoId participanteId)
      `onForeignKeyViolation` throwJsonError
        err409
        "Este participante está siendo usado, no se puede borrar hasta que nadie lo referencie"
  pure participanteId

handleShowGrupo :: ULID -> AppHandler Grupo
handleShowGrupo grupoId = do
  runBeamFastRead (fetchGrupo grupoId)
    `orElseMay` throwJsonError err404 "Grupo no encontrado"

handleCreateParticipante :: ULID -> ParticipanteAddParams -> AppHandler Participante
handleCreateParticipante grupoId ParticipanteAddParams{name} = do
  runBeamWrite (addParticipante grupoId name)
    `Site.Handler.Utils.orElse` (\_e -> throwJsonError err400 "falle")

handleUpdateParticipante :: ULID -> ULID -> ParticipanteUpdateParams -> AppHandler Participante
handleUpdateParticipante grupoId participanteId params = do
  let nombre = Text.strip params.nombre
  when (Text.null nombre) $
    throwJsonError err400 "El nombre no puede estar vacío"

  -- Vacío es lo mismo que no haberlo cargado.
  let datosDeCobro = mfilter (not . Text.null) (Text.strip <$> params.datosDeCobro)

  resultado <- runBeamWrite $ editarParticipante grupoId participanteId nombre datosDeCobro
  case resultado of
    Right participante -> pure participante
    Left ParticipanteNoExiste -> throwJsonError err404 "Participante no encontrado"
    Left NombreRepetido -> throwJsonError err409 "Ya hay un participante con ese nombre"

handleGetMisGrupos :: User -> AppHandler [GrupoParaUsuario]
handleGetMisGrupos user = do
  runBeamFastRead $ fetchGruposForUser user.id

handleClaimParticipante :: User -> ULID -> ULID -> AppHandler ClaimParticipanteResult
handleClaimParticipante user grupoId participanteId = do
  result <- runBeamWrite $ claimParticipante grupoId participanteId user.id
  pure $ case result of
    Left rejection -> ClaimRejected rejection
    Right participante -> ClaimAccepted participante

handleAddMeAsParticipante :: User -> ULID -> ParticipanteAddParams -> AppHandler ClaimParticipanteResult
handleAddMeAsParticipante user grupoId ParticipanteAddParams{name} = do
  when (Text.null (Text.strip name)) $
    throwJsonError err400 "El nombre no puede estar vacío"
  result <- runBeamWrite $ addParticipanteForUser grupoId (Text.strip name) user.id
  pure $ case result of
    Left rejection -> ClaimRejected rejection
    Right participante -> ClaimAccepted participante

handleUnclaimParticipante :: User -> ULID -> ULID -> AppHandler Participante
handleUnclaimParticipante user grupoId participanteId = do
  runBeamWrite $ unclaimParticipante grupoId participanteId user.id

handleFreezeGrupo :: ULID -> AppHandler Grupo
handleFreezeGrupo grupoId = do
  resultado <- runBeamWrite $ congelarGrupo grupoId

  case resultado of
    Right grupo -> pure grupo
    Left GrupoNoExiste -> throwJsonError err404 "Grupo no encontrado"
    Left (HayGastosInvalidos cantidad) ->
      throwJsonError err409 $
        if cantidad == 1
          then "Hay 1 gasto inválido, arreglalo antes de congelar el grupo"
          else "Hay " <> show cantidad <> " gastos inválidos, arreglalos antes de congelar el grupo"
    -- El error dice qué monedas faltan porque el front puede mandar a cargarlas.
    Left (FaltanTasasDeCambio monedas) ->
      throwJsonError err409 $
        "Faltan las tasas de cambio de: "
          <> Text.intercalate ", " (fmap show monedas)

handleUnfreezeGrupo :: ULID -> AppHandler Grupo
handleUnfreezeGrupo grupoId = do
  runBeamWrite
    ( do
        unfreezeGrupo grupoId
        fetchGrupo grupoId
    )
    `orElseMay` throwJsonError err404 "Grupo no encontrado"

handleUpdateGrupo :: ULID -> UpdateGrupoParams -> AppHandler Grupo
handleUpdateGrupo grupoId params = do
  grupo <-
    runBeamFastRead (fetchGrupo grupoId)
      `orElseMay` throwJsonError err404 "Grupo no encontrado"

  -- Las transferencias congeladas están en la moneda por defecto de cuando se
  -- congeló: cambiarla ahora las dejaría hablando de otra moneda. El nombre sí
  -- se puede cambiar.
  when (estaCongelado grupo && params.monedaPorDefecto /= grupo.monedaPorDefecto) $
    throwJsonError err423 "El grupo está congelado"

  runBeamWrite
    ( do
        updateGrupo grupoId params.nombre params.monedaPorDefecto
        fetchGrupo grupoId
    )
    `orElseMay` throwJsonError err404 "Grupo no encontrado"

handleGuardarTasasDeCambio :: ULID -> Moneda -> [TasaDeCambio] -> AppHandler [TasaDeCambio]
handleGuardarTasasDeCambio grupoId moneda tasas = do
  grupo <-
    runBeamFastRead (fetchGrupo grupoId)
      `orElseMay` throwJsonError err404 "Grupo no encontrado"

  -- La tasa es lo que fija las deudas al congelar, así que cambiarla después
  -- dejaría las transferencias guardadas hablando de otro tipo de cambio.
  when (estaCongelado grupo) $
    throwJsonError err423 "El grupo está congelado"

  -- La tabla usa una tasa por moneda y descarta la que no le sirve a 'moneda':
  -- la ajena, la de una moneda consigo misma, la que tiene un lado en cero y la
  -- repetida. Si sobró alguna es que algo de eso pasó; cuál no lo decimos
  -- porque el front no llega a ver el cuerpo del 400.
  unless (cantidadDeTasas (tablaDeTasas moneda tasas) == length tasas) $
    throwJsonError err400 "Alguna de las tasas de cambio no es válida"

  runBeamWrite $ guardarTasasDeCambio grupoId moneda tasas
