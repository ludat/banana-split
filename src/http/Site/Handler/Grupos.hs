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
  fetchTransacciones,
  freezeGrupo,
  guardarTasasDeCambio,
  transaccionesHechas,
  transaccionesHechasDesde,
  transaccionesPendientes,
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

-- | Lo que falta mover de verdad: los netos de los gastos menos lo que ya se
-- transfirió. No es lo que se muestra —los netos que ve el usuario son solo de
-- los gastos— sino lo que se usa para decidir qué transacciones quedan. Sin
-- descontar lo transferido, un grupo que se congeló, se saldó y se descongeló
-- volvería a pedir las transferencias que ya se hicieron.
netosPendientes :: Grupo -> PorMoneda [Transaccion] -> PorMoneda (Netos Monto)
netosPendientes grupo hechas =
  calcularNetosTotales grupo <> netosDeTransacciones hechas

-- | Las monedas donde nadie quedó debiendo nada no necesitan tasa: no aportan
-- al consolidado, así que ni avisamos por ellas ni trabamos el congelamiento.
netosConSaldo :: PorMoneda (Netos Monto) -> PorMoneda (Netos Monto)
netosConSaldo = filterPorMoneda ((> 0) . deudoresNoNulos)

handleGetNetos :: ULID -> AppHandler ResumenGrupo
handleGetNetos grupoId = do
  shallowGrupo <-
    runBeam (fetchGrupo grupoId)
      `orElseMay` throwJsonError err404 "Grupo no encontrado"

  case shallowGrupo.congeladoAt of
    -- Las deudas ya están decididas y guardadas como transacciones, así que
    -- este camino no mira los pagos: no hay nada que recalcular con ellos.
    Just congeladoAt -> do
      guardadas <- runBeam $ fetchTransacciones grupoId
      pure $
        GrupoCongelado
          ResumenCongelado
            { transaccionesParaSaldar = transaccionesPendientes guardadas
            , transaccionesHechas = transaccionesHechasDesde congeladoAt guardadas
            }
    Nothing -> do
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

      -- Los netos que se muestran son solo de los gastos: quién puso cuánto.
      -- Las transferencias no los mueven, para que sigan diciendo lo mismo
      -- mientras se salda.
      let netos = calcularNetosTotales grupo
      let tabla = tablaDeTasas shallowGrupo.monedaPorDefecto shallowGrupo.tasasDeCambio

      pure $
        GrupoAbierto
          ResumenAbierto
            { netos = netos
            , consolidado = consolidarNetos tabla (netosConSaldo netos)
            , cantidadPagos = length grupo.pagos
            , cantidadPagosInvalidos = length $ filter (not . (.isValid)) grupo.pagos
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
  guardadas <- runBeam $ fetchTransacciones grupoId
  tasasDeCambio <- runBeam $ fetchTasasDeCambio grupoId

  let netos = netosPendientes grupo (transaccionesHechas guardadas)
  let consolidado =
        consolidarNetos
          (tablaDeTasas shallowGrupo.monedaPorDefecto tasasDeCambio)
          (netosConSaldo netos)

  -- Congelar deja una sola tanda de transacciones en la moneda por defecto, así
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

  -- Las transacciones congeladas están en la moneda por defecto de cuando se
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
  -- dejaría las transacciones guardadas hablando de otro tipo de cambio.
  when (estaCongelado shallowGrupo) $
    throwJsonError err423 "El grupo está congelado"

  -- La tabla usa una tasa por moneda y descarta la que no le sirve a 'moneda':
  -- la ajena, la de una moneda consigo misma, la que tiene un lado en cero y la
  -- repetida. Si sobró alguna es que algo de eso pasó; cuál no lo decimos
  -- porque el front no llega a ver el cuerpo del 400.
  unless (cantidadDeTasas (tablaDeTasas moneda tasas) == length tasas) $
    throwJsonError err400 "Alguna de las tasas de cambio no es válida"

  runBeam $ guardarTasasDeCambio grupoId moneda tasas
