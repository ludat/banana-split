module BananaSplit.Persistence.TransferenciasSpec (
  spec,
) where

import Data.Time (fromGregorian)
import Database.Beam.Postgres (Pg)
import Protolude
import Test.Hspec

import BananaSplit.Core
import BananaSplit.Deudas
import BananaSplit.Moneda
import BananaSplit.Monto (Monto)
import BananaSplit.Participante
import BananaSplit.Persistence
import BananaSplit.Persistence.SpecHook
import BananaSplit.ULID (ULID)

spec :: SpecWith RunDb
spec = do
  let
    -- Lo que hay para comparar de una fila: el id y la fecha los pone la db,
    -- así que quedan afuera.
    entre desde hacia monto moneda = (desde, hacia, monto, moneda)

    resumir t = entre t.from t.to t.monto t.moneda

    pendientes = fmap resumir . filter (not . transferenciaEstaHecha)

    hechas = fmap resumir . filter transferenciaEstaHecha

  describe "congelarGrupo" $ do
    it "deja pendientes las transferencias que saldan las deudas de los gastos" $ \(RunDb runDb) -> do
      (grupo, una, otra) <- grupoDeDos (RunDb runDb)
      _ <- runDb $ savePago grupo.id $ gastoEntre ARS 100 una otra

      congelado <- runDb (congelarGrupo grupo.id) >>= either (panic . show) pure
      congelado.congeladoAt `shouldSatisfy` isJust

      guardadas <- runDb $ fetchTransferencias grupo.id
      pendientes guardadas `shouldBe` [entre otra una 100 ARS]
      hechas guardadas `shouldBe` []

    it "pisa las pendientes del congelamiento anterior" $ \(RunDb runDb) -> do
      (grupo, una, otra) <- grupoDeDos (RunDb runDb)
      congelarConDeuda runDb grupo una otra 100

      _ <- runDb $ savePago grupo.id $ gastoEntre ARS 150 una otra
      _ <- runDb (congelarGrupo grupo.id) >>= either (panic . show) pure

      guardadas <- runDb $ fetchTransferencias grupo.id
      pendientes guardadas `shouldBe` [entre otra una 250 ARS]

    it "no congela si hay un gasto inválido" $ \(RunDb runDb) -> do
      (grupo, una, otra) <- grupoDeDos (RunDb runDb)
      _ <- runDb $ savePago grupo.id $ gastoEntre ARS 100 una otra
      -- Un gasto sin deudores no entra en los netos: congelar así fijaría una
      -- deuda a la que le falta plata.
      _ <- runDb $ savePago grupo.id $ (gastoEntre ARS 70 una otra){deudores = distribucionVacia}

      runDb (congelarGrupo grupo.id) `shouldReturn` Left (HayGastosInvalidos 1)

      sigueAbierto <- runDb $ fetchGrupo grupo.id
      (sigueAbierto >>= (.congeladoAt)) `shouldBe` Nothing
      guardadas <- runDb $ fetchTransferencias grupo.id
      pendientes guardadas `shouldBe` []

    it "no congela si falta la tasa de cambio de una moneda con deuda" $ \(RunDb runDb) -> do
      (grupo, una, otra) <- grupoDeDos (RunDb runDb)
      _ <- runDb $ savePago grupo.id $ gastoEntre USD 50 una otra

      runDb (congelarGrupo grupo.id) `shouldReturn` Left (FaltanTasasDeCambio [USD])

      sigueAbierto <- runDb $ fetchGrupo grupo.id
      (sigueAbierto >>= (.congeladoAt)) `shouldBe` Nothing

  describe "marcarTransferenciaSaldada" $ do
    it "pasa la transferencia de pendiente a hecha" $ \(RunDb runDb) -> do
      (grupo, una, otra) <- grupoDeDos (RunDb runDb)
      congelarConDeuda runDb grupo una otra 100
      pendiente <- unaPendiente runDb grupo.id

      runDb $ marcarTransferenciaSaldada grupo.id pendiente

      guardadas <- runDb $ fetchTransferencias grupo.id
      pendientes guardadas `shouldBe` []
      hechas guardadas `shouldBe` [entre otra una 100 ARS]

    it "no toca las transferencias de otro grupo" $ \(RunDb runDb) -> do
      (grupo, una, otra) <- grupoDeDos (RunDb runDb)
      (ajeno, _, _) <- grupoDeDos (RunDb runDb)
      congelarConDeuda runDb grupo una otra 100
      pendiente <- unaPendiente runDb grupo.id

      runDb $ marcarTransferenciaSaldada ajeno.id pendiente

      guardadas <- runDb $ fetchTransferencias grupo.id
      hechas guardadas `shouldBe` []

  describe "desmarcarTransferenciaSaldada" $ do
    it "la vuelve a dejar pendiente" $ \(RunDb runDb) -> do
      (grupo, una, otra) <- grupoDeDos (RunDb runDb)
      congelarConDeuda runDb grupo una otra 100
      pendiente <- unaPendiente runDb grupo.id
      runDb $ marcarTransferenciaSaldada grupo.id pendiente

      runDb $ desmarcarTransferenciaSaldada grupo.id pendiente

      guardadas <- runDb $ fetchTransferencias grupo.id
      hechas guardadas `shouldBe` []
      pendientes guardadas `shouldBe` [entre otra una 100 ARS]

    it "no toca las transferencias de otro grupo" $ \(RunDb runDb) -> do
      (grupo, una, otra) <- grupoDeDos (RunDb runDb)
      (ajeno, _, _) <- grupoDeDos (RunDb runDb)
      congelarConDeuda runDb grupo una otra 100
      pendiente <- unaPendiente runDb grupo.id
      runDb $ marcarTransferenciaSaldada grupo.id pendiente

      runDb $ desmarcarTransferenciaSaldada ajeno.id pendiente

      guardadas <- runDb $ fetchTransferencias grupo.id
      pendientes guardadas `shouldBe` []

  describe "unfreezeGrupo" $ do
    it "borra las pendientes pero deja las hechas" $ \(RunDb runDb) -> do
      (grupo, una, otra) <- grupoDeDos (RunDb runDb)
      congelarConDeuda runDb grupo una otra 100
      pendiente <- unaPendiente runDb grupo.id
      runDb $ marcarTransferenciaSaldada grupo.id pendiente
      -- Otro gasto y otro congelamiento para que quede una pendiente al lado de
      -- la hecha: la deuda vieja ya la saldó la transferencia de arriba.
      _ <- runDb $ savePago grupo.id $ gastoEntre ARS 40 una otra
      _ <- runDb (congelarGrupo grupo.id) >>= either (panic . show) pure

      runDb $ unfreezeGrupo grupo.id

      guardadas <- runDb $ fetchTransferencias grupo.id
      pendientes guardadas `shouldBe` []
      hechas guardadas `shouldBe` [entre otra una 100 ARS]

    it "deja el grupo descongelado y sin fecha de congelamiento" $ \(RunDb runDb) -> do
      (grupo, una, otra) <- grupoDeDos (RunDb runDb)
      congelarConDeuda runDb grupo una otra 100

      runDb $ unfreezeGrupo grupo.id

      descongelado <- runDb $ fetchGrupo grupo.id
      (descongelado >>= (.congeladoAt)) `shouldBe` Nothing

  describe "crearTransferenciaSaldada" $ do
    it "nace hecha, sin pasar por un congelamiento" $ \(RunDb runDb) -> do
      (grupo, una, otra) <- grupoDeDos (RunDb runDb)

      creada <- runDb $ crearTransferenciaSaldada grupo.id otra una 20 USD
      creada.saldadaAt `shouldSatisfy` isJust

      guardadas <- runDb $ fetchTransferencias grupo.id
      pendientes guardadas `shouldBe` []
      hechas guardadas `shouldBe` [entre otra una 20 USD]

-- | Un gasto donde uno pone todo y el otro consume todo.
gastoEntre :: Moneda -> Monto -> ParticipanteId -> ParticipanteId -> Pago
gastoEntre moneda monto pagador deudor =
  Pago
    { pagoId = nullUlid
    , monto = monto
    , moneda = moneda
    , nombre = "Gasto"
    , fecha = fromGregorian 2025 1 1
    , pagadores = distribucionDe [MontoFijo monto pagador]
    , deudores = distribucionDe [MontoFijo monto deudor]
    }

distribucionDe :: [Parte] -> Distribucion
distribucionDe partes =
  Distribucion nullUlid $ TipoDistribucionPartes $ DistribucionPartes nullUlid partes

distribucionVacia :: Distribucion
distribucionVacia = distribucionDe []

-- | Deja el grupo congelado con una sola transferencia pendiente: el gasto lo
-- pone @pagador@ y lo consume @deudor@, así que queda debiendo el monto entero.
congelarConDeuda ::
  (forall a. Pg a -> IO a)
  -> Grupo
  -> ParticipanteId
  -> ParticipanteId
  -> Monto
  -> IO ()
congelarConDeuda runDb grupo pagador deudor monto = do
  _ <- runDb $ savePago grupo.id $ gastoEntre ARS monto pagador deudor
  _ <- runDb (congelarGrupo grupo.id) >>= either (panic . show) pure
  pure ()

-- | Un grupo con dos participantes, que es todo lo que hace falta para mirar el
-- ciclo congelar/marcar/descongelar.
grupoDeDos :: RunDb -> IO (Grupo, ParticipanteId, ParticipanteId)
grupoDeDos (RunDb runDb) = do
  grupo <- runDb $ createGrupo "Viaje" "una"
  otra <- runDb (addParticipante grupo.id "otra") >>= either panic pure
  case grupo.participantes of
    [una] -> pure (grupo, ParticipanteId una.id, ParticipanteId otra.id)
    _ -> panic "se esperaba exactamente un participante"

-- | El id de la única transferencia pendiente que dejó el congelamiento.
unaPendiente :: (forall a. Pg a -> IO a) -> ULID -> IO ULID
unaPendiente runDb grupoId = do
  guardadas <- runDb $ fetchTransferencias grupoId
  case guardadas & filter (not . transferenciaEstaHecha) of
    (primera : _) -> pure primera.id
    [] -> panic "se esperaba al menos una transferencia pendiente"
