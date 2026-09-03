module BananaSplit.Persistence.TransaccionesSpec (
  spec,
) where

import Database.Beam.Postgres (Pg)
import Protolude
import Test.Hspec

import BananaSplit.Core
import BananaSplit.Deudas
import BananaSplit.Moneda
import BananaSplit.Participante
import BananaSplit.Persistence
import BananaSplit.Persistence.SpecHook
import BananaSplit.ULID (ULID)

spec :: SpecWith RunDb
spec = do
  let
    transaccionEntre desde hacia monto =
      Transaccion{id = Nothing, from = desde, to = hacia, monto = monto}

    sinId transaccion = (transaccion.from, transaccion.to, transaccion.monto)

    montosDe = fmap (fmap sinId)

    montosDeHechas = fmap (fmap (sinId . (.transaccion)))

  describe "freezeGrupo" $ do
    it "deja las transacciones pendientes en la moneda que se le pasa" $ \(RunDb runDb) -> do
      (grupo, una, otra) <- grupoDeDos (RunDb runDb)

      runDb $ freezeGrupo grupo.id ARS [transaccionEntre otra una 100]

      guardadas <- runDb $ fetchTransacciones grupo.id
      montosDe (transaccionesPendientes guardadas)
        `shouldBe` [transaccionEntre otra una 100 & sinId] `enMoneda` ARS
      transaccionesHechas guardadas `shouldBe` mempty

    it "marca el grupo como congelado" $ \(RunDb runDb) -> do
      (grupo, una, otra) <- grupoDeDos (RunDb runDb)

      runDb $ freezeGrupo grupo.id ARS [transaccionEntre otra una 100]

      congelado <- runDb $ fetchGrupo grupo.id
      (congelado >>= (.congeladoAt)) `shouldSatisfy` isJust

    it "pisa las pendientes del congelamiento anterior" $ \(RunDb runDb) -> do
      (grupo, una, otra) <- grupoDeDos (RunDb runDb)

      runDb $ freezeGrupo grupo.id ARS [transaccionEntre otra una 100]
      runDb $ freezeGrupo grupo.id ARS [transaccionEntre otra una 250]

      guardadas <- runDb $ fetchTransacciones grupo.id
      montosDe (transaccionesPendientes guardadas)
        `shouldBe` [transaccionEntre otra una 250 & sinId] `enMoneda` ARS

  describe "marcarTransaccionSaldada" $ do
    it "pasa la transacción de pendiente a hecha" $ \(RunDb runDb) -> do
      (grupo, una, otra) <- grupoDeDos (RunDb runDb)
      runDb $ freezeGrupo grupo.id ARS [transaccionEntre otra una 100]
      pendiente <- unaPendiente runDb grupo.id

      runDb $ marcarTransaccionSaldada grupo.id pendiente

      guardadas <- runDb $ fetchTransacciones grupo.id
      transaccionesPendientes guardadas `shouldBe` mempty
      montosDeHechas (transaccionesHechas guardadas)
        `shouldBe` [transaccionEntre otra una 100 & sinId] `enMoneda` ARS

    it "no toca las transacciones de otro grupo" $ \(RunDb runDb) -> do
      (grupo, una, otra) <- grupoDeDos (RunDb runDb)
      (ajeno, _, _) <- grupoDeDos (RunDb runDb)
      runDb $ freezeGrupo grupo.id ARS [transaccionEntre otra una 100]
      pendiente <- unaPendiente runDb grupo.id

      runDb $ marcarTransaccionSaldada ajeno.id pendiente

      guardadas <- runDb $ fetchTransacciones grupo.id
      transaccionesHechas guardadas `shouldBe` mempty

  describe "desmarcarTransaccionSaldada" $ do
    it "la vuelve a dejar pendiente" $ \(RunDb runDb) -> do
      (grupo, una, otra) <- grupoDeDos (RunDb runDb)
      runDb $ freezeGrupo grupo.id ARS [transaccionEntre otra una 100]
      pendiente <- unaPendiente runDb grupo.id
      runDb $ marcarTransaccionSaldada grupo.id pendiente

      runDb $ desmarcarTransaccionSaldada grupo.id pendiente

      guardadas <- runDb $ fetchTransacciones grupo.id
      transaccionesHechas guardadas `shouldBe` mempty
      montosDe (transaccionesPendientes guardadas)
        `shouldBe` [transaccionEntre otra una 100 & sinId] `enMoneda` ARS

    it "no toca las transacciones de otro grupo" $ \(RunDb runDb) -> do
      (grupo, una, otra) <- grupoDeDos (RunDb runDb)
      (ajeno, _, _) <- grupoDeDos (RunDb runDb)
      runDb $ freezeGrupo grupo.id ARS [transaccionEntre otra una 100]
      pendiente <- unaPendiente runDb grupo.id
      runDb $ marcarTransaccionSaldada grupo.id pendiente

      runDb $ desmarcarTransaccionSaldada ajeno.id pendiente

      guardadas <- runDb $ fetchTransacciones grupo.id
      transaccionesPendientes guardadas `shouldBe` mempty

  describe "unfreezeGrupo" $ do
    it "borra las pendientes pero deja las hechas" $ \(RunDb runDb) -> do
      (grupo, una, otra) <- grupoDeDos (RunDb runDb)
      runDb $ freezeGrupo grupo.id ARS [transaccionEntre otra una 100]
      pendiente <- unaPendiente runDb grupo.id
      runDb $ marcarTransaccionSaldada grupo.id pendiente
      -- Se congela de nuevo para que quede una pendiente al lado de la hecha.
      runDb $ freezeGrupo grupo.id ARS [transaccionEntre una otra 40]

      runDb $ unfreezeGrupo grupo.id

      guardadas <- runDb $ fetchTransacciones grupo.id
      transaccionesPendientes guardadas `shouldBe` mempty
      montosDeHechas (transaccionesHechas guardadas)
        `shouldBe` [transaccionEntre otra una 100 & sinId] `enMoneda` ARS

    it "deja el grupo descongelado y sin fecha de congelamiento" $ \(RunDb runDb) -> do
      (grupo, una, otra) <- grupoDeDos (RunDb runDb)
      runDb $ freezeGrupo grupo.id ARS [transaccionEntre otra una 100]

      runDb $ unfreezeGrupo grupo.id

      descongelado <- runDb $ fetchGrupo grupo.id
      (descongelado >>= (.congeladoAt)) `shouldBe` Nothing

  describe "crearTransaccionSaldada" $ do
    it "nace hecha, sin pasar por un congelamiento" $ \(RunDb runDb) -> do
      (grupo, una, otra) <- grupoDeDos (RunDb runDb)

      creada <- runDb $ crearTransaccionSaldada grupo.id USD (transaccionEntre otra una 20)
      creada.id `shouldSatisfy` isJust

      guardadas <- runDb $ fetchTransacciones grupo.id
      transaccionesPendientes guardadas `shouldBe` mempty
      montosDeHechas (transaccionesHechas guardadas)
        `shouldBe` [transaccionEntre otra una 20 & sinId] `enMoneda` USD

-- | Un grupo con dos participantes, que es todo lo que hace falta para mirar el
-- ciclo congelar/marcar/descongelar.
grupoDeDos :: RunDb -> IO (Grupo, ParticipanteId, ParticipanteId)
grupoDeDos (RunDb runDb) = do
  grupo <- runDb $ createGrupo "Viaje" "una"
  otra <- runDb (addParticipante grupo.id "otra") >>= either panic pure
  case grupo.participantes of
    [una] -> pure (grupo, ParticipanteId una.id, ParticipanteId otra.id)
    _ -> panic "se esperaba exactamente un participante"

-- | El id de la única transacción pendiente que dejó el congelamiento.
unaPendiente :: (forall a. Pg a -> IO a) -> ULID -> IO ULID
unaPendiente runDb grupoId = do
  guardadas <- runDb $ fetchTransacciones grupoId
  case guardadas & filter (isNothing . (.saldadaAt)) of
    (primera : _) -> case primera.transaccion.id of
      Just transaccionId -> pure transaccionId
      Nothing -> panic "una transacción guardada siempre tiene id"
    [] -> panic "se esperaba al menos una transacción pendiente"
