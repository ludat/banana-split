module BananaSplit.Persistence.TransferenciasSpec (
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
    transferenciaEntre desde hacia monto =
      Transferencia{id = Nothing, from = desde, to = hacia, monto = monto}

    sinId transferencia = (transferencia.from, transferencia.to, transferencia.monto)

    montosDe = fmap (fmap sinId)

    montosDeHechas = fmap (fmap (sinId . (.transferencia)))

  describe "freezeGrupo" $ do
    it "deja las transferencias pendientes en la moneda que se le pasa" $ \(RunDb runDb) -> do
      (grupo, una, otra) <- grupoDeDos (RunDb runDb)

      runDb $ freezeGrupo grupo.id ARS [transferenciaEntre otra una 100]

      guardadas <- runDb $ fetchTransferencias grupo.id
      montosDe (transferenciasPendientes guardadas)
        `shouldBe` [transferenciaEntre otra una 100 & sinId] `enMoneda` ARS
      transferenciasHechas guardadas `shouldBe` mempty

    it "marca el grupo como congelado" $ \(RunDb runDb) -> do
      (grupo, una, otra) <- grupoDeDos (RunDb runDb)

      runDb $ freezeGrupo grupo.id ARS [transferenciaEntre otra una 100]

      congelado <- runDb $ fetchGrupo grupo.id
      (congelado >>= (.congeladoAt)) `shouldSatisfy` isJust

    it "pisa las pendientes del congelamiento anterior" $ \(RunDb runDb) -> do
      (grupo, una, otra) <- grupoDeDos (RunDb runDb)

      runDb $ freezeGrupo grupo.id ARS [transferenciaEntre otra una 100]
      runDb $ freezeGrupo grupo.id ARS [transferenciaEntre otra una 250]

      guardadas <- runDb $ fetchTransferencias grupo.id
      montosDe (transferenciasPendientes guardadas)
        `shouldBe` [transferenciaEntre otra una 250 & sinId] `enMoneda` ARS

  describe "marcarTransferenciaSaldada" $ do
    it "pasa la transferencia de pendiente a hecha" $ \(RunDb runDb) -> do
      (grupo, una, otra) <- grupoDeDos (RunDb runDb)
      runDb $ freezeGrupo grupo.id ARS [transferenciaEntre otra una 100]
      pendiente <- unaPendiente runDb grupo.id

      runDb $ marcarTransferenciaSaldada grupo.id pendiente

      guardadas <- runDb $ fetchTransferencias grupo.id
      transferenciasPendientes guardadas `shouldBe` mempty
      montosDeHechas (transferenciasHechas guardadas)
        `shouldBe` [transferenciaEntre otra una 100 & sinId] `enMoneda` ARS

    it "no toca las transferencias de otro grupo" $ \(RunDb runDb) -> do
      (grupo, una, otra) <- grupoDeDos (RunDb runDb)
      (ajeno, _, _) <- grupoDeDos (RunDb runDb)
      runDb $ freezeGrupo grupo.id ARS [transferenciaEntre otra una 100]
      pendiente <- unaPendiente runDb grupo.id

      runDb $ marcarTransferenciaSaldada ajeno.id pendiente

      guardadas <- runDb $ fetchTransferencias grupo.id
      transferenciasHechas guardadas `shouldBe` mempty

  describe "desmarcarTransferenciaSaldada" $ do
    it "la vuelve a dejar pendiente" $ \(RunDb runDb) -> do
      (grupo, una, otra) <- grupoDeDos (RunDb runDb)
      runDb $ freezeGrupo grupo.id ARS [transferenciaEntre otra una 100]
      pendiente <- unaPendiente runDb grupo.id
      runDb $ marcarTransferenciaSaldada grupo.id pendiente

      runDb $ desmarcarTransferenciaSaldada grupo.id pendiente

      guardadas <- runDb $ fetchTransferencias grupo.id
      transferenciasHechas guardadas `shouldBe` mempty
      montosDe (transferenciasPendientes guardadas)
        `shouldBe` [transferenciaEntre otra una 100 & sinId] `enMoneda` ARS

    it "no toca las transferencias de otro grupo" $ \(RunDb runDb) -> do
      (grupo, una, otra) <- grupoDeDos (RunDb runDb)
      (ajeno, _, _) <- grupoDeDos (RunDb runDb)
      runDb $ freezeGrupo grupo.id ARS [transferenciaEntre otra una 100]
      pendiente <- unaPendiente runDb grupo.id
      runDb $ marcarTransferenciaSaldada grupo.id pendiente

      runDb $ desmarcarTransferenciaSaldada ajeno.id pendiente

      guardadas <- runDb $ fetchTransferencias grupo.id
      transferenciasPendientes guardadas `shouldBe` mempty

  describe "unfreezeGrupo" $ do
    it "borra las pendientes pero deja las hechas" $ \(RunDb runDb) -> do
      (grupo, una, otra) <- grupoDeDos (RunDb runDb)
      runDb $ freezeGrupo grupo.id ARS [transferenciaEntre otra una 100]
      pendiente <- unaPendiente runDb grupo.id
      runDb $ marcarTransferenciaSaldada grupo.id pendiente
      -- Se congela de nuevo para que quede una pendiente al lado de la hecha.
      runDb $ freezeGrupo grupo.id ARS [transferenciaEntre una otra 40]

      runDb $ unfreezeGrupo grupo.id

      guardadas <- runDb $ fetchTransferencias grupo.id
      transferenciasPendientes guardadas `shouldBe` mempty
      montosDeHechas (transferenciasHechas guardadas)
        `shouldBe` [transferenciaEntre otra una 100 & sinId] `enMoneda` ARS

    it "deja el grupo descongelado y sin fecha de congelamiento" $ \(RunDb runDb) -> do
      (grupo, una, otra) <- grupoDeDos (RunDb runDb)
      runDb $ freezeGrupo grupo.id ARS [transferenciaEntre otra una 100]

      runDb $ unfreezeGrupo grupo.id

      descongelado <- runDb $ fetchGrupo grupo.id
      (descongelado >>= (.congeladoAt)) `shouldBe` Nothing

  describe "crearTransferenciaSaldada" $ do
    it "nace hecha, sin pasar por un congelamiento" $ \(RunDb runDb) -> do
      (grupo, una, otra) <- grupoDeDos (RunDb runDb)

      creada <- runDb $ crearTransferenciaSaldada grupo.id USD (transferenciaEntre otra una 20)
      creada.id `shouldSatisfy` isJust

      guardadas <- runDb $ fetchTransferencias grupo.id
      transferenciasPendientes guardadas `shouldBe` mempty
      montosDeHechas (transferenciasHechas guardadas)
        `shouldBe` [transferenciaEntre otra una 20 & sinId] `enMoneda` USD

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
  case guardadas & filter (isNothing . (.saldadaAt)) of
    (primera : _) -> case primera.transferencia.id of
      Just transferenciaId -> pure transferenciaId
      Nothing -> panic "una transferencia guardada siempre tiene id"
    [] -> panic "se esperaba al menos una transferencia pendiente"
