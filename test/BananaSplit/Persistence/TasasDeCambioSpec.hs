module BananaSplit.Persistence.TasasDeCambioSpec (
  spec,
) where

import Database.Beam (insert, insertValues, runInsert)
import Protolude
import Test.Hspec

import BananaSplit.Core
import BananaSplit.Moneda
import BananaSplit.Persistence
import BananaSplit.Persistence.Schema qualified as Schema
import BananaSplit.Persistence.SpecHook
import BananaSplit.TasaDeCambio

spec :: SpecWith RunDb
spec = do
  let
    usdArs = TasaDeCambio{id = nullUlid, monedaFrom = USD, monedaTo = ARS, montoFrom = 1, montoTo = 1350}
    eurArs = TasaDeCambio{id = nullUlid, monedaFrom = EUR, monedaTo = ARS, montoFrom = 1, montoTo = 1500}
    usdEur = TasaDeCambio{id = nullUlid, monedaFrom = USD, monedaTo = EUR, montoFrom = 1, montoTo = 1}

    sinId tasa = (tasa.monedaFrom, tasa.monedaTo, tasa.montoFrom, tasa.montoTo)
    comoSeGuarda = sinId . normalizarTasa

  describe "guardarTasasDeCambio" $ do
    it "guarda las tasas y les asigna un id" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Viaje" "alguien"

      guardadas <- runDb $ guardarTasasDeCambio grupo.id ARS [usdArs, eurArs]
      (guardadas & fmap sinId) `shouldMatchList` ([usdArs, eurArs] & fmap comoSeGuarda)
      (guardadas & fmap (.id)) `shouldSatisfy` all (/= nullUlid)

      leidas <- runDb $ fetchTasasDeCambio grupo.id
      leidas `shouldMatchList` guardadas

    it "guarda el par siempre en el mismo orden, se escriba como se escriba" $ \(RunDb runDb) -> do
      unGrupo <- runDb $ createGrupo "Viaje" "alguien"
      otroGrupo <- runDb $ createGrupo "Otro viaje" "otre"
      let arsUsd = TasaDeCambio{id = nullUlid, monedaFrom = ARS, monedaTo = USD, montoFrom = 1350, montoTo = 1}

      unas <- runDb $ guardarTasasDeCambio unGrupo.id ARS [usdArs]
      otras <- runDb $ guardarTasasDeCambio otroGrupo.id ARS [arsUsd]

      (unas & fmap sinId) `shouldBe` (otras & fmap sinId)

    it "la base rechaza el par guardado al revés" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Viaje" "alguien"

      -- Saltea 'normalizarTasa' a propósito.
      let alReves =
            Schema.TasaDeCambio
              { Schema.id = nullUlid
              , Schema.grupo = Schema.GrupoId grupo.id
              , Schema.moneda_from = USD
              , Schema.moneda_to = ARS
              , Schema.monto_from = Schema.Monto 2 100
              , Schema.monto_to = Schema.Monto 2 135000
              }

      runDb (runInsert $ insert db.tasas_de_cambio $ insertValues [alReves])
        `shouldThrow` anyException

    it "borra las tasas de la moneda que no van en la lista" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Viaje" "alguien"

      _ <- runDb $ guardarTasasDeCambio grupo.id ARS [usdArs, eurArs]
      _ <- runDb $ guardarTasasDeCambio grupo.id ARS [eurArs]

      leidas <- runDb $ fetchTasasDeCambio grupo.id
      (leidas & fmap sinId) `shouldBe` [comoSeGuarda eurArs]

    it "borra todas las tasas de la moneda si la lista viene vacía" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Viaje" "alguien"

      _ <- runDb $ guardarTasasDeCambio grupo.id ARS [usdArs, eurArs]
      _ <- runDb $ guardarTasasDeCambio grupo.id ARS []

      runDb (fetchTasasDeCambio grupo.id) `shouldReturn` []

    it "no toca las tasas que no involucran a la moneda" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Viaje" "alguien"

      _ <- runDb $ guardarTasasDeCambio grupo.id ARS [usdArs, eurArs]
      _ <- runDb $ guardarTasasDeCambio grupo.id EUR [eurArs, usdEur]

      leidas <- runDb $ fetchTasasDeCambio grupo.id
      (leidas & fmap sinId)
        `shouldMatchList` ([usdArs, eurArs, usdEur] & fmap comoSeGuarda)

    it "pisa la tasa del par" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Viaje" "alguien"

      _ <- runDb $ guardarTasasDeCambio grupo.id ARS [usdArs]
      [corregida] <- runDb $ guardarTasasDeCambio grupo.id ARS [usdArs{montoTo = 1400}]

      sinId corregida `shouldBe` comoSeGuarda usdArs{montoTo = 1400}

      leidas <- runDb $ fetchTasasDeCambio grupo.id
      leidas `shouldBe` [corregida]

    it "pisa la tasa aunque el par venga escrito al revés" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Viaje" "alguien"
      let arsUsd = TasaDeCambio{id = nullUlid, monedaFrom = ARS, monedaTo = USD, montoFrom = 1400, montoTo = 1}

      _ <- runDb $ guardarTasasDeCambio grupo.id ARS [usdArs]
      leidas <- runDb $ guardarTasasDeCambio grupo.id ARS [arsUsd]

      (leidas & fmap sinId) `shouldBe` [comoSeGuarda arsUsd]

    it "no toca las tasas de otro grupo" $ \(RunDb runDb) -> do
      unGrupo <- runDb $ createGrupo "Viaje" "alguien"
      otroGrupo <- runDb $ createGrupo "Otro viaje" "otre"

      _ <- runDb $ guardarTasasDeCambio otroGrupo.id ARS [usdArs, eurArs]
      _ <- runDb $ guardarTasasDeCambio unGrupo.id ARS []

      otras <- runDb $ fetchTasasDeCambio otroGrupo.id
      (otras & fmap sinId) `shouldMatchList` ([usdArs, eurArs] & fmap comoSeGuarda)

  describe "fetchTasasDeCambio" $ do
    it "devuelve vacío para un grupo que nunca cargó tasas" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Viaje" "alguien"
      runDb (fetchTasasDeCambio grupo.id) `shouldReturn` []
