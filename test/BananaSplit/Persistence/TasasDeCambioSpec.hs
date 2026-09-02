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
import BananaSplit.TestUtils (tasaEntre)

spec :: SpecWith RunDb
spec = do
  let
    usdArs = tasaEntre USD 1 ARS 1350
    eurArs = tasaEntre EUR 1 ARS 1500
    usdEur = tasaEntre USD 1 EUR 1

    sinId tasa = (tasa.unaMoneda, tasa.otraMoneda, tasa.unMonto, tasa.otroMonto)

    guardar = guardarTasasDeCambio
    comoSeGuarda = sinId . normalizarTasa

  describe "normalizarTasa" $ do
    it "escribe el par en el orden del código, venga como venga" $ \_ -> do
      let arsUsd = tasaEntre ARS 1350 USD 1
      normalizarTasa usdArs `shouldBe` arsUsd
      normalizarTasa arsUsd `shouldBe` arsUsd

    it "manda cada par a una sola forma, se escriba como se escriba" $ \_ ->
      forM_ [(una, otra) | una <- todasLasMonedas, otra <- todasLasMonedas, una /= otra] $ \(una, otra) ->
        normalizarTasa (tasaEntre una 2 otra 3) `shouldBe` normalizarTasa (tasaEntre otra 3 una 2)

    it "no le cambia el sentido a la tasa" $ \_ -> do
      let factorDe base desde tasas =
            factorEntre (tablaDeTasas base tasas) desde
              & fmap unFactor
      factorDe ARS USD [normalizarTasa usdArs] `shouldBe` factorDe ARS USD [usdArs]
      factorDe USD ARS [normalizarTasa usdArs] `shouldBe` factorDe USD ARS [usdArs]

  describe "guardarTasasDeCambio" $ do
    it "guarda las tasas y les asigna un id" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Viaje" "alguien"

      guardadas <- runDb $ guardar grupo.id ARS [usdArs, eurArs]
      (guardadas & fmap sinId) `shouldMatchList` ([usdArs, eurArs] & fmap comoSeGuarda)
      (guardadas & fmap (.id)) `shouldSatisfy` all (/= nullUlid)

      leidas <- runDb $ fetchTasasDeCambio grupo.id
      leidas `shouldMatchList` guardadas

    it "guarda el par siempre en el mismo orden, se escriba como se escriba" $ \(RunDb runDb) -> do
      unGrupo <- runDb $ createGrupo "Viaje" "alguien"
      otroGrupo <- runDb $ createGrupo "Otro viaje" "otre"
      let arsUsd = tasaEntre ARS 1350 USD 1

      unas <- runDb $ guardar unGrupo.id ARS [usdArs]
      otras <- runDb $ guardar otroGrupo.id ARS [arsUsd]

      (unas & fmap sinId) `shouldBe` (otras & fmap sinId)

    it "la base rechaza el par guardado al revés" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Viaje" "alguien"

      -- Saltea 'normalizarTasa' a propósito.
      let alReves =
            Schema.TasaDeCambio
              { Schema.id = nullUlid
              , Schema.grupo = Schema.GrupoId grupo.id
              , Schema.una_moneda = USD
              , Schema.otra_moneda = ARS
              , Schema.un_monto_en_unidades_minimas = 100
              , Schema.otro_monto_en_unidades_minimas = 135000
              }

      runDb (runInsert $ insert db.tasas_de_cambio $ insertValues [alReves])
        `shouldThrow` anyException

    it "la base rechaza un monto que no es positivo" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Viaje" "alguien"

      let enCero =
            Schema.TasaDeCambio
              { Schema.id = nullUlid
              , Schema.grupo = Schema.GrupoId grupo.id
              , Schema.una_moneda = ARS
              , Schema.otra_moneda = USD
              , Schema.un_monto_en_unidades_minimas = 0
              , Schema.otro_monto_en_unidades_minimas = 100
              }

      runDb (runInsert $ insert db.tasas_de_cambio $ insertValues [enCero])
        `shouldThrow` anyException

    it "borra las tasas de la moneda que no van en la lista" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Viaje" "alguien"

      _ <- runDb $ guardar grupo.id ARS [usdArs, eurArs]
      _ <- runDb $ guardar grupo.id ARS [eurArs]

      leidas <- runDb $ fetchTasasDeCambio grupo.id
      (leidas & fmap sinId) `shouldBe` [comoSeGuarda eurArs]

    it "borra todas las tasas de la moneda si la lista viene vacía" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Viaje" "alguien"

      _ <- runDb $ guardar grupo.id ARS [usdArs, eurArs]
      _ <- runDb $ guardar grupo.id ARS []

      runDb (fetchTasasDeCambio grupo.id) `shouldReturn` []

    it "no toca las tasas que no involucran a la moneda" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Viaje" "alguien"

      _ <- runDb $ guardar grupo.id ARS [usdArs, eurArs]
      _ <- runDb $ guardar grupo.id EUR [eurArs, usdEur]

      leidas <- runDb $ fetchTasasDeCambio grupo.id
      (leidas & fmap sinId)
        `shouldMatchList` [comoSeGuarda usdArs, comoSeGuarda eurArs, comoSeGuarda usdEur]

    it "pisa la tasa del par" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Viaje" "alguien"

      _ <- runDb $ guardar grupo.id ARS [usdArs]
      [corregida] <- runDb $ guardar grupo.id ARS [usdArs{otroMonto = 1400}]

      sinId corregida `shouldBe` comoSeGuarda usdArs{otroMonto = 1400}

      leidas <- runDb $ fetchTasasDeCambio grupo.id
      leidas `shouldBe` [corregida]

    it "pisa la tasa aunque el par venga escrito al revés" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Viaje" "alguien"
      let arsUsd = tasaEntre ARS 1400 USD 1

      _ <- runDb $ guardar grupo.id ARS [usdArs]
      leidas <- runDb $ guardar grupo.id ARS [arsUsd]

      (leidas & fmap sinId) `shouldBe` [comoSeGuarda arsUsd]

    it "no toca las tasas de otro grupo" $ \(RunDb runDb) -> do
      unGrupo <- runDb $ createGrupo "Viaje" "alguien"
      otroGrupo <- runDb $ createGrupo "Otro viaje" "otre"

      _ <- runDb $ guardar otroGrupo.id ARS [usdArs, eurArs]
      _ <- runDb $ guardar unGrupo.id ARS []

      otras <- runDb $ fetchTasasDeCambio otroGrupo.id
      (otras & fmap sinId) `shouldMatchList` ([usdArs, eurArs] & fmap comoSeGuarda)

  describe "fetchTasasDeCambio" $ do
    it "devuelve vacío para un grupo que nunca cargó tasas" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Viaje" "alguien"
      runDb (fetchTasasDeCambio grupo.id) `shouldReturn` []
