module BananaSplit.TasaDeCambioSpec (
  spec,
) where

import Protolude
import Test.Hspec

import BananaSplit
import BananaSplit.TestUtils

spec :: Spec
spec = do
  let
    u1 = participante 1
    u2 = participante 2

    usdArs =
      TasaDeCambio
        { id = nullUlid
        , monedaFrom = USD
        , monedaTo = ARS
        , montoFrom = 1
        , montoTo = 1000
        }

  describe "factorEntre" $ do
    it "es 1 entre una moneda y sí misma, aunque no haya ninguna tasa" $
      factorEntre [] ARS ARS `shouldBe` Just 1

    it "usa la tasa cargada tal cual" $
      factorEntre [usdArs] USD ARS `shouldBe` Just 1000

    it "da vuelta la tasa cuando se pide al revés" $
      factorEntre [usdArs] ARS USD `shouldBe` Just (1 / 1000)

    it "no encadena tasas: sin EUR -> USD directo no hay conversión" $ do
      let eurArs = TasaDeCambio{id = nullUlid, monedaFrom = EUR, monedaTo = ARS, montoFrom = 1, montoTo = 2000}
      factorEntre [usdArs, eurArs] EUR USD `shouldBe` Nothing

    it "no usa una tasa con montos en cero" $ do
      let rota = TasaDeCambio{id = nullUlid, monedaFrom = USD, monedaTo = ARS, montoFrom = 0, montoTo = 1000}
      factorEntre [rota] USD ARS `shouldBe` Nothing

    it "sirve una tasa que no arranca en 1" $ do
      let clpEur = TasaDeCambio{id = nullUlid, monedaFrom = CLP, monedaTo = EUR, montoFrom = 1000, montoTo = 1}
      factorEntre [clpEur] CLP EUR `shouldBe` Just (1 / 1000)

  describe "consolidarNetos" $ do
    it "suma las otras monedas convertidas a la moneda destino" $ do
      let porMoneda =
            netos [(u1, 100), (u2, -100)]
              `enMoneda` ARS
              <> netos [(u1, -1), (u2, 1)]
              `enMoneda` USD
          consolidado = consolidarNetos ARS [usdArs] porMoneda

      consolidado.netos `shouldBe` netos [(u1, -900), (u2, 900)]
      consolidado.monedasConvertidas `shouldMatchList` [ARS, USD]
      consolidado.monedasSinTasa `shouldBe` []

    it "deja afuera las monedas sin tasa y las reporta" $ do
      let porMoneda =
            netos [(u1, 100), (u2, -100)]
              `enMoneda` ARS
              <> netos [(u1, -1), (u2, 1)]
              `enMoneda` EUR
          consolidado = consolidarNetos ARS [usdArs] porMoneda

      consolidado.netos `shouldBe` netos [(u1, 100), (u2, -100)]
      consolidado.monedasConvertidas `shouldBe` [ARS]
      consolidado.monedasSinTasa `shouldBe` [EUR]

    it "los netos convertidos siguen sumando cero aunque el redondeo no cierre" $ do
      let tasa = TasaDeCambio{id = nullUlid, monedaFrom = USD, monedaTo = ARS, montoFrom = 3, montoTo = 10}
          u3 = participante 3
          porMoneda = netos [(u1, 1), (u2, 1), (u3, -2)] `enMoneda` USD
          consolidado = consolidarNetos ARS [tasa] porMoneda

      totalNetos consolidado.netos `shouldBe` 0

    it "la misma tasa sirve después de cambiar la moneda por defecto del grupo" $ do
      let porMoneda =
            netos [(u1, 1000), (u2, -1000)]
              `enMoneda` ARS
              <> netos [(u1, -3), (u2, 3)]
              `enMoneda` USD

      (consolidarNetos ARS [usdArs] porMoneda).netos
        `shouldBe` netos [(u1, -2000), (u2, 2000)]
      (consolidarNetos USD [usdArs] porMoneda).netos
        `shouldBe` netos [(u1, -2), (u2, 2)]

      (consolidarNetos USD [usdArs] porMoneda).monedasSinTasa `shouldBe` []

  describe "validarTasas" $ do
    it "acepta un conjunto sano" $
      validarTasas ARS [usdArs] `shouldBe` Right [usdArs]

    it "rechaza una tasa que no involucra a la moneda que se guarda" $ do
      let tasa = TasaDeCambio{id = nullUlid, monedaFrom = USD, monedaTo = EUR, montoFrom = 1, montoTo = 1}
      validarTasas ARS [tasa] `shouldSatisfy` isLeft

    it "rechaza una tasa entre la misma moneda" $ do
      let tasa = TasaDeCambio{id = nullUlid, monedaFrom = ARS, monedaTo = ARS, montoFrom = 1, montoTo = 1}
      validarTasas ARS [tasa] `shouldSatisfy` isLeft

    it "rechaza montos en cero" $ do
      let tasa = TasaDeCambio{id = nullUlid, monedaFrom = USD, monedaTo = ARS, montoFrom = 1, montoTo = 0}
      validarTasas ARS [tasa] `shouldSatisfy` isLeft

    it "rechaza dos tasas para el mismo par, aunque estén al revés" $ do
      let arsUsd = TasaDeCambio{id = nullUlid, monedaFrom = ARS, monedaTo = USD, montoFrom = 1000, montoTo = 1}
      validarTasas ARS [usdArs, arsUsd] `shouldSatisfy` isLeft

  describe "normalizarTasa" $ do
    it "deja el par como está si ya viene en orden" $
      normalizarTasa usdArs{monedaFrom = ARS, monedaTo = USD, montoFrom = 1000, montoTo = 1}
        `shouldBe` usdArs{monedaFrom = ARS, monedaTo = USD, montoFrom = 1000, montoTo = 1}

    it "da vuelta el par y los montos si viene al revés" $
      normalizarTasa usdArs
        `shouldBe` usdArs{monedaFrom = ARS, monedaTo = USD, montoFrom = 1000, montoTo = 1}

    it "no le cambia el sentido a la tasa" $ do
      let normalizada = normalizarTasa usdArs
      factorEntre [normalizada] USD ARS `shouldBe` factorEntre [usdArs] USD ARS
      factorEntre [normalizada] ARS USD `shouldBe` factorEntre [usdArs] ARS USD

    it "manda cada par a una sola forma, se escriba como se escriba" $
      forM_ [(una, otra) | una <- todasLasMonedas, otra <- todasLasMonedas, una /= otra] $ \(una, otra) -> do
        let ida = usdArs{monedaFrom = una, monedaTo = otra, montoFrom = 2, montoTo = 3}
            vuelta = usdArs{monedaFrom = otra, monedaTo = una, montoFrom = 3, montoTo = 2}
        normalizarTasa ida `shouldBe` normalizarTasa vuelta
