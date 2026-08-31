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

    -- La tabla se arma para la moneda a la que se convierte, así que el destino
    -- es también su base. Las tasas de estos casos son sanas para esa base.
    tabla moneda tasas = tablaDeTasas moneda tasas

    factor tasas desde hasta = factorEntre (tabla hasta tasas) desde & fmap unFactor

  describe "factorEntre" $ do
    it "es 1 entre una moneda y sí misma, aunque no haya ninguna tasa" $
      factor [] ARS ARS `shouldBe` Just 1

    it "usa la tasa cargada tal cual" $
      factor [usdArs] USD ARS `shouldBe` Just 1000

    it "da vuelta la tasa cuando se pide al revés" $
      factor [usdArs] ARS USD `shouldBe` Just (1 / 1000)

    it "no convierte una moneda que no está en la tabla" $
      factor [usdArs] EUR USD `shouldBe` Nothing

    it "sirve una tasa que no arranca en 1" $ do
      let clpEur = TasaDeCambio{id = nullUlid, monedaFrom = CLP, monedaTo = EUR, montoFrom = 1000, montoTo = 1}
      factor [clpEur] CLP EUR `shouldBe` Just (1 / 1000)

  -- Guardar acepta las tasas si entraron todas, así que lo que la tabla
  -- descarta es lo que el PUT rechaza.
  describe "cantidadDeTasas" $ do
    it "usa una tasa por cada moneda que no es la base" $ do
      let eurArs = TasaDeCambio{id = nullUlid, monedaFrom = EUR, monedaTo = ARS, montoFrom = 1, montoTo = 2000}
      cantidadDeTasas (tabla ARS [usdArs, eurArs]) `shouldBe` 2

    it "descarta la tasa que no involucra a la base" $ do
      let usdEur = TasaDeCambio{id = nullUlid, monedaFrom = USD, monedaTo = EUR, montoFrom = 1, montoTo = 1}
      cantidadDeTasas (tabla ARS [usdEur]) `shouldBe` 0

    it "descarta la tasa entre una moneda y sí misma" $ do
      let arsArs = TasaDeCambio{id = nullUlid, monedaFrom = ARS, monedaTo = ARS, montoFrom = 1, montoTo = 1}
      cantidadDeTasas (tabla ARS [arsArs]) `shouldBe` 0

    it "descarta la tasa con un lado en cero" $ do
      let rota = usdArs{montoFrom = 0}
      cantidadDeTasas (tabla ARS [rota]) `shouldBe` 0

    it "se queda con una sola cuando el par viene repetido al revés" $ do
      let arsUsd = usdArs{monedaFrom = ARS, monedaTo = USD, montoFrom = 1000, montoTo = 1}
      cantidadDeTasas (tabla ARS [usdArs, arsUsd]) `shouldBe` 1

  describe "consolidarNetos" $ do
    it "suma las otras monedas convertidas a la moneda destino" $ do
      let porMoneda =
            netos [(u1, 100), (u2, -100)]
              `enMoneda` ARS
              <> netos [(u1, -1), (u2, 1)]
              `enMoneda` USD
          consolidado = consolidarNetos (tabla ARS [usdArs]) porMoneda

      consolidado.netos `shouldBe` netos [(u1, -900), (u2, 900)]
      consolidado.monedasConvertidas `shouldMatchList` [ARS, USD]
      consolidado.monedasSinTasa `shouldBe` []

    it "deja afuera las monedas sin tasa y las reporta" $ do
      let porMoneda =
            netos [(u1, 100), (u2, -100)]
              `enMoneda` ARS
              <> netos [(u1, -1), (u2, 1)]
              `enMoneda` EUR
          consolidado = consolidarNetos (tabla ARS [usdArs]) porMoneda

      consolidado.netos `shouldBe` netos [(u1, 100), (u2, -100)]
      consolidado.monedasConvertidas `shouldBe` [ARS]
      consolidado.monedasSinTasa `shouldBe` [EUR]

    it "reparte el redondeo entre los acreedores en vez de amontonarlo en uno" $ do
      let tasa = TasaDeCambio{id = nullUlid, monedaFrom = USD, monedaTo = ARS, montoFrom = 3, montoTo = 10}
          u3 = participante 3
          porMoneda = netos [(u1, 1), (u2, 1), (u3, -2)] `enMoneda` USD
          consolidado = consolidarNetos (tabla ARS [tasa]) porMoneda

      -- Los 2 USD que se deben son 6.67 ARS: el deudor se queda con el total
      -- convertido exacto y el centavo que sobra cae en un solo acreedor.
      consolidado.netos
        `shouldBe` netos [(u1, mkMonto 2 334), (u2, mkMonto 2 333), (u3, mkMonto 2 (-667))]

    it "deja afuera a los participantes que no deben ni les deben" $ do
      let u3 = participante 3
          porMoneda = netos [(u1, 1), (u2, -1), (u3, 0)] `enMoneda` USD
          consolidado = consolidarNetos (tabla ARS [usdArs]) porMoneda

      consolidado.netos `shouldBe` netos [(u1, 1000), (u2, -1000)]

    it "los netos convertidos siguen sumando cero aunque el redondeo no cierre" $ do
      let tasa = TasaDeCambio{id = nullUlid, monedaFrom = USD, monedaTo = ARS, montoFrom = 3, montoTo = 10}
          u3 = participante 3
          porMoneda = netos [(u1, 1), (u2, 1), (u3, -2)] `enMoneda` USD
          consolidado = consolidarNetos (tabla ARS [tasa]) porMoneda

      totalNetos consolidado.netos `shouldBe` 0

    it "la misma tasa sirve después de cambiar la moneda por defecto del grupo" $ do
      let porMoneda =
            netos [(u1, 1000), (u2, -1000)]
              `enMoneda` ARS
              <> netos [(u1, -3), (u2, 3)]
              `enMoneda` USD

      (consolidarNetos (tabla ARS [usdArs]) porMoneda).netos
        `shouldBe` netos [(u1, -2000), (u2, 2000)]
      (consolidarNetos (tabla USD [usdArs]) porMoneda).netos
        `shouldBe` netos [(u1, -2), (u2, 2)]

      (consolidarNetos (tabla USD [usdArs]) porMoneda).monedasSinTasa `shouldBe` []
