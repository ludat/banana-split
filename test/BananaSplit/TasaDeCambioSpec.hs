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

    usdArs = tasaEntre USD 1 ARS 1000

    -- La tabla se arma para la moneda a la que se convierte, así que el destino
    -- es también su base.
    factor tasas desde hasta = factorEntre (tablaDeTasas hasta tasas) desde & fmap unFactor

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
      let clpEur = tasaEntre CLP 1000 EUR 1
      factor [clpEur] CLP EUR `shouldBe` Just (1 / 1000)

  -- Guardar acepta las tasas si entraron todas, así que lo que la tabla
  -- descarta es lo que el PUT rechaza.
  describe "cantidadDeTasas" $ do
    it "usa una tasa por cada moneda que no es la base" $ do
      let eurArs = tasaEntre EUR 1 ARS 2000
      cantidadDeTasas (tablaDeTasas ARS [usdArs, eurArs]) `shouldBe` 2

    it "descarta la tasa que no involucra a la base" $ do
      let usdEur = tasaEntre USD 1 EUR 1
      cantidadDeTasas (tablaDeTasas ARS [usdEur]) `shouldBe` 0

    it "descarta la tasa entre una moneda y sí misma" $ do
      let arsArs = tasaEntre ARS 1 ARS 1
      cantidadDeTasas (tablaDeTasas ARS [arsArs]) `shouldBe` 0

    it "descarta la tasa con un lado en cero" $ do
      let rota = tasaEntre USD 0 ARS 1000
      cantidadDeTasas (tablaDeTasas ARS [rota]) `shouldBe` 0

    it "se queda con una sola cuando el par viene repetido al revés" $
      cantidadDeTasas (tablaDeTasas ARS [usdArs, tasaEntre ARS 1000 USD 1]) `shouldBe` 1

  describe "consolidarNetos" $ do
    it "suma las otras monedas convertidas a la moneda destino" $ do
      let porMoneda =
            netos [(u1, 100), (u2, -100)]
              `enMoneda` ARS
              <> netos [(u1, -1), (u2, 1)]
              `enMoneda` USD
          consolidado = consolidarNetos (tablaDeTasas ARS [usdArs]) porMoneda

      consolidado.netos `shouldBe` netos [(u1, -900), (u2, 900)]
      consolidado.monedasConvertidas `shouldMatchList` [ARS, USD]
      consolidado.monedasSinTasa `shouldBe` []

    it "cancela una deuda con una transferencia hecha en otra moneda" $ do
      -- u2 le debe 1 USD a u1 por los gastos y se lo paga con 1000 ARS. La
      -- transferencia entra en el bucket de ARS, así que solo queda en cero si
      -- las tasas se aplican antes de sumar las monedas entre sí.
      let deudaEnUsd = netos [(u1, 1), (u2, -1)] `enMoneda` USD
          transferenciaEnArs =
            netosDeTransferencia (Transferencia Nothing u2 u1 1000) `enMoneda` ARS
          consolidado =
            consolidarNetos (tablaDeTasas ARS [usdArs]) (deudaEnUsd <> transferenciaEnArs)

      consolidado.netos `shouldBe` netos [(u1, 0), (u2, 0)]
      consolidado.monedasSinTasa `shouldBe` []

    it "deja afuera las monedas sin tasa y las reporta" $ do
      let porMoneda =
            netos [(u1, 100), (u2, -100)]
              `enMoneda` ARS
              <> netos [(u1, -1), (u2, 1)]
              `enMoneda` EUR
          consolidado = consolidarNetos (tablaDeTasas ARS [usdArs]) porMoneda

      consolidado.netos `shouldBe` netos [(u1, 100), (u2, -100)]
      consolidado.monedasConvertidas `shouldBe` [ARS]
      consolidado.monedasSinTasa `shouldBe` [EUR]

    it "reparte el redondeo entre los acreedores en vez de amontonarlo en uno" $ do
      let tasa = tasaEntre USD 3 ARS 10
          u3 = participante 3
          porMoneda = netos [(u1, 1), (u2, 1), (u3, -2)] `enMoneda` USD
          consolidado = consolidarNetos (tablaDeTasas ARS [tasa]) porMoneda

      -- Los 2 USD que se deben son 6.67 ARS: el deudor se queda con el total
      -- convertido exacto y el centavo que sobra cae en un solo acreedor.
      consolidado.netos
        `shouldBe` netos [(u1, mkMonto 2 334), (u2, mkMonto 2 333), (u3, mkMonto 2 (-667))]

    it "deja afuera a los participantes que no deben ni les deben" $ do
      let u3 = participante 3
          porMoneda = netos [(u1, 1), (u2, -1), (u3, 0)] `enMoneda` USD
          consolidado = consolidarNetos (tablaDeTasas ARS [usdArs]) porMoneda

      consolidado.netos `shouldBe` netos [(u1, 1000), (u2, -1000)]

    it "los netos convertidos siguen sumando cero aunque el redondeo no cierre" $ do
      let tasa = tasaEntre USD 3 ARS 10
          u3 = participante 3
          porMoneda = netos [(u1, 1), (u2, 1), (u3, -2)] `enMoneda` USD
          consolidado = consolidarNetos (tablaDeTasas ARS [tasa]) porMoneda

      totalNetos consolidado.netos `shouldBe` 0

    it "la misma tasa sirve después de cambiar la moneda por defecto del grupo" $ do
      let porMoneda =
            netos [(u1, 1000), (u2, -1000)]
              `enMoneda` ARS
              <> netos [(u1, -3), (u2, 3)]
              `enMoneda` USD

      (consolidarNetos (tablaDeTasas ARS [usdArs]) porMoneda).netos
        `shouldBe` netos [(u1, -2000), (u2, 2000)]
      (consolidarNetos (tablaDeTasas USD [usdArs]) porMoneda).netos
        `shouldBe` netos [(u1, -2), (u2, 2)]

      (consolidarNetos (tablaDeTasas USD [usdArs]) porMoneda).monedasSinTasa `shouldBe` []
