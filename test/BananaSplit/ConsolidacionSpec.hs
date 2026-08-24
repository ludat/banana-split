module BananaSplit.ConsolidacionSpec (
  spec,
) where

import Data.Map.Strict qualified as Map
import Protolude
import Test.Hspec
import Test.QuickCheck

import BananaSplit
import BananaSplit.TestUtils

spec :: Spec
spec = do
  let
    u1 = participante 1
    u2 = participante 2
    u3 = participante 3
    cotizaciones = PorMoneda . Map.fromList

  describe "consolidarNetos" $ do
    it "con una sola moneda igual a la destino es la identidad" $ do
      let netosArs = netos [(u1, 100), (u2, -100)]
      consolidarNetos ARS mempty (netosArs `enMoneda` ARS)
        `shouldBe` Right netosArs

    it "ignora la cotización de la moneda destino" $ do
      let netosArs = netos [(u1, 100), (u2, -100)]
      consolidarNetos ARS (cotizaciones [(ARS, 2)]) (netosArs `enMoneda` ARS)
        `shouldBe` Right netosArs

    it "convierte y suma netos de varias monedas" $ do
      let porMoneda =
            netos [(u1, 100), (u2, -100)] `enMoneda` ARS
              <> netos [(u1, -10), (u3, 10)] `enMoneda` USD
      consolidarNetos ARS (cotizaciones [(USD, 1200)]) porMoneda
        `shouldBe` Right (netos [(u1, 100 - 12000), (u2, -100), (u3, 12000)])

    it "falla si falta la cotización de una moneda presente" $ do
      let porMoneda = netos [(u1, 10), (u2, -10)] `enMoneda` USD
      consolidarNetos ARS mempty porMoneda
        `shouldBe` Left (CotizacionFaltante USD)

    it "falla con una cotización cero o negativa" $ do
      let porMoneda = netos [(u1, 10), (u2, -10)] `enMoneda` USD
      consolidarNetos ARS (cotizaciones [(USD, 0)]) porMoneda
        `shouldBe` Left (CotizacionInvalida USD)
      consolidarNetos ARS (cotizaciones [(USD, -3)]) porMoneda
        `shouldBe` Left (CotizacionInvalida USD)

    it "descuenta el residuo de redondeo del neto de mayor valor absoluto" $ do
      -- 0.33 * 1.5 = 0.495 que redondea a 0.50 dos veces, mientras que
      -- -0.66 * 1.5 = -0.99: el total daría 0.01 y el residuo se descuenta
      -- del neto más grande (u3).
      let porMoneda =
            netos [(u1, mkMonto 2 33), (u2, mkMonto 2 33), (u3, mkMonto 2 (-66))]
              `enMoneda` USD
      consolidarNetos ARS (cotizaciones [(USD, mkMonto 1 15)]) porMoneda
        `shouldBe` Right (netos [(u1, mkMonto 2 50), (u2, mkMonto 2 50), (u3, mkMonto 2 (-100))])

    it "los netos consolidados siempre suman cero" $
      property $
        forAll genNetosMultiMoneda $ \porMoneda ->
          forAll genCotizaciones $ \cotis ->
            case consolidarNetos ARS cotis porMoneda of
              Left e -> counterexample (show e) False
              Right consolidado -> totalNetos consolidado === 0

genNetosMultiMoneda :: Gen (PorMoneda (Netos Monto))
genNetosMultiMoneda = do
  monedas <- sublistOf todasLasMonedas
  mconcat <$> forM monedas (\moneda -> (`enMoneda` moneda) <$> genNetosSumaCero)

genNetosSumaCero :: Gen (Netos Monto)
genNetosSumaCero = do
  montos <- listOf (mkMonto 2 <$> arbitrary)
  let contrapeso = negate (sum montos)
  pure $ netos $ (participante 0, contrapeso) : zip (participante <$> [1 ..]) montos

genCotizaciones :: Gen (PorMoneda Monto)
genCotizaciones =
  fmap (PorMoneda . Map.fromList) $
    forM todasLasMonedas $ \moneda -> do
      valor <- mkMonto 2 . getPositive <$> arbitrary
      pure (moneda, valor)
