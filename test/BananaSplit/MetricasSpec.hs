module BananaSplit.MetricasSpec (
  spec,
) where

import Data.Map.Strict qualified as Map
import Data.Time (fromGregorian)
import Protolude
import Test.Hspec

import BananaSplit
import BananaSplit.TestUtils

spec :: Spec
spec = describe "calcularMetricas" $ do
  let
    u1 = participante 1
    u2 = participante 2
    u3 = participante 3

    -- u1 paga una compra de 300 en la que u1 y u2 gastan 150 cada uno.
    gastoChico :: Pago
    gastoChico =
      Pago
        { pagoId = fakeUlid 1
        , monto = 300
        , moneda = ARS
        , isValid = True
        , nombre = "Compra chica"
        , fecha = fromGregorian 2025 1 5
        , pagadores = distribucionMontosEspecificos [(u1, 300)]
        , deudores = distribucionMontosEspecificos [(u1, 150), (u2, 150)]
        }

    -- u2 paga la compra grande de 1000, la gastan entre u2 y u3.
    gastoGrande :: Pago
    gastoGrande =
      Pago
        { pagoId = fakeUlid 2
        , monto = 1000
        , moneda = ARS
        , isValid = True
        , nombre = "Compra grande"
        , fecha = fromGregorian 2025 2 10
        , pagadores = distribucionMontosEspecificos [(u2, 1000)]
        , deudores = distribucionMontosEspecificos [(u2, 500), (u3, 500)]
        }

    -- u3 le transfiere 200 a u1 para saldar cuentas: esto es una liquidación.
    liquidacion :: Pago
    liquidacion =
      Pago
        { pagoId = fakeUlid 3
        , monto = 200
        , moneda = ARS
        , isValid = True
        , nombre = "Transferencia"
        , fecha = fromGregorian 2025 2 10
        , pagadores = distribucionMontoEquitativo [u3]
        , deudores = distribucionMontoEquitativo [u1]
        }

    -- Un pago inválido no debería afectar ninguna métrica.
    pagoInvalido :: Pago
    pagoInvalido =
      Pago
        { pagoId = fakeUlid 4
        , monto = 5000
        , moneda = ARS
        , isValid = False
        , nombre = "Pago roto"
        , fecha = fromGregorian 2025 3 1
        , pagadores = distribucionMontosEspecificos [(u1, 100)]
        , deudores = distribucionMontosEspecificos [(u1, 5000)]
        }

    grupo :: Grupo
    grupo =
      Grupo
        { id = fakeUlid 0
        , nombre = "Grupo de test"
        , participantes = []
        , monedaPorDefecto = ARS
        , pagos = [gastoChico, gastoGrande, liquidacion, pagoInvalido]
        }

    metricas = calcularMetricas grupo
    (PorMoneda porMonedaMap) = metricas.porMoneda
    datosArs = Map.lookup ARS porMonedaMap

  describe "esLiquidacion" $ do
    it "detecta una transferencia simple como liquidación" $
      liquidacion `shouldSatisfy` esLiquidacion

    it "no considera liquidación un gasto repartido entre varios deudores" $
      gastoChico `shouldNotSatisfy` esLiquidacion

  describe "totalPagadoPorParticipante" $ do
    it "suma lo pagado por cada participante en todos los pagos válidos, incluyendo liquidaciones" $ do
      let Just datos = datosArs
      getNetos' datos.totalPagadoPorParticipante u1 `shouldBe` 300
      getNetos' datos.totalPagadoPorParticipante u2 `shouldBe` 1000
      getNetos' datos.totalPagadoPorParticipante u3 `shouldBe` 200

  describe "totalPagosSaldadosPorParticipante" $ do
    it "sólo cuenta lo pagado en liquidaciones" $ do
      let Just datos = datosArs
      getNetos' datos.totalPagosSaldadosPorParticipante u3 `shouldBe` 200
      getNetos' datos.totalPagosSaldadosPorParticipante u1 `shouldBe` 0

  describe "totalGastadoPorParticipante" $ do
    it "excluye liquidaciones del cálculo de gasto" $ do
      let Just datos = datosArs
      getNetos' datos.totalGastadoPorParticipante u1 `shouldBe` 150
      getNetos' datos.totalGastadoPorParticipante u2 `shouldBe` 650
      getNetos' datos.totalGastadoPorParticipante u3 `shouldBe` 500

  describe "generosidad" $ do
    it "es mayor a 0 para participantes que gastaron y pagaron" $ do
      let Just datos = datosArs
      case Map.lookup u2 datos.generosidad of
        Just ratio -> ratio `shouldSatisfy` (> 0)
        Nothing -> expectationFailure "se esperaba un valor de generosidad para u2"

  describe "busiestMes" $ do
    it "identifica el mes con más plata movida" $ do
      let Just datos = datosArs
      fmap (\m -> (m.anio, m.mes)) datos.busiestMes `shouldBe` Just (2025, 2)

  describe "busiestDia" $ do
    it "identifica el día con más plata movida" $ do
      let Just datos = datosArs
      fmap (.dia) datos.busiestDia `shouldBe` Just (fromGregorian 2025 2 10)

  describe "theBigOne" $ do
    it "es el pago más grande, excluyendo liquidaciones" $ do
      let Just datos = datosArs
      fmap (.pagoId) datos.theBigOne `shouldBe` Just (fakeUlid 2)
  where
    getNetos' (Netos m) p = Map.findWithDefault 0 p m
