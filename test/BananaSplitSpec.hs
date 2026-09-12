module BananaSplitSpec (
  spec,
) where

import Data.Time (fromGregorian)
import Protolude
import Test.Hspec

import BananaSplit
import BananaSplit.TestUtils

pagoValido :: Pago
pagoValido =
  Pago
    { pagoId = nullUlid
    , nombre = "Pago"
    , monto = 200
    , moneda = ARS
    , fecha = fromGregorian 2025 1 1
    , deudores =
        distribucionMontosEspecificos
          [ (participante 1, 200)
          ]
    , pagadores =
        distribucionMontosEspecificos
          [ (participante 2, 200)
          ]
    }

spec :: Spec
spec = describe "Pago" $ do
  describe "#isValid" $ do
    it "un pago completo es valido" $
      pagoValido `shouldSatisfy` isValid
    it "un pago con monto de deudores negativo es invalido" $
      pagoValido
        { deudores =
            distribucionMontosEspecificos
              [ (participante 1, -100)
              ]
        }
        `shouldNotSatisfy` isValid
    it "un pago con monto de deudores 0 es invalido" $
      pagoValido
        { deudores =
            distribucionMontosEspecificos
              [ (participante 1, 0)
              ]
        }
        `shouldNotSatisfy` isValid
    it "un pago con monto de pagadores negativo es invalido" $
      pagoValido
        { pagadores =
            distribucionMontosEspecificos
              [ (participante 1, -100)
              ]
        }
        `shouldNotSatisfy` isValid
    it "un pago con monto de pagadores 0 es invalido" $
      pagoValido
        { pagadores =
            distribucionMontosEspecificos
              [ (participante 1, 0)
              ]
        }
        `shouldNotSatisfy` isValid

    it "un pago con monto de pagadores que difiere del monto de deudores es invalido" $
      pagoValido
        { pagadores =
            distribucionMontosEspecificos
              [ (participante 1, 100)
              ]
        , deudores =
            distribucionMontosEspecificos
              [ (participante 2, 101)
              ]
        }
        `shouldNotSatisfy` isValid

    it "un pago sin deudores es invalido" $
      pagoValido
        { deudores = distribucionMontosEspecificos []
        }
        `shouldNotSatisfy` isValid

    it "un pago sin pagadores es invalido" $
      pagoValido
        { pagadores = distribucionMontosEspecificos []
        }
        `shouldNotSatisfy` isValid

    it "un pago con monto de pagadores que difiere del monto del pago es invalido" $
      pagoValido
        { monto = 200
        , pagadores =
            distribucionMontosEspecificos
              [ (participante 1, 100)
              ]
        }
        `shouldNotSatisfy` isValid
    it "un pago con monto de deudores que difiere del monto del pago es invalido" $
      pagoValido
        { monto = 200
        , deudores =
            distribucionMontosEspecificos
              [ (participante 1, 100)
              ]
        }
        `shouldNotSatisfy` isValid

  describe "#getResumenGasto" $ do
    it "separa lo que puso cada uno de lo que consumio cada uno" $ do
      let resumen = getResumenGasto pagoValido
      resumen.pagado `shouldBe` netos [(participante 2, 200)]
      resumen.consumido `shouldBe` netos [(participante 1, 200)]
      resumen.errores `shouldBe` []

    -- Este es el invariante que justifica guardar los dos lados por separado:
    -- restarlos tiene que dar exactamente los netos que se calculaban antes.
    it "el neto de cada participante es lo que puso menos lo que consumio" $
      netosDeGasto (getResumenGasto pagoValido)
        `shouldBe` calcularNetosPago pagoValido

    it "vale tambien cuando alguien paga y consume lo mismo" $ do
      let gastoPropio =
            pagoValido
              { pagadores = distribucionMontosEspecificos [(participante 1, 200)]
              , deudores = distribucionMontosEspecificos [(participante 1, 200)]
              }
      netosDeGasto (getResumenGasto gastoPropio)
        `shouldBe` calcularNetosPago gastoPropio
      getResumenGasto gastoPropio `shouldSatisfy` gastoEsValido

    it "un pago invalido dice por que lo es" $ do
      let resumen = getResumenGasto pagoValido{deudores = distribucionMontosEspecificos []}
      resumen `shouldNotSatisfy` gastoEsValido
      fmap (.objeto) resumen.errores `shouldBe` [["deudores"]]
