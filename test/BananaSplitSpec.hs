module BananaSplitSpec
    ( spec
    ) where

import BananaSplit
import BananaSplit.TestUtils

import Protolude

import Test.Hspec

pagoValido :: Pago
pagoValido =
  Pago
  { pagoId = nullUlid
  , isValid = True
  , nombre = "Pago"
  , monto = 200
  , deudores = distribucionMontosEspecificos
    [ (participante 1, 200)
    ]
  , pagadores = distribucionMontosEspecificos
    [ (participante 2, 200)
    ]
  }

spec :: Spec
spec = describe "Pago" $ do
  describe "#isValid" $ do
    it "un pago completo es valido" $
      pagoValido `shouldSatisfy` isValid
    it "un pago con monto de deudores negativo es invalido" $
      pagoValido {
        deudores = distribucionMontosEspecificos
          [ (participante 1, -100)
          ]
      } `shouldNotSatisfy` isValid
    it "un pago con monto de deudores 0 es invalido" $
      pagoValido {
        deudores = distribucionMontosEspecificos
          [ (participante 1, 0)
          ]
      } `shouldNotSatisfy` isValid
    it "un pago con monto de pagadores negativo es invalido" $
      pagoValido {
        pagadores = distribucionMontosEspecificos
          [ (participante 1, -100)
          ]
      } `shouldNotSatisfy` isValid
    it "un pago con monto de pagadores 0 es invalido" $
      pagoValido {
        pagadores = distribucionMontosEspecificos
          [ (participante 1, 0)
          ]
      } `shouldNotSatisfy` isValid

    it "un pago con monto de pagadores que difiere del monto de deudores es invalido" $
      pagoValido {
        pagadores = distribucionMontosEspecificos
          [ (participante 1, 100)
          ]
        , deudores = distribucionMontosEspecificos
          [ (participante 2, 101)
          ]
      } `shouldNotSatisfy` isValid

    it "un pago sin deudores es invalido" $
      pagoValido {
        deudores = distribucionMontosEspecificos []
      } `shouldNotSatisfy` isValid

    it "un pago sin pagadores es invalido" $
      pagoValido {
        pagadores = distribucionMontosEspecificos []
      } `shouldNotSatisfy` isValid

    it "un pago con monto de pagadores que difiere del monto del pago es invalido" $
      pagoValido
        { monto = 200
        , pagadores = distribucionMontosEspecificos
          [ (participante 1, 100)
          ]
      } `shouldNotSatisfy` isValid
    it "un pago con monto de deudores que difiere del monto del pago es invalido" $
      pagoValido
        { monto = 200
        , deudores = distribucionMontosEspecificos
          [ (participante 1, 100)
          ]
      } `shouldNotSatisfy` isValid
