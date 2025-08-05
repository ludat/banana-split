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
  , monto = Monto 200.00
  , deudores = [Ponderado 1 (participante 1)]
  , pagadores = [Ponderado 1 (participante 1)]
  }

spec :: Spec
spec = describe "Pago" $ do
  describe "#isValid" $ do
    it "un pago completo es valido" $
      pagoValido `shouldSatisfy` isPagoValid
    it "un pago con monto negativo es invalido" $
      pagoValido {
        monto = -2
      } `shouldNotSatisfy` isPagoValid
    it "un pago con monto 0 es invalido" $
      pagoValido {
        monto = 0
      } `shouldNotSatisfy` isPagoValid

    it "un pago sin deudores es invalido" $
      pagoValido {
        deudores = []
      } `shouldNotSatisfy` isPagoValid

    it "un pago sin pagadores es invalido" $
      pagoValido {
        pagadores = []
      } `shouldNotSatisfy` isPagoValid
