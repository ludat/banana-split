module BananaSplit.DeudasSpec
    ( spec
    ) where

import BananaSplit
import BananaSplit.TestUtils

import Data.Decimal qualified as Decimal

import Protolude

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  let
    u1 = participante 1
    u2 = participante 2
    u3 = participante 3
    u4 = participante 4
    u5 = participante 5
    u6 = participante 6
    pagoValido :: Pago
    pagoValido =
      Pago
      { pagoId = fakeUlid 100
      , monto = 1000
      , isValid = True
      , nombre = "Pago"
      , pagadores = distribucionMontosEspecificos []
      , deudores = distribucionMontosEspecificos []
      }

  describe "calcularDeudas" $ do
    it "calcula deudas de un pago montos especificos en pagadores y deudores" $ do
      calcularDeudasPago pagoValido
        { monto = 500
        , pagadores = distribucionMontosEspecificos
          [ (u1, 500)
          ]
        , deudores = distribucionMontosEspecificos
          [ (u2, 500)
          ]
        } `shouldBe` deudas
          [ (u1, 500)
          , (u2, -500)
          ]

  describe "calcularDeudasMontoEquitativo" $ do
    it "entre varias personas devuelve equitativo" $ do
      getDeudas 300
        (distribucionMontoEquitativo [u1, u2, u3])
        `shouldBe` Just (deudas
          [ (u1, 100)
          , (u2, 100)
          , (u3, 100)
          ])
    it "calcular deudas de un monto equitativo siempre suma el total de nuevo" $ property $ \(Positive m :: Positive Int) (NonEmpty us :: NonEmptyList (Positive Integer))-> do
      let monto = fromIntegral @_ @Monto m
      let participants =  us <&> participante . getPositive
      (getDeudas monto
        (distribucionMontoEquitativo participants)
            & fmap totalDeudas)
            `shouldBe` Just monto

    it "con monto cero devuelve deudas vacias" $ do
      pendingWith "Not sure if this is the right default right now"
      getDeudas 0
        (distribucionMontoEquitativo [u1, u2, u3])
        `shouldBe` Just (deudas [])

  describe "simplify transactions" $ do
    it "simplifica ningun transaccion trivialmente" $ do
      minimizeTransactions mempty `shouldBe` []

    it "simplifica una sola transaccion trivialmente" $ do
      let transacciones = deudas [(participante 1, 10), (participante 2, -10)]

      minimizeTransactions transacciones `shouldBe` [Transaccion (participante 2) (participante 1) 10]
    it "simplifica un caso en el que el algoritmo greedy falla" $ do
      minimizeTransactions (deudas
        [ (u1, 10)
        , (u2, -5)
        , (u3, -5)
        , (u4,  6)
        , (u5, -3)
        , (u6, -3)
        ])`shouldSatisfy` ((== 4) . length)
    it "devuelve vacio si se le pasa vacio" $ do
      minimizeTransactions (deudas []) `shouldBe` []
    it "simplifica un caso con numeros con coma" $ do
      minimizeTransactions (deudas
        [ (u1, mkMonto 2 66)
        , (u2, mkMonto 2 -33)
        , (u3, mkMonto 2 -33)
        ])`shouldSatisfy` ((== 2) . length)
    context "con deudas no coherentes (no suman 0 en total)" $ do
      it "con una deuda simple que esta desbalanceada" $ do
        evaluate (minimizeTransactions (deudas
          [ (u1, Monto $ Decimal.Decimal 0 10)
          , (u2, Monto $ Decimal.Decimal 0 -11)
          ])) `shouldThrow` errorCall "Balanace is not 0, instead is: -1.0"

      it "con una deuda compleja no crashea" $ do
        evaluate (minimizeTransactions (deudas
          [ (u1, 10)
          , (u2, -5)
          , (u3, -5)
          , (u4,  6)
          , (u5, -3)
          , (u6, -2)
          ])) `shouldThrow` errorCall "Balanace is not 0, instead is: 1.0"

      it "cuando una sola persona tiene plata a favor" $ do
        pendingWith "this crashes the solver"
        evaluate (minimizeTransactions (deudas
          [ (u3, Monto $ Decimal.Decimal 2 -3)
          ])) `shouldThrow` errorCall "Balanace is not 0, instead is: -1.0"

      -- xit "manual testing" $ do
      --   let
      --     grupo :: Grupo
      --     grupo = case fromJSON @Grupo [aesonQQ|
      --         |] of
      --           Success x -> x
      --           Error _ -> error "no json"

      --   minimizeTransactions (calcularDeudasTotales grupo) `shouldBe` []
    context "cuando hay netos con coma y numeros decimales extraños" $ do
      it "una deuda simple con montos con mas de dos decimales" $ do
        minimizeTransactions (deudas
          [ ( u1, mkMonto 10  5)
          , ( u2, mkMonto 10 -5)
          ]) `shouldBe`
            [ Transaccion u2 u1 $ mkMonto 10 5
            ]

      it "una deuda simple con montos heterogeneos" $ do
        minimizeTransactions (deudas
          [ ( u1, mkMonto 10  5)
          , ( u2, mkMonto 10 -5)
          , ( u3, mkMonto 0  5)
          , ( u4, mkMonto 0 -5)
          ]) `shouldBe`
            [ Transaccion u2 u1 $ mkMonto 10 5
            , Transaccion u4 u3 5
            ]
