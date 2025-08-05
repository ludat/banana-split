module BananaSplit.SolverSpec
    ( spec
    ) where

import BananaSplit
import BananaSplit.TestUtils

import Data.Decimal qualified as Decimal

import Protolude

import Test.Hspec

spec :: Spec
spec = do
  let
    u1 = participante 1
    u2 = participante 2
    u3 = participante 3
    u4 = participante 4
    u5 = participante 5
    u6 = participante 6
  describe "calcularDeudas" $ do
    it "calcula deudas de un pago con ponderador" $ do
      calcularDeudasPago Pago
        { pagoId = fakeUlid 100
        , monto = 1000
        , nombre = "cosa"
        , pagadores =
            [ Ponderado 1 u1
            ]
        , deudores =
            [ Ponderado 1 u1
            , Ponderado 1 u2
            ]
        } `shouldBe` deudas
          [ (u1, 500)
          , (u2, -500)
          ]

    it "devuelve deudas vacias si el pago es invalido" $ do
      calcularDeudasPago Pago
        { pagoId = fakeUlid 100
        , monto = 1000
        , nombre = "cosa"
        , pagadores = []
        , deudores =
            [ Ponderado 1 u1
            , Ponderado 1 u2
            ]
        } `shouldBe` mempty

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
        [ (u1, Monto $ Decimal.Decimal 2 66)
        , (u2, Monto $ Decimal.Decimal 2 -33)
        , (u3, Monto $ Decimal.Decimal 2 -33)
        ])`shouldSatisfy` ((== 2) . length)
    context "con deudas no coherentes (no suman 0 en total)" $ do
      it "con una deuda simple que esta desbalanceada" $ do
        minimizeTransactions (deudas
          [ (u1, Monto $ Decimal.Decimal 0 10)
          , (u2, Monto $ Decimal.Decimal 0 -11)
          ])`shouldBe` [Transaccion
            { transaccionFrom = u2
            , transaccionTo = u1
            , transaccionMonto = 10
            }
          ]
      it "con una deuda compleja no crashea" $ do
        minimizeTransactions (deudas
          [ (u1, 10)
          , (u2, -5)
          , (u3, -5)
          , (u4,  6)
          , (u5, -3)
          , (u6, -2)
          ]) `shouldBe`
            [ Transaccion
            { transaccionFrom = u2
            , transaccionTo = u4
            , transaccionMonto = 6
            }
            , Transaccion
            { transaccionFrom = u3
            , transaccionTo = u1
            , transaccionMonto = 5
            }
            , Transaccion
            { transaccionFrom = u5
            , transaccionTo = u1
            , transaccionMonto = 3
            }
            , Transaccion
            { transaccionFrom = u6
            , transaccionTo = u1
            , transaccionMonto = 2
            }
            ]
      it "cuando una sola persona tiene plata a favor" $ do
        pendingWith "this crashes the solver"
        minimizeTransactions (deudas
          [ (u3, Monto $ Decimal.Decimal 2 -3)
          ])`shouldBe` []

      -- xit "manual testing" $ do
      --   let
      --     grupo :: Grupo
      --     grupo = case fromJSON @Grupo [aesonQQ|
      --         |] of
      --           Success x -> x
      --           Error _ -> error "no json"

      --   minimizeTransactions (calcularDeudasTotales grupo) `shouldBe` []

deudas :: [(ParticipanteId, Monto)] -> Deudas Monto
deudas l =
  l
  & fmap (uncurry mkDeuda)
  & mconcat
