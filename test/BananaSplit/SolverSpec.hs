module BananaSplit.SolverSpec
    ( spec
    ) where

import BananaSplit

import Data.Function ((&))
import Data.Text qualified as Text
import Data.ULID

import Test.Hspec

spec :: Spec
spec = describe "simplify transactions" $ do
  let
    u1 = participante 1
    u2 = participante 2
    u3 = participante 3
    u4 = participante 4
    u5 = participante 5
    u6 = participante 6

  it "simplifica ningun transaccion trivialmente" $ do
    minimizeTransactions mempty `shouldBe` []

  it "simplifica una sola transaccion trivialmente" $ do
    let transacciones = deuda [(participante 1, 10), (participante 2, -10)]

    minimizeTransactions transacciones `shouldBe` [Transaccion (participante 2) (participante 1) 10]
  it "simplifica un caso en el que el algoritmo greedy falla" $ do
    minimizeTransactions (deuda
      [ (u1, 10)
      , (u2, -5)
      , (u3, -5)
      , (u4,  6)
      , (u5, -3)
      , (u6, -3)
      ])`shouldSatisfy` ((== 4) . length)

deuda :: [(ParticipanteId, Monto)] -> Deudas Monto
deuda l =
  l
  & fmap (uncurry mkDeuda)
  & mconcat

participante :: Integer -> ParticipanteId
participante = ParticipanteId . fakeUlid

fakeUlid :: Integer -> ULID
fakeUlid integer =
  case ulidFromInteger integer of
    Right ulid -> ulid
    Left e -> error $ Text.unpack e
