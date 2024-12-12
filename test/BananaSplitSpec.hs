module BananaSplitSpec where

import Test.Hspec
import BananaSplit
import qualified Data.Map as Map
import Data.ULID
import qualified Data.Text as Text

spec :: Spec
spec = describe "calcularDeudasTotales" $ do
  it "calcula que no hay deudas cuando nadie debe nada" $ do
    calcularDeudasPago (Pago
      { pagoId = fakeUlid 10
      , monto = 1000
      , nombre = "cosa"
      , pagadores =
          [ Ponderado 1 (participante 1)
          ]
      , deudores =
          [ Ponderado 1 (participante 1)
          , Ponderado 1 (participante 2)
          ]
      }) `shouldBe`
            Deudas (Map.fromList
              [ (participante 1, Monto 500)
              , (participante 2, Monto (-500))
              ])

participante :: Integer -> ParticipanteId
participante = ParticipanteId . fakeUlid

fakeUlid :: Integer -> ULID
fakeUlid integer =
  case ulidFromInteger integer of
    Right ulid -> ulid
    Left e -> error $ Text.unpack e