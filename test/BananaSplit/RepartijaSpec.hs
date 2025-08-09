module BananaSplit.RepartijaSpec where

import BananaSplit
import BananaSplit.TestUtils

import Protolude

import Test.Hspec

spec :: Spec
spec = describe "validar repartija" $ do
  it "una repartija vacia genera un pago vacio" $ do
    calcularDeudasRepartija (Repartija
        { id = fakeUlid 1
        , extra = 0
        , claims = []
        , items = []
        })
      `shouldBe` deudas []
  it "con un items repartidos generamos un pago acorde" $ do
    let participante1 = participante 32
    let item1 = fakeUlid 2
    calcularDeudasRepartija (Repartija
        { id = fakeUlid 1
        , extra = 0
        , items =
          [ RepartijaItem item1 "Birrita" 100 1
          ]
        , claims =
          [ RepartijaClaim (fakeUlid 100) participante1 item1 (Just 1)
          ]
        })
      `shouldBe` deudas [
        (participante1, 100)
      ]
  it "con un item repartido entre varios participantes generamos un pago acorde" $ do
    let participante1 = participante 32
    let participante2 = participante 33
    let item1 = fakeUlid 2
    calcularDeudasRepartija (Repartija
        {  id = fakeUlid 1
        , extra = 0
        , items =
          [ RepartijaItem item1 "Birrita" 200 2
          ]
        , claims =
          [ RepartijaClaim (fakeUlid 100) participante1 item1 (Just 1)
          , RepartijaClaim (fakeUlid 101) participante2 item1 (Just 1)
          ]
        })
      `shouldBe` deudas
        [ (participante1, 100)
        , (participante2, 100)
        ]
  it "con algunos items repartidos generamos un pago acorde" $ do
    let participante1 = participante 32
    let participante2 = participante 33
    let birrita = fakeUlid 2
    let papitas = fakeUlid 3

    calcularDeudasRepartija (Repartija
        { id = fakeUlid 1
        , extra = 0
        , items =
          [ RepartijaItem birrita "Birrita" 200 2
          , RepartijaItem papitas "Papitas" 300 1
          ]
        , claims =
          [ RepartijaClaim (fakeUlid 100) participante1 birrita (Just 1)
          , RepartijaClaim (fakeUlid 101) participante2 birrita (Just 1)
          , RepartijaClaim (fakeUlid 102) participante1 papitas Nothing
          , RepartijaClaim (fakeUlid 103) participante2 papitas Nothing
          ]
        })
      `shouldBe`  deudas
            [ (participante1, 100)
            , (participante2, 100)
            , (participante1, 150)
            , (participante2, 150)
            ]
  it "con algunos items repartidos y propina generamos un pago acorde" $ do
    let participante1 = participante 32
    let participante2 = participante 33
    let birrita = fakeUlid 2
    let papitas = fakeUlid 3
    calcularDeudasRepartija (Repartija
        { id = fakeUlid 1
        , extra = 50
        , items =
          [ RepartijaItem birrita "Birrita" 200 2
          , RepartijaItem papitas "Papitas" 300 1
          ]
        , claims =
          [ RepartijaClaim (fakeUlid 100) participante1 birrita (Just 1)
          , RepartijaClaim (fakeUlid 101) participante2 birrita (Just 1)
          , RepartijaClaim (fakeUlid 102) participante1 papitas Nothing
          ]
        })
      `shouldBe` deudas
            [ (participante1, 100)
            , (participante2, 100)
            , (participante1, 300)
            , (participante1, 40)
            , (participante2, 10)
            ]
  it "con algunos items repartidos cuando sobra alguno los repartimos como si fueran propina" $ do
    let participante1 = participante 32
    let participante2 = participante 33
    let birrita = fakeUlid 2
    let papitas = fakeUlid 3
    calcularDeudasRepartija (Repartija
        { id = fakeUlid 1
        , extra = 0
        , items =
          [ RepartijaItem birrita "Birrita" 200 2
          , RepartijaItem papitas "Papitas" 300 1
          ]
        , claims =
          [ RepartijaClaim (fakeUlid 100) participante1 birrita (Just 1)
          , RepartijaClaim (fakeUlid 101) participante2 birrita (Just 1)
          ]
        })
      `shouldBe` deudas
        [ (participante1, 100)
        , (participante2, 100)
        , (participante1, 150)
        , (participante2, 150)
        ]
