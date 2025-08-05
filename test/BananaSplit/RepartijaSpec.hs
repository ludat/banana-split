module BananaSplit.RepartijaSpec where

import BananaSplit
import BananaSplit.TestUtils

import Protolude

import Test.Hspec

spec :: Spec
spec = describe "validar repartija" $ do
  it "una repartija vacia genera un pago vacio" $ do
    repartija2Pago (Repartija
        { repartijaNombre = "un nombre"
        , repartijaGrupoId = nullUlid
        , repartijaId = fakeUlid 1
        , repartijaExtra = 0
        , repartijaClaims = []
        , repartijaItems = []
        })
      `shouldBe` Pago
          { pagoId = nullUlid
          , isValid = False
          , monto = 0
          , nombre = "un nombre"
          , deudores = []
          , pagadores = []
          }
  it "con un items repartidos generamos un pago acorde" $ do
    let participante1 = participante 32
    let item1 = fakeUlid 2
    repartija2Pago (Repartija
        { repartijaNombre = "un nombre"
        , repartijaGrupoId = nullUlid
        , repartijaId = fakeUlid 1
        , repartijaExtra = 0
        , repartijaItems =
          [ RepartijaItem item1 "Birrita" 100 1
          ]
        , repartijaClaims =
          [ RepartijaClaim (fakeUlid 100) participante1 item1 (Just 1)
          ]
        })
      `shouldBe` Pago
          { pagoId = nullUlid
          , isValid = False
          , monto = 100
          , nombre = "un nombre"
          , deudores =
            [ MontoFijo 100 participante1
            ]
          , pagadores = []
          }
  it "con un item repartido entre varios participantes generamos un pago acorde" $ do
    let participante1 = participante 32
    let participante2 = participante 33
    let item1 = fakeUlid 2
    repartija2Pago (Repartija
        { repartijaNombre = "un nombre"
        , repartijaGrupoId = nullUlid
        , repartijaId = fakeUlid 1
        , repartijaExtra = 0
        , repartijaItems =
          [ RepartijaItem item1 "Birrita" 200 2
          ]
        , repartijaClaims =
          [ RepartijaClaim (fakeUlid 100) participante1 item1 (Just 1)
          , RepartijaClaim (fakeUlid 101) participante2 item1 (Just 1)
          ]
        })
      `shouldBe` Pago
          { pagoId = nullUlid
          , isValid = False
          , monto = 200
          , nombre = "un nombre"
          , deudores =
            [ MontoFijo 100 participante1
            , MontoFijo 100 participante2
            ]
          , pagadores = []
          }
  it "con algunos items repartidos generamos un pago acorde" $ do
    let participante1 = participante 32
    let participante2 = participante 33
    let birrita = fakeUlid 2
    let papitas = fakeUlid 3

    repartija2Pago (Repartija
        { repartijaNombre = "un nombre"
        , repartijaGrupoId = nullUlid
        , repartijaId = fakeUlid 1
        , repartijaExtra = 0
        , repartijaItems =
          [ RepartijaItem birrita "Birrita" 200 2
          , RepartijaItem papitas "Papitas" 300 1
          ]
        , repartijaClaims =
          [ RepartijaClaim (fakeUlid 100) participante1 birrita (Just 1)
          , RepartijaClaim (fakeUlid 101) participante2 birrita (Just 1)
          , RepartijaClaim (fakeUlid 102) participante1 papitas Nothing
          , RepartijaClaim (fakeUlid 103) participante2 papitas Nothing
          ]
        })
      `shouldBe` Pago
          { pagoId = nullUlid
          , isValid = False
          , monto = 500
          , nombre = "un nombre"
          , deudores =
            [ MontoFijo 100 participante1
            , MontoFijo 100 participante2
            , MontoFijo 150 participante1
            , MontoFijo 150 participante2
            ]
          , pagadores = []
          }
  it "con algunos items repartidos y propina generamos un pago acorde" $ do
    let participante1 = participante 32
    let participante2 = participante 33
    let birrita = fakeUlid 2
    let papitas = fakeUlid 3
    repartija2Pago (Repartija
        { repartijaNombre = "un nombre"
        , repartijaGrupoId = nullUlid
        , repartijaId = fakeUlid 1
        , repartijaExtra = 50
        , repartijaItems =
          [ RepartijaItem birrita "Birrita" 200 2
          , RepartijaItem papitas "Papitas" 300 1
          ]
        , repartijaClaims =
          [ RepartijaClaim (fakeUlid 100) participante1 birrita (Just 1)
          , RepartijaClaim (fakeUlid 101) participante2 birrita (Just 1)
          , RepartijaClaim (fakeUlid 102) participante1 papitas Nothing
          ]
        })
      `shouldBe` Pago
          { pagoId = nullUlid
          , isValid = False
          , monto = 550
          , nombre = "un nombre"
          , deudores =
            [ MontoFijo 100 participante1
            , MontoFijo 100 participante2
            , MontoFijo 300 participante1
            , MontoFijo 40 participante1
            , MontoFijo 10 participante2
            ]
          , pagadores = []
          }
  it "con algunos items repartidos cuando sobra alguno los repartimos como si fueran propina" $ do
    let participante1 = participante 32
    let participante2 = participante 33
    let birrita = fakeUlid 2
    let papitas = fakeUlid 3
    repartija2Pago (Repartija
        { repartijaNombre = "un nombre"
        , repartijaGrupoId = nullUlid
        , repartijaId = fakeUlid 1
        , repartijaExtra = 0
        , repartijaItems =
          [ RepartijaItem birrita "Birrita" 200 2
          , RepartijaItem papitas "Papitas" 300 1
          ]
        , repartijaClaims =
          [ RepartijaClaim (fakeUlid 100) participante1 birrita (Just 1)
          , RepartijaClaim (fakeUlid 101) participante2 birrita (Just 1)
          ]
        })
      `shouldBe` Pago
          { pagoId = nullUlid
          , isValid = False
          , monto = 500
          , nombre = "un nombre"
          , deudores =
            [ MontoFijo 100 participante1
            , MontoFijo 100 participante2
            , MontoFijo 150 participante1
            , MontoFijo 150 participante2
            ]
          , pagadores = []
          }
