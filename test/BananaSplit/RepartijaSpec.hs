module BananaSplit.RepartijaSpec where

import Protolude
import Test.Hspec

import BananaSplit
import BananaSplit.TestUtils

fakeRepartija :: Repartija
fakeRepartija =
  Repartija
    { id = fakeUlid 1
    , nombre = "falso"
    , extra = 0
    , distribucionDeSobras = SobrasProporcional
    , claims = []
    , items = []
    }

spec :: Spec
spec = describe "validar repartija" $ do
  it "una repartija vacia genera un pago vacio" $ do
    calcularNetosRepartija fakeRepartija
      `shouldBe` netos []
  it "con un items repartidos generamos un pago acorde" $ do
    let participante1 = participante 32
    let item1 = fakeUlid 2
    calcularNetosRepartija
      ( fakeRepartija
          { extra = 0
          , items =
              [ RepartijaItem item1 "Birrita" 100 1
              ]
          , claims =
              [ RepartijaClaim (fakeUlid 100) participante1 item1 (Just 1)
              ]
          }
      )
      `shouldBe` netos
        [ (participante1, 100)
        ]
  it "con un item repartido entre varios participantes generamos un pago acorde" $ do
    let participante1 = participante 32
    let participante2 = participante 33
    let item1 = fakeUlid 2
    calcularNetosRepartija
      ( fakeRepartija
          { extra = 0
          , items =
              [ RepartijaItem item1 "Birrita" 200 2
              ]
          , claims =
              [ RepartijaClaim (fakeUlid 100) participante1 item1 (Just 1)
              , RepartijaClaim (fakeUlid 101) participante2 item1 (Just 1)
              ]
          }
      )
      `shouldBe` netos
        [ (participante1, 100)
        , (participante2, 100)
        ]
  it "con algunos items repartidos generamos un pago acorde" $ do
    let participante1 = participante 32
    let participante2 = participante 33
    let birrita = fakeUlid 2
    let papitas = fakeUlid 3

    calcularNetosRepartija
      ( fakeRepartija
          { extra = 0
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
          }
      )
      `shouldBe` netos
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
    calcularNetosRepartija
      ( fakeRepartija
          { extra = 50
          , items =
              [ RepartijaItem birrita "Birrita" 200 2
              , RepartijaItem papitas "Papitas" 300 1
              ]
          , claims =
              [ RepartijaClaim (fakeUlid 100) participante1 birrita (Just 1)
              , RepartijaClaim (fakeUlid 101) participante2 birrita (Just 1)
              , RepartijaClaim (fakeUlid 102) participante1 papitas Nothing
              ]
          }
      )
      `shouldBe` netos
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
    calcularNetosRepartija
      ( fakeRepartija
          { extra = 0
          , items =
              [ RepartijaItem birrita "Birrita" 200 2
              , RepartijaItem papitas "Papitas" 300 1
              ]
          , claims =
              [ RepartijaClaim (fakeUlid 100) participante1 birrita (Just 1)
              , RepartijaClaim (fakeUlid 101) participante2 birrita (Just 1)
              ]
          }
      )
      `shouldBe` netos
        [ (participante1, 100)
        , (participante2, 100)
        , (participante1, 150)
        , (participante2, 150)
        ]
  it "para un item con cantidad 1, reclamar 1 y 1 es equivalente a reclamar participacion" $ do
    let participante1 = participante 32
    let participante2 = participante 33
    let item1 = fakeUlid 2
    let repartijaConCantidades =
          fakeRepartija
            { extra = 0
            , items = [RepartijaItem item1 "Item" 100 1]
            , claims =
                [ RepartijaClaim (fakeUlid 100) participante1 item1 (Just 1)
                , RepartijaClaim (fakeUlid 101) participante2 item1 (Just 1)
                ]
            }
    let repartijaConParticipacion =
          fakeRepartija
            { extra = 0
            , items = [RepartijaItem item1 "Item" 100 1]
            , claims =
                [ RepartijaClaim (fakeUlid 100) participante1 item1 Nothing
                , RepartijaClaim (fakeUlid 101) participante2 item1 Nothing
                ]
            }
    calcularNetosRepartija repartijaConCantidades
      `shouldBe` calcularNetosRepartija repartijaConParticipacion
  describe "estado de cada item" $ do
    let participante1 = participante 32
    let participante2 = participante 33
    let birrita = fakeUlid 2
    let papitas = fakeUlid 3
    let claim n p item cantidad = RepartijaClaim (fakeUlid n) p item cantidad
    it "reclamar exactamente las unidades que hay lo deja repartido" $ do
      revisar
        fakeRepartija
          { items = [RepartijaItem birrita "Birrita" 200 2]
          , claims = [claim 100 participante1 birrita (Just 1), claim 101 participante2 birrita (Just 1)]
          }
        `shouldBe` ([(EstadoRepartidoPorCantidad UnidadesExactas, ItemResuelto)], True)
    it "en partes iguales entre dos o más lo deja repartido" $ do
      revisar
        fakeRepartija
          { items = [RepartijaItem birrita "Birrita" 200 2]
          , claims = [claim 100 participante1 birrita Nothing, claim 101 participante2 birrita Nothing]
          }
        `shouldBe` ([(EstadoRepartidoEquitativamente 2, ItemResuelto)], True)
    it "reclamar de más se reparte igual, con advertencia" $ do
      revisar
        fakeRepartija
          { items = [RepartijaItem birrita "Birrita" 200 2]
          , claims = [claim 100 participante1 birrita (Just 2), claim 101 participante2 birrita (Just 1)]
          }
        `shouldBe` ([(EstadoRepartidoPorCantidad (SobranUnidades 1), ItemConAdvertencia)], True)
    it "en partes iguales con una sola persona se reparte igual, con advertencia" $ do
      revisar
        fakeRepartija
          { items = [RepartijaItem birrita "Birrita" 200 2]
          , claims = [claim 100 participante1 birrita Nothing]
          }
        `shouldBe` ([(EstadoRepartidoEquitativamente 1, ItemConAdvertencia)], True)
    it "las unidades sin reclamar se reparten si las sobras se distribuyen" $ do
      revisar
        fakeRepartija
          { distribucionDeSobras = SobrasProporcional
          , items = [RepartijaItem birrita "Birrita" 200 2, RepartijaItem papitas "Papitas" 300 1]
          , claims = [claim 100 participante1 birrita (Just 1)]
          }
        `shouldBe` ([(EstadoRepartidoPorCantidad (FaltanUnidades 1), ItemConAdvertencia), (EstadoSinReclamar, ItemConAdvertencia)], True)
    it "las unidades sin reclamar no se resuelven si las sobras no se distribuyen" $ do
      revisar
        fakeRepartija
          { distribucionDeSobras = SobrasNoDistribuir
          , items = [RepartijaItem birrita "Birrita" 200 2, RepartijaItem papitas "Papitas" 300 1]
          , claims = [claim 100 participante1 birrita (Just 1)]
          }
        `shouldBe` ([(EstadoRepartidoPorCantidad (FaltanUnidades 1), ItemNoResuelto), (EstadoSinReclamar, ItemNoResuelto)], False)
    it "si nadie reclamó nada, las sobras no tienen a quién ir" $ do
      revisar
        fakeRepartija
          { distribucionDeSobras = SobrasProporcional
          , items = [RepartijaItem birrita "Birrita" 200 2]
          }
        `shouldBe` ([(EstadoSinReclamar, ItemNoResuelto)], False)
    it "reclamar por cantidad y en partes iguales a la vez no se resuelve" $ do
      revisar
        fakeRepartija
          { distribucionDeSobras = SobrasProporcional
          , items = [RepartijaItem birrita "Birrita" 200 2, RepartijaItem papitas "Papitas" 300 1]
          , claims =
              [ claim 100 participante1 birrita (Just 1)
              , claim 101 participante2 birrita Nothing
              , claim 102 participante1 papitas (Just 1)
              ]
          }
        `shouldBe` ([(EstadoRepartoIncorrecto, ItemNoResuelto), (EstadoRepartidoPorCantidad UnidadesExactas, ItemResuelto)], False)

-- | El estado y la resolución de cada item, y si la repartija es válida. Que
-- haya algún 'ItemNoResuelto' tiene que coincidir con que no lo sea.
revisar :: Repartija -> ([(EstadoDeItem, ResolucionDeItem)], Bool)
revisar repartija =
  ( (detalleRepartija repartija).porItem
      & fmap (\detalle -> (detalle.estado, detalle.resolucion))
  , null (getResumen (sum (fmap (.monto) repartija.items) + repartija.extra) repartija).errores
  )
