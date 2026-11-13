module BananaSplit.AccionDeRepartijaSpec where

import Data.Aeson qualified as Aeson
import Protolude
import Test.Hspec

import BananaSplit
import BananaSplit.TestUtils

itemId :: ULID
itemId = fakeUlid 2

otroItemId :: ULID
otroItemId = fakeUlid 3

nuevoItemId :: ULID
nuevoItemId = fakeUlid 99

-- | 3 cervezas a $5, unas papas de $10 y $15 de propina: $40 en total.
repartija :: Repartija
repartija =
  Repartija
    { id = fakeUlid 1
    , nombre = "cena"
    , extra = mkMonto 2 1500
    , distribucionDeSobras = SobrasNoDistribuir
    , items =
        [ RepartijaItem itemId "Cerveza" (mkMonto 2 1500) 3
        , RepartijaItem otroItemId "Papas" (mkMonto 2 1000) 1
        ]
    , claims = [RepartijaClaim (fakeUlid 100) (participante 1) itemId (Just 3)]
    , noConsumieron = []
    }

aplicar :: AccionDeRepartija -> Repartija -> Either AccionDeRepartijaInvalida Repartija
aplicar = aplicarAccionDeRepartija nuevoItemId

aplicarBien :: AccionDeRepartija -> Repartija -> Repartija
aplicarBien accion r = either (panic . show) identity (aplicar accion r)

total :: Repartija -> Monto
total r = sum (fmap (.monto) r.items) + r.extra

todasLasAcciones :: [AccionDeRepartija]
todasLasAcciones =
  [ AgregarUnidades itemId 1
  , AgregarUnidades itemId 5
  , QuitarUnidades itemId 2
  , SepararUnidades itemId 1 "Cerveza"
  , RepartirProporcionalmente itemId
  , UnirItems itemId otroItemId "Cerveza"
  , AgregarItem "Postre" (mkMonto 2 2000) 2
  ]

spec :: Spec
spec = describe "aplicarAccionDeRepartija" $ do
  it "ninguna acción cambia lo que suma la repartija" $ do
    for_ todasLasAcciones $ \accion ->
      fmap total (aplicar accion repartija) `shouldBe` Right (total repartija)

  it "agregar una unidad mantiene el precio por unidad y lo saca de la propina" $ do
    let resultado = aplicarBien (AgregarUnidades itemId 1) repartija
    resultado.items
      `shouldBe` [ RepartijaItem itemId "Cerveza" (mkMonto 2 2000) 4
                 , RepartijaItem otroItemId "Papas" (mkMonto 2 1000) 1
                 ]
    resultado.extra `shouldBe` mkMonto 2 1000

  it "agregar más de lo que hay de propina la deja negativa" $ do
    (aplicarBien (AgregarUnidades itemId 4) repartija).extra `shouldBe` mkMonto 2 -500
    (aplicarBien (AgregarUnidades itemId 1) repartija{extra = 0}).extra `shouldBe` mkMonto 2 -500

  it "quitar unidades mantiene el precio por unidad y lo suma a la propina" $ do
    let resultado = aplicarBien (QuitarUnidades itemId 2) repartija
    resultado.items
      `shouldBe` [ RepartijaItem itemId "Cerveza" (mkMonto 2 500) 1
                 , RepartijaItem otroItemId "Papas" (mkMonto 2 1000) 1
                 ]
    resultado.extra `shouldBe` mkMonto 2 2500

  it "no deja quitar todas las unidades" $ do
    aplicar (QuitarUnidades itemId 3) repartija `shouldBe` Left CantidadDeUnidadesInvalida

  it "no acepta cantidades nulas o negativas" $ do
    aplicar (AgregarUnidades itemId 0) repartija `shouldBe` Left CantidadDeUnidadesInvalida
    aplicar (QuitarUnidades itemId -1) repartija `shouldBe` Left CantidadDeUnidadesInvalida

  it "separar pasa unidades a un item nuevo sin tocar la propina" $ do
    let resultado = aplicarBien (SepararUnidades itemId 1 "Cerveza de Juan") repartija
    resultado.items
      `shouldBe` [ RepartijaItem itemId "Cerveza" (mkMonto 2 1000) 2
                 , RepartijaItem nuevoItemId "Cerveza de Juan" (mkMonto 2 500) 1
                 , RepartijaItem otroItemId "Papas" (mkMonto 2 1000) 1
                 ]
    resultado.extra `shouldBe` repartija.extra

  it "no deja separar sin nombre" $ do
    aplicar (SepararUnidades itemId 1 "  ") repartija `shouldBe` Left ItemSinNombre

  it "al separar el redondeo no cambia el total" $ do
    -- Diez pesos en 3 unidades: lo que queda se redondea y lo separado es la
    -- diferencia.
    let conTercios = repartija{items = [RepartijaItem itemId "Pizza" (mkMonto 2 1000) 3]}
        resultado = aplicarBien (SepararUnidades itemId 1 "Pizza") conTercios
    fmap (.monto) resultado.items `shouldBe` [mkMonto 2 667, mkMonto 2 333]

  it "los claims del item no se tocan al separar" $ do
    (aplicarBien (SepararUnidades itemId 1 "Cerveza") repartija).claims `shouldBe` repartija.claims

  it "repartir proporcionalmente saca el item y sus claims y suma su monto a la propina" $ do
    let resultado = aplicarBien (RepartirProporcionalmente itemId) repartija
    resultado.items `shouldBe` [RepartijaItem otroItemId "Papas" (mkMonto 2 1000) 1]
    resultado.extra `shouldBe` mkMonto 2 3000
    resultado.claims `shouldBe` []

  it "no deja repartir proporcionalmente el último item" $ do
    let conUnItem = repartija{items = [RepartijaItem itemId "Cerveza" (mkMonto 2 2500) 3]}
    aplicar (RepartirProporcionalmente itemId) conUnItem `shouldBe` Left NoPuedeQuedarSinItems

  describe "agregar un item" $ do
    it "lo agrega al final, sin reclamos, y lo saca de la propina" $ do
      let resultado = aplicarBien (AgregarItem " Postre " (mkMonto 2 2000) 2) repartija
      resultado.items
        `shouldBe` repartija.items <> [RepartijaItem nuevoItemId "Postre" (mkMonto 2 2000) 2]
      resultado.extra `shouldBe` mkMonto 2 -500
      resultado.claims `shouldBe` repartija.claims

    it "no acepta un item sin nombre" $ do
      aplicar (AgregarItem "  " (mkMonto 2 100) 1) repartija `shouldBe` Left ItemSinNombre

    it "no acepta cantidades nulas o negativas" $ do
      aplicar (AgregarItem "Postre" (mkMonto 2 100) 0) repartija
        `shouldBe` Left CantidadDeUnidadesInvalida

  describe "unir items" $ do
    let mismoPrecio =
          repartija
            { items =
                [ RepartijaItem itemId "Cerveza" (mkMonto 2 1500) 3
                , RepartijaItem otroItemId "Cerveza" (mkMonto 2 1000) 2
                ]
            }
        conClaims claims = mismoPrecio{claims = claims}
        unir = aplicarBien (UnirItems itemId otroItemId "Cerveza")
        sinIds = fmap (\c -> (c.participante, c.itemId, c.cantidad))

    it "suma unidades y montos" $ do
      (unir repartija).items `shouldBe` [RepartijaItem itemId "Cerveza" (mkMonto 2 2500) 4]

    it "queda el primero, aunque esté después en la lista" $ do
      (aplicarBien (UnirItems otroItemId itemId "Papas") repartija).items
        `shouldBe` [RepartijaItem otroItemId "Papas" (mkMonto 2 2500) 4]

    it "usa el nombre que se le pase" $ do
      fmap (.nombre) (aplicarBien (UnirItems itemId otroItemId " Cena ") repartija).items
        `shouldBe` ["Cena"]

    it "no deja unir sin nombre" $ do
      aplicar (UnirItems itemId otroItemId "  ") repartija `shouldBe` Left ItemSinNombre

    it "con claims por cantidad suma las unidades de cada uno" $ do
      let resultado =
            unir $
              conClaims
                [ RepartijaClaim (fakeUlid 100) (participante 1) itemId (Just 2)
                , RepartijaClaim (fakeUlid 101) (participante 2) itemId (Just 1)
                , RepartijaClaim (fakeUlid 102) (participante 1) otroItemId (Just 1)
                ]
      sinIds resultado.claims
        `shouldBe` [(participante 1, itemId, Just 3), (participante 2, itemId, Just 1)]
      -- El claim que ya estaba en el item que queda conserva su id.
      fmap (.id) resultado.claims `shouldBe` [fakeUlid 100, fakeUlid 101]

    it "con claims en partes iguales participan todos los de los dos" $ do
      let resultado =
            unir $
              conClaims
                [ RepartijaClaim (fakeUlid 100) (participante 1) itemId Nothing
                , RepartijaClaim (fakeUlid 101) (participante 2) otroItemId Nothing
                , RepartijaClaim (fakeUlid 102) (participante 1) otroItemId Nothing
                ]
      sinIds resultado.claims
        `shouldBe` [(participante 1, itemId, Nothing), (participante 2, itemId, Nothing)]

    it "con claims de las dos formas el item queda sin reclamar" $ do
      let resultado =
            unir $
              conClaims
                [ RepartijaClaim (fakeUlid 100) (participante 1) itemId (Just 1)
                , RepartijaClaim (fakeUlid 101) (participante 2) otroItemId Nothing
                ]
      resultado.claims `shouldBe` []

    describe "con precios unitarios distintos" $ do
      let conClaimsDistintos claims = repartija{claims = claims}

      it "los claims por cantidad pasan a ser en partes iguales" $ do
        let resultado =
              unir $
                conClaimsDistintos
                  [ RepartijaClaim (fakeUlid 100) (participante 1) itemId (Just 3)
                  , RepartijaClaim (fakeUlid 101) (participante 2) otroItemId (Just 1)
                  ]
        sinIds resultado.claims
          `shouldBe` [(participante 1, itemId, Nothing), (participante 2, itemId, Nothing)]

      it "aunque estén mezclados, participan todos en partes iguales" $ do
        let resultado =
              unir $
                conClaimsDistintos
                  [ RepartijaClaim (fakeUlid 100) (participante 1) itemId (Just 1)
                  , RepartijaClaim (fakeUlid 101) (participante 2) otroItemId Nothing
                  ]
        sinIds resultado.claims
          `shouldBe` [(participante 1, itemId, Nothing), (participante 2, itemId, Nothing)]

    it "compara el precio unitario sin redondear" $ do
      -- \$10 en 3 y $20 en 6 valen lo mismo por unidad aunque no sea exacto.
      let tercios =
            repartija
              { items =
                  [ RepartijaItem itemId "Pizza" (mkMonto 2 1000) 3
                  , RepartijaItem otroItemId "Pizza" (mkMonto 2 2000) 6
                  ]
              , claims =
                  [ RepartijaClaim (fakeUlid 100) (participante 1) itemId (Just 1)
                  , RepartijaClaim (fakeUlid 101) (participante 2) otroItemId (Just 2)
                  ]
              }
      sinIds (unir tercios).claims
        `shouldBe` [(participante 1, itemId, Just 1), (participante 2, itemId, Just 2)]

    it "no deja unir un item consigo mismo" $ do
      aplicar (UnirItems itemId itemId "") repartija `shouldBe` Left UnirConElMismoItem

    it "rechaza si el otro item no es de la repartija" $ do
      aplicar (UnirItems itemId (fakeUlid 77) "") repartija `shouldBe` Left ItemNoEncontrado

  it "entiende lo que manda el frontend para una acción con un solo argumento" $ do
    -- Es lo que arma `jsonEncAccionDeRepartija` en `Generated/Api.elm`: un
    -- argumento solo va sin lista.
    Aeson.decode ("{\"RepartirProporcionalmente\":" <> Aeson.encode itemId <> "}")
      `shouldBe` Just (RepartirProporcionalmente itemId)

  it "rechaza items que no son de la repartija" $ do
    aplicar (AgregarUnidades (fakeUlid 77) 1) repartija `shouldBe` Left ItemNoEncontrado
