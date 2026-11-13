module BananaSplit.CambiosDeClaimsSpec (
  spec,
) where

import Protolude
import Test.Hspec
import Test.QuickCheck

import BananaSplit
import BananaSplit.TestUtils

spec :: Spec
spec = describe "cambiosDeClaims" $ do
  it "si nada cambió no hay nada que guardar" $ do
    cambiosDeClaims [claimDeP1, claimDeP2] [claimDeP2, claimDeP1]
      `shouldBe` CambiosDeClaims{nuevosOModificados = [], borrados = []}

  it "un claim nuevo se guarda y uno que ya no está se borra" $ do
    let nuevo = RepartijaClaim nullUlid (participante 3) item1 (Just 1)
    cambiosDeClaims [claimDeP1, claimDeP2] [claimDeP1, nuevo]
      `shouldBe` CambiosDeClaims{nuevosOModificados = [nuevo], borrados = [claimDeP2.id]}

  it "un claim al que le cambió la cantidad se guarda" $ do
    let modificado = RepartijaClaim claimDeP1.id claimDeP1.participante item1 (Just 2)
    cambiosDeClaims [claimDeP1, claimDeP2] [modificado, claimDeP2]
      `shouldBe` CambiosDeClaims{nuevosOModificados = [modificado], borrados = []}

  it "unir items guarda sólo los claims de los dos items" $ do
    let otroItem = fakeUlid 3
        repartija =
          Repartija
            { id = fakeUlid 1
            , nombre = "cena"
            , extra = 0
            , distribucionDeSobras = SobrasNoDistribuir
            , items =
                [ RepartijaItem item1 "Cerveza" (mkMonto 2 1500) 3
                , RepartijaItem otroItem "Cerveza" (mkMonto 2 1000) 2
                , RepartijaItem (fakeUlid 4) "Papas" (mkMonto 2 1000) 1
                ]
            , claims =
                [ claimDeP1
                , RepartijaClaim (fakeUlid 201) (participante 1) otroItem (Just 1)
                , RepartijaClaim (fakeUlid 202) (participante 2) (fakeUlid 4) Nothing
                ]
            , noConsumieron = []
            }
        unida =
          either (panic . show) identity $
            aplicarAccionDeRepartija (fakeUlid 99) (UnirItems item1 otroItem "Unido") repartija
    cambiosDeClaims repartija.claims unida.claims
      `shouldBe` CambiosDeClaims
        { nuevosOModificados = [RepartijaClaim claimDeP1.id (participante 1) item1 (Just 2)]
        , borrados = [fakeUlid 201]
        }

  it "aplicar los cambios a los claims de antes da los de después" $
    property $
      forAll casoDeClaims $ \(antes, despues) ->
        ordenar (aplicarCambios antes (cambiosDeClaims antes despues)) === ordenar despues

  it "sólo incluye lo que cambió" $
    property $
      forAll casoDeClaims $ \(antes, despues) ->
        let cambios = cambiosDeClaims antes despues
        in conjoin
             [ counterexample "guarda uno que no cambió" $
                 all (\c -> c.id == nullUlid || c `notElem` antes) cambios.nuevosOModificados
             , counterexample "borra uno que sigue estando" $
                 all (\claimId -> claimId `notElem` fmap (.id) despues) cambios.borrados
             ]

item1 :: ULID
item1 = fakeUlid 2

claimDeP1 :: RepartijaClaim
claimDeP1 = RepartijaClaim (fakeUlid 100) (participante 1) item1 (Just 1)

claimDeP2 :: RepartijaClaim
claimDeP2 = RepartijaClaim (fakeUlid 101) (participante 2) item1 Nothing

-- | Lo que haría la base con los cambios, mirando sólo los ids: borra los
-- borrados, pisa por id los que ya estaban y agrega los nuevos.
aplicarCambios :: [RepartijaClaim] -> CambiosDeClaims -> [RepartijaClaim]
aplicarCambios antes cambios =
  let guardadosConId = cambios.nuevosOModificados & filter (\c -> c.id /= nullUlid)
      quedan =
        antes
          & filter (\c -> c.id `notElem` cambios.borrados)
          & filter (\c -> c.id `notElem` fmap (.id) guardadosConId)
  in quedan <> cambios.nuevosOModificados

ordenar :: [RepartijaClaim] -> [(ULID, ParticipanteId, ULID, Maybe Int)]
ordenar = sort . fmap (\c -> (c.id, c.participante, c.itemId, c.cantidad))

casoDeClaims :: Gen ([RepartijaClaim], [RepartijaClaim])
casoDeClaims = do
  n <- choose (0, 8)
  antes <- forM [1 .. n] $ \i -> claimConId (fakeUlid (100 + i))
  quedan <- fmap catMaybes $ forM antes $ \c ->
    oneof
      [ pure Nothing
      , pure (Just c)
      , Just . RepartijaClaim c.id c.participante c.itemId <$> cantidadDeClaim
      ]
  k <- choose (0, 3)
  nuevos <- replicateM k (claimConId nullUlid)
  despues <- shuffle (quedan <> nuevos)
  pure (antes, despues)
  where
    claimConId claimId =
      RepartijaClaim claimId
        <$> (participante <$> choose (1, 4))
        <*> (fakeUlid <$> choose (1, 3))
        <*> cantidadDeClaim
    cantidadDeClaim = oneof [pure Nothing, Just <$> choose (1, 5)]
