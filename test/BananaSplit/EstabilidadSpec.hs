module BananaSplit.EstabilidadSpec (
  spec,
) where

import Data.List qualified as List
import Data.Time (fromGregorian)
import Protolude
import Test.Hspec
import Test.QuickCheck

import BananaSplit
import BananaSplit.TestUtils

spec :: Spec
spec = describe "Estabilidad de los netos de un gasto" $ do
  it "no dependen del orden de las partes, los items, los claims ni los que no consumieron" $
    property $ forAll genPago $ \pago ->
      forAll (reordenar pago) $ \reordenado ->
        lasCuentasDe reordenado === lasCuentasDe pago

  it "no dependen de los ids de los claims, los items ni las distribuciones" $
    property $ forAll genPago $ \pago ->
      lasCuentasDe (renumerar pago) === lasCuentasDe pago

  xit "no dependen de la escala con la que vienen los montos" $
    let gastoDe monto =
          Pago
            { pagoId = fakeUlid 100
            , monto = monto
            , moneda = ARS
            , nombre = "Pago"
            , fecha = fromGregorian 2025 1 1
            , pagadores = distribucionMontoEquitativo [participante 1]
            , deudores = distribucionMontoEquitativo [participante 1, participante 2, participante 3]
            }
    in calcularNetosPago (gastoDe 100) `shouldBe` calcularNetosPago (gastoDe (mkMonto 2 10000))

  it "el generador produce gastos válidos y repartijas" $
    property $ forAll genPago $ \pago ->
      let resumen = getResumenGasto pago
      in cover 30 (gastoEsValido pago) "válido"
           $ cover 20 (isJust resumen.participantesEnRepartija) "repartija"
           $ cover 10 (isJust resumen.participantesEnRepartija && gastoEsValido pago) "repartija válida"
           $ property True

-- | Lo que sale de un gasto para el resto del sistema. Se compara también el
-- 'show' porque el 'Eq' de 'Decimal' iguala 10 con 10.00 y esa diferencia
-- sí llega al JSON.
lasCuentasDe :: Pago -> (Netos Monto, Netos Monto, Bool, Maybe [ParticipanteId], [ParticipanteId], Text)
lasCuentasDe pago =
  let resumen = getResumenGasto pago
  in ( resumen.pagado
     , resumen.consumido
     , null resumen.errores
     , resumen.participantesEnRepartija
     , resumen.noConsumieronEnRepartija
     , show (resumen.pagado, resumen.consumido, calcularNetosPago pago)
     )

genParticipantes :: Gen [ParticipanteId]
genParticipantes = do
  n <- choose (1, 6)
  ids <- vectorOf n (choose (1, 2 ^ (80 :: Int)))
  pure $ fmap participante (List.nub ids)

genMonto :: Integer -> Integer -> Gen Monto
genMonto desde hasta = mkMonto 2 <$> choose (desde, hasta)

genPago :: Gen Pago
genPago = do
  ps <- genParticipantes
  -- Con un total al azar casi nada cierra: que casi siempre alcance para los
  -- montos fijos, o coincida con la suma de los items de la repartija.
  (montoQueCierra, deudores) <-
    oneof
      [ (\partes -> (sumaDeFijos partes + 100000, distribucionPartes partes)) <$> genPartes ps
      , (\r -> (totalDe r, distribucionRepartija r)) <$> genRepartija ps
      ]
  monto <- frequency [(4, pure montoQueCierra), (1, genMonto 1 300000)]
  pagadores <-
    frequency
      [ (3, distribucionPartes <$> listOf1 (Ponderado <$> choose (1, 5) <*> elements ps))
      , (1, distribucionPartes <$> genPartes ps)
      ]
  pure
    Pago
      { pagoId = fakeUlid 100
      , monto = monto
      , moneda = ARS
      , nombre = "Pago"
      , fecha = fromGregorian 2025 1 1
      , pagadores = pagadores
      , deudores = deudores
      }
  where
    sumaDeFijos partes =
      partes
        & fmap
          ( \case
              MontoFijo m _ -> m
              PonderadoYMontoFijo m _ _ -> m
              Ponderado _ _ -> 0
          )
        & sum
    totalDe r = sum (fmap (.monto) r.items) + r.extra

genPartes :: [ParticipanteId] -> Gen [Parte]
genPartes ps =
  listOf1
    $ oneof
      [ MontoFijo <$> genMonto 0 5000 <*> elements ps
      , Ponderado <$> choose (0, 5) <*> elements ps
      , PonderadoYMontoFijo <$> genMonto 0 5000 <*> choose (0, 5) <*> elements ps
      ]

genRepartija :: [ParticipanteId] -> Gen Repartija
genRepartija ps = do
  cantidadDeItems <- choose (1, 5)
  items <- for [1 .. cantidadDeItems] $ \i ->
    RepartijaItem (fakeUlid (1000 + i)) ("item " <> show i) <$> genMonto 1 50000 <*> choose (1, 4)
  claimsPorItem <- for items $ \item -> do
    quienes <- sublistOf ps
    forma <- frequency [(4, pure porCantidad), (4, pure equitativo), (1, pure mixto)]
    for quienes $ \p -> RepartijaClaim nullUlid p item.id <$> forma item
  extra <- frequency [(2, pure 0), (1, genMonto 1 10000)]
  sobras <- elements [SobrasNoDistribuir, SobrasProporcional]
  noConsumieron <- sublistOf ps
  let claims =
        concat claimsPorItem
          & zipWith (\i c -> RepartijaClaim (fakeUlid (2000 + i)) c.participante c.itemId c.cantidad) [1 ..]
  pure
    Repartija
      { id = fakeUlid 50
      , nombre = "repartija"
      , extra = extra
      , distribucionDeSobras = sobras
      , items = items
      , claims = claims
      , noConsumieron = noConsumieron
      }
  where
    porCantidad item = Just <$> choose (1, item.cantidad)
    equitativo _ = pure Nothing
    mixto item = oneof [porCantidad item, equitativo item]

reordenar :: Pago -> Gen Pago
reordenar pago = do
  pagadores <- reordenarDistribucion pago.pagadores
  deudores <- reordenarDistribucion pago.deudores
  pure pago{pagadores = pagadores, deudores = deudores}
  where
    reordenarDistribucion d =
      Distribucion d.id <$> case d.tipo of
        TipoDistribucionPartes dp -> do
          partes <- shuffle dp.partes
          pure $ TipoDistribucionPartes dp{partes = partes}
        TipoDistribucionRepartija r -> do
          items <- shuffle r.items
          claims <- shuffle r.claims
          noConsumieron <- shuffle r.noConsumieron
          pure $ TipoDistribucionRepartija r{items = items, claims = claims, noConsumieron = noConsumieron}

-- | Los mismos datos con otros ids en todo lo que no es un participante: el
-- reparto no debería mirarlos más que para saber qué claim es de qué item.
renumerar :: Pago -> Pago
renumerar pago =
  pago
    { pagoId = fakeUlid 9100
    , pagadores = renumerarDistribucion pago.pagadores
    , deudores = renumerarDistribucion pago.deudores
    }
  where
    otroId u = fakeUlid (ulidToInteger u * 7 + 3)
    renumerarDistribucion d =
      Distribucion (otroId d.id) $ case d.tipo of
        TipoDistribucionPartes dp -> TipoDistribucionPartes (DistribucionPartes (otroId dp.id) dp.partes)
        TipoDistribucionRepartija (r :: Repartija) ->
          TipoDistribucionRepartija
            r
              { id = otroId r.id
              , items = fmap (\i -> RepartijaItem (otroId i.id) i.nombre i.monto i.cantidad) r.items
              , claims = fmap (\c -> RepartijaClaim (otroId c.id) c.participante (otroId c.itemId) c.cantidad) r.claims
              }
