{-# OPTIONS_GHC -Wno-orphans #-}
module BananaSplit.Persistence.PagosSpec
    ( spec
    ) where

import BananaSplit.Core
import BananaSplit.Deudas
import BananaSplit.Monto
import BananaSplit.Persistence
import BananaSplit.Persistence.SpecHook
import BananaSplit.Repartija

import Protolude

import Test.Hspec
import Test.QuickCheck

spec :: SpecWith RunDb
spec =
  describe "pago persistance" $ do
    it "I can update a distribution multiple times and the last one is the one that counts" $ \(RunDb runDb) -> property $ \pagoOriginal d1 d2 -> do
      grupo <- runDb $ createGrupo "Test Grupo" "alguien"
      pago <- runDb $ savePago grupo.id pagoOriginal
      pago <- runDb $ updatePago grupo.id pago.pagoId pago {pagadores = pago.pagadores { tipo = d1 }}
      pago <- runDb $ updatePago grupo.id pago.pagoId pago {pagadores = pago.pagadores { tipo = d2 }}
      pagoWithoutIds pago `shouldBe` pagoWithoutIds pagoOriginal {pagadores = Distribucion nullUlid d2}

    it "Pago roundtrips from the db" $ \(RunDb runDb) -> property $ \pago -> do
      grupo <- runDb $ createGrupo "Test Grupo" "alguien"
      savedPago <- runDb $ savePago grupo.id pago
      fetchedPago <- runDb $ fetchPago grupo.id savedPago.pagoId
      fetchedPago `shouldBe` savedPago

instance Arbitrary TipoDistribucion where
  arbitrary =
    oneof
      [ pure $ TipoDistribucionMontoEquitativo $ DistribucionMontoEquitativo nullUlid []
      , pure $ TipoDistribucionMontosEspecificos $ DistribucionMontosEspecificos nullUlid []
      , TipoDistribucionRepartija <$> (Repartija nullUlid "nombre" <$> arbitrary <*> pure [] <*> pure [])
      ]

instance Arbitrary Distribucion where
  arbitrary = Distribucion nullUlid <$> arbitrary

instance Arbitrary Monto where
  arbitrary = do
    Monto <$> fmap fromInteger arbitrary

instance Arbitrary Pago where
  arbitrary = Pago nullUlid
    <$> arbitrary
    <*> pure False
    <*> pure "nombre"
    <*> arbitrary
    <*> arbitrary

pagoWithoutIds :: Pago -> Pago
pagoWithoutIds pago =
  pago
    { pagoId = nullUlid
    , pagadores = distribucionWithoutIds pago.pagadores
    , deudores = distribucionWithoutIds pago.deudores
    }

distribucionWithoutIds :: Distribucion -> Distribucion
distribucionWithoutIds distribucion =
  distribucion
    { id = nullUlid
    , tipo = case distribucion.tipo of
        TipoDistribucionMontoEquitativo d -> TipoDistribucionMontoEquitativo (distribucionMontoEquitativoWithoutIds d)
        TipoDistribucionMontosEspecificos d -> TipoDistribucionMontosEspecificos (distribucionMontosEspecificosWithoutIds d)
        TipoDistribucionRepartija r -> TipoDistribucionRepartija (repartijaWithoutIds r)
    }

distribucionMontoEquitativoWithoutIds :: DistribucionMontoEquitativo -> DistribucionMontoEquitativo
distribucionMontoEquitativoWithoutIds d =
  d { id = nullUlid }

distribucionMontosEspecificosWithoutIds :: DistribucionMontosEspecificos -> DistribucionMontosEspecificos
distribucionMontosEspecificosWithoutIds d =
  d
    { id = nullUlid
    , montos = fmap montoEspecificoWithoutIds d.montos
    }

montoEspecificoWithoutIds :: MontoEspecifico -> MontoEspecifico
montoEspecificoWithoutIds m =
  m { id = nullUlid }

repartijaWithoutIds :: Repartija -> Repartija
repartijaWithoutIds r =
  r
    { id = nullUlid
    , items = fmap repartijaItemWithoutIds r.items
    , claims = fmap repartijaClaimWithoutIds r.claims
    }

repartijaItemWithoutIds :: RepartijaItem -> RepartijaItem
repartijaItemWithoutIds item =
  item { id = nullUlid }

repartijaClaimWithoutIds :: RepartijaClaim -> RepartijaClaim
repartijaClaimWithoutIds claim =
  claim { id = nullUlid }
