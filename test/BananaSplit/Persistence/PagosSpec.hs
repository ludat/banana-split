{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module BananaSplit.Persistence.PagosSpec (
  spec,
) where

import Data.Time (fromGregorian)
import Database.Beam
import Database.Beam.Postgres (Pg)
import Protolude
import Test.Hspec
import Test.QuickCheck

import BananaSplit.Core
import BananaSplit.Deudas
import BananaSplit.Moneda
import BananaSplit.Monto
import BananaSplit.Participante (Participante (..), ParticipanteId (..))
import BananaSplit.Persistence
import BananaSplit.Persistence.Schema qualified as Schema
import BananaSplit.Persistence.SpecHook
import BananaSplit.Repartija

spec :: SpecWith RunDb
spec =
  describe "pago persistance" $ do
    it "I can update a distribution multiple times and the last one is the one that counts" $ \(RunDb runDb) -> property $ \pagoOriginal d1 d2 -> do
      grupo <- runDb $ createGrupo "Test Grupo" "alguien"
      pago <- runDb $ savePago grupo.id pagoOriginal
      pago <- runDb $ updatePago grupo.id pago.pagoId pago{pagadores = pago.pagadores{tipo = d1}}
      pago <- runDb $ updatePago grupo.id pago.pagoId pago{pagadores = pago.pagadores{tipo = d2}}
      pagoWithoutIds pago `shouldBe` pagoWithoutIds pagoOriginal{pagadores = Distribucion nullUlid d2}

    it "updating a pago with fresh distribución ids doesn't leave orphans" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Test Grupo" "alguien"
      pago <- runDb $ saveInvalidRepartijaPago grupo

      runDb countOrphanedDistribuciones `shouldReturn` 0

      let nuevosPagadores =
            Distribucion nullUlid $
              TipoDistribucionPartes $
                DistribucionPartes nullUlid [Ponderado 1 (participanteDe grupo)]
          nuevosDeudores =
            Distribucion nullUlid $
              TipoDistribucionRepartija $
                Repartija nullUlid "Cena" 0 SobrasNoDistribuir [RepartijaItem nullUlid "Item" 100 1] []
      _ <- runDb $ updatePago grupo.id pago.pagoId pago{pagadores = nuevosPagadores, deudores = nuevosDeudores}

      runDb countOrphanedDistribuciones `shouldReturn` 0

    it "re-saving a distribución with the same tipo preserves its repartija claims" $ \(RunDb runDb) -> do
      let
        countAllClaims :: Pg Int
        countAllClaims =
          fmap length $ runSelectReturningList $ select $ do
            c <- all_ db.repartija_claims
            pure c
      grupo <- runDb $ createGrupo "Test Grupo" "alguien"
      pago <- runDb $ saveInvalidRepartijaPago grupo
      let repartija = repartijaDe pago

      _ <- runDb $ saveRepartijaClaim repartija.id (RepartijaClaim nullUlid (participanteDe grupo) (primerItem repartija).id Nothing)
      claimsBefore <- runDb countAllClaims
      claimsBefore `shouldBe` 1

      _ <- runDb $ updatePago grupo.id pago.pagoId pago
      claimsAfter <- runDb countAllClaims
      claimsAfter `shouldBe` 1

    it "Pago roundtrips from the db" $ \(RunDb runDb) -> property $ \pago -> do
      grupo <- runDb $ createGrupo "Test Grupo" "alguien"
      savedPago <- runDb $ savePago grupo.id pago
      fetchedPago <- runDb $ fetchPago savedPago.pagoId
      fetchedPago `shouldBe` savedPago

    it "adding a claim turns an invalid repartija pago valid, and the stored flag reflects it" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Test Grupo" "alguien"
      pago <- runDb $ saveInvalidRepartijaPago grupo
      let repartija = repartijaDe pago

      -- The items add up to the monto, but there are no claims yet, so the pago
      -- is invalid and the stored flag (read shallowly, without recomputing) is False.
      shallowBefore <- runDb $ fetchShallowPagos grupo.id
      fmap (.isValid) shallowBefore `shouldBe` [False]

      -- Claiming the whole item makes the montos add up. Saving the claim must
      -- update the stored flag on its own, since the resumen no longer recomputes
      -- validity on read.
      _ <- runDb $ saveRepartijaClaim repartija.id (RepartijaClaim nullUlid (participanteDe grupo) (primerItem repartija).id Nothing)
      shallowAfter <- runDb $ fetchShallowPagos grupo.id
      fmap (.isValid) shallowAfter `shouldBe` [True]

    it "deleting a claim turns a valid repartija pago invalid again" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Test Grupo" "alguien"
      pago <- runDb $ saveInvalidRepartijaPago grupo
      let repartija = repartijaDe pago

      claim <- runDb $ saveRepartijaClaim repartija.id (RepartijaClaim nullUlid (participanteDe grupo) (primerItem repartija).id Nothing)
      shallowValid <- runDb $ fetchShallowPagos grupo.id
      fmap (.isValid) shallowValid `shouldBe` [True]

      runDb $ deleteRepartijaClaim claim.id
      shallowInvalid <- runDb $ fetchShallowPagos grupo.id
      fmap (.isValid) shallowInvalid `shouldBe` [False]

-- | Save a pago whose deudores is a repartija with a single item that sums to
-- the monto but has no claims, leaving the pago invalid until something is
-- claimed.
saveInvalidRepartijaPago :: Grupo -> Pg Pago
saveInvalidRepartijaPago grupo =
  savePago grupo.id $
    Pago
      { pagoId = nullUlid
      , monto = 100
      , moneda = ARS
      , isValid = False
      , nombre = "Cena"
      , fecha = fromGregorian 2025 1 1
      , pagadores =
          Distribucion nullUlid $
            TipoDistribucionPartes $
              DistribucionPartes nullUlid [Ponderado 1 (participanteDe grupo)]
      , deudores =
          Distribucion nullUlid $
            TipoDistribucionRepartija $
              Repartija nullUlid "Cena" 0 SobrasNoDistribuir [RepartijaItem nullUlid "Item" 100 1] []
      }

-- | Count distribuciones not referenced by any pago. Their subtype rows
-- (partes/repartijas y sus items) son huérfanas y deberían haberse borrado.
countOrphanedDistribuciones :: Pg Int
countOrphanedDistribuciones =
  fmap length $ runSelectReturningList $ select $ do
    distribucion <- all_ db.distribuciones
    guard_ $ not_ $ exists_ $ do
      pago <- all_ db.pagos
      guard_ $
        pago.distribucion_pagadores
          `references_` distribucion
          ||. pago.distribucion_deudores
          `references_` distribucion
      pure pago
    pure distribucion.id

participanteDe :: Grupo -> ParticipanteId
participanteDe grupo = case grupo.participantes of
  (p : _) -> ParticipanteId p.id
  [] -> panic "el grupo deberia tener un participante"

repartijaDe :: Pago -> Repartija
repartijaDe pago = case pago.deudores.tipo of
  TipoDistribucionRepartija r -> r
  _ -> panic "esperaba una repartija en deudores"

primerItem :: Repartija -> RepartijaItem
primerItem repartija = case repartija.items of
  (item : _) -> item
  [] -> panic "la repartija deberia tener un item"

instance Arbitrary DistribucionDeSobras where
  arbitrary = elements [SobrasNoDistribuir, SobrasProporcional]

instance Arbitrary TipoDistribucion where
  arbitrary =
    oneof
      [ pure $ TipoDistribucionPartes $ DistribucionPartes nullUlid []
      , TipoDistribucionRepartija <$> (Repartija nullUlid "nombre" <$> arbitrary <*> arbitrary <*> pure [] <*> pure [])
      ]

instance Arbitrary Distribucion where
  arbitrary = Distribucion nullUlid <$> arbitrary

instance Arbitrary Monto where
  arbitrary = do
    Monto <$> fmap fromInteger arbitrary

instance Arbitrary Pago where
  arbitrary =
    Pago nullUlid
      <$> arbitrary
      <*> (elements [minBound .. maxBound])
      <*> pure False
      <*> pure "nombre"
      <*> pure (fromGregorian 2025 1 1)
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
        TipoDistribucionPartes d -> TipoDistribucionPartes (distribucionPartesWithoutIds d)
        TipoDistribucionRepartija r -> TipoDistribucionRepartija (repartijaWithoutIds r)
    }

distribucionPartesWithoutIds :: DistribucionPartes -> DistribucionPartes
distribucionPartesWithoutIds d =
  d{id = nullUlid}

repartijaWithoutIds :: Repartija -> Repartija
repartijaWithoutIds r =
  r
    { id = nullUlid
    , items = fmap repartijaItemWithoutIds r.items
    , claims = fmap repartijaClaimWithoutIds r.claims
    }

repartijaItemWithoutIds :: RepartijaItem -> RepartijaItem
repartijaItemWithoutIds item =
  item{id = nullUlid}

repartijaClaimWithoutIds :: RepartijaClaim -> RepartijaClaim
repartijaClaimWithoutIds claim =
  claim{id = nullUlid}
