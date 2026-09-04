{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module BananaSplit.Persistence.PagosSpec (
  spec,
) where

import Data.Aeson qualified as Aeson
import Data.Time (fromGregorian)
import Database.Beam
import Database.Beam.Postgres (Pg, PgJSONB (..))
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
import BananaSplit.TestUtils (netos)
import BananaSplit.ULID (ULID)

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
      fmap esValido shallowBefore `shouldBe` [False]

      -- Claiming the whole item makes the montos add up. Saving the claim must
      -- update the stored flag on its own, since the resumen no longer recomputes
      -- validity on read.
      _ <- runDb $ saveRepartijaClaim repartija.id (RepartijaClaim nullUlid (participanteDe grupo) (primerItem repartija).id Nothing)
      shallowAfter <- runDb $ fetchShallowPagos grupo.id
      fmap esValido shallowAfter `shouldBe` [True]

    it "deleting a claim turns a valid repartija pago invalid again" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Test Grupo" "alguien"
      pago <- runDb $ saveInvalidRepartijaPago grupo
      let repartija = repartijaDe pago

      claim <- runDb $ saveRepartijaClaim repartija.id (RepartijaClaim nullUlid (participanteDe grupo) (primerItem repartija).id Nothing)
      shallowValid <- runDb $ fetchShallowPagos grupo.id
      fmap esValido shallowValid `shouldBe` [True]

      runDb $ deleteRepartijaClaim claim.id
      shallowInvalid <- runDb $ fetchShallowPagos grupo.id
      fmap esValido shallowInvalid `shouldBe` [False]

    it "netosDeGrupo suma lo mismo que recalcular todos los gastos" $ \(RunDb runDb) -> do
      (grupo, uno, otro) <- runDb grupoConDosParticipantes

      _ <- runDb $ savePago grupo.id $ gastoEntre ARS 100 uno otro
      _ <- runDb $ savePago grupo.id $ gastoEntre USD 50 otro uno
      -- Un gasto inválido no tiene que aportar nada.
      _ <- runDb $ savePago grupo.id $ (gastoEntre ARS 70 uno otro){deudores = distribucionVacia}

      desdeElCache <- runDb $ netosDeGrupo grupo.id
      desdeElCache
        `shouldBe` netos [(uno, 100), (otro, -100)]
        `enMoneda` ARS
        <> netos [(otro, 50), (uno, -50)]
        `enMoneda` USD

      -- Y tiene que coincidir con recalcular todo desde las distribuciones,
      -- que es lo que hacía el loop de fetchPago que este cache reemplaza.
      recalculado <- runDb $ netosRecalculados grupo
      desdeElCache `shouldBe` recalculado

    it "un gasto invalido no deja filas en el cache" $ \(RunDb runDb) -> do
      (grupo, uno, otro) <- runDb grupoConDosParticipantes
      pago <- runDb $ savePago grupo.id $ gastoEntre ARS 100 uno otro
      runDb (contarNetosDe pago.pagoId) `shouldReturn` 2

      _ <- runDb $ updatePago grupo.id pago.pagoId pago{deudores = distribucionVacia}
      runDb (contarNetosDe pago.pagoId) `shouldReturn` 0

    it "borrar un gasto se lleva sus filas del cache" $ \(RunDb runDb) -> do
      (grupo, uno, otro) <- runDb grupoConDosParticipantes
      pago <- runDb $ savePago grupo.id $ gastoEntre ARS 100 uno otro
      runDb (contarNetosDe pago.pagoId) `shouldReturn` 2

      runDb $ deletePago pago.pagoId
      runDb (contarNetosDe pago.pagoId) `shouldReturn` 0

    -- Los dos casos de cache frío: nunca calculado, y calculado con un formato
    -- que ya no decodifica. Este último es el que deja cambiar la forma de
    -- ErrorResumen sin migrar los blobs viejos.
    it "la lectura repara un cache sin calcular" $ \(RunDb runDb) -> do
      (grupo, uno, otro) <- runDb grupoConDosParticipantes
      pago <- runDb $ savePago grupo.id $ gastoEntre ARS 100 uno otro

      runDb $ enfriarCache pago.pagoId Nothing
      -- La suma sola no lo puede ver: el gasto no tiene filas y queda afuera.
      runDb (netosDeGrupo grupo.id) `shouldReturn` mempty

      -- Leer los gastos lo detecta y lo recalcula en el momento.
      reparado <- runDb $ fetchShallowPagos grupo.id
      fmap esValido reparado `shouldBe` [True]
      runDb (netosDeGrupo grupo.id)
        `shouldReturn` (netos [(uno, 100), (otro, -100)] `enMoneda` ARS)

    it "la lectura repara un cache que no se puede decodificar" $ \(RunDb runDb) -> do
      (grupo, uno, otro) <- runDb grupoConDosParticipantes
      pago <- runDb $ savePago grupo.id $ gastoEntre ARS 100 uno otro

      runDb $ enfriarCache pago.pagoId $ Just $ Aeson.String "un formato viejo"
      _ <- runDb $ fetchShallowPagos grupo.id
      runDb (netosDeGrupo grupo.id)
        `shouldReturn` (netos [(uno, 100), (otro, -100)] `enMoneda` ARS)

    it "leer no toca los gastos que ya estan calculados" $ \(RunDb runDb) -> do
      (grupo, uno, otro) <- runDb grupoConDosParticipantes
      _ <- runDb $ savePago grupo.id $ gastoEntre ARS 100 uno otro

      antes <- runDb $ netosDeGrupo grupo.id
      _ <- runDb $ fetchShallowPagos grupo.id
      runDb (netosDeGrupo grupo.id) `shouldReturn` antes

    it "un gasto invalido no se recalcula en cada lectura" $ \(RunDb runDb) -> do
      (grupo, uno, otro) <- runDb grupoConDosParticipantes
      -- Un gasto inválido no deja filas, así que "sin filas" por sí solo no
      -- puede significar "frío": lo que lo distingue es el resumen guardado.
      _ <- runDb $ savePago grupo.id $ (gastoEntre ARS 100 uno otro){deudores = distribucionVacia}

      gastos <- runDb $ fetchShallowPagos grupo.id
      fmap esValido gastos `shouldBe` [False]
      fmap (fmap (.errores) . (.resumen)) gastos `shouldNotBe` [Just []]

esValido :: ShallowPago -> Bool
esValido = maybe False gastoEsValido . (.resumen)

grupoConDosParticipantes :: Pg (Grupo, ParticipanteId, ParticipanteId)
grupoConDosParticipantes = do
  grupo <- createGrupo "Test Grupo" "uno"
  otro <-
    addParticipante grupo.id "otro" >>= \case
      Right participante -> pure $ ParticipanteId participante.id
      Left e -> panic e
  pure (grupo, participanteDe grupo, otro)

-- | Un gasto donde uno pone todo y el otro consume todo.
gastoEntre :: Moneda -> Monto -> ParticipanteId -> ParticipanteId -> Pago
gastoEntre moneda monto pagador deudor =
  Pago
    { pagoId = nullUlid
    , monto = monto
    , moneda = moneda
    , nombre = "Gasto"
    , fecha = fromGregorian 2025 1 1
    , pagadores = distribucionDe [MontoFijo monto pagador]
    , deudores = distribucionDe [MontoFijo monto deudor]
    }

distribucionDe :: [Parte] -> Distribucion
distribucionDe partes =
  Distribucion nullUlid $ TipoDistribucionPartes $ DistribucionPartes nullUlid partes

distribucionVacia :: Distribucion
distribucionVacia = distribucionDe []

-- | Los netos reconstruidos desde las distribuciones, sin pasar por el cache.
netosRecalculados :: Grupo -> Pg (PorMoneda (Netos Monto))
netosRecalculados grupo = do
  shallowPagos <- fetchShallowPagos grupo.id
  pagos <- traverse (fetchPago . (.pagoId)) shallowPagos
  pure $ calcularNetosTotales grupo{pagos = pagos}

contarNetosDe :: ULID -> Pg Int
contarNetosDe pagoId =
  fmap length $ runSelectReturningList $ select $ do
    pagoNeto <- all_ db.pago_netos
    guard_ (pagoNeto.pago ==. val_ (Schema.PagoId pagoId))
    pure pagoNeto.participante

-- | Deja el cache de un gasto como si nunca se hubiera calculado (o como si lo
-- hubiera calculado una versión con otro formato de resumen).
enfriarCache :: ULID -> Maybe Aeson.Value -> Pg ()
enfriarCache pagoId resumen = do
  runUpdate $
    update
      db.pagos
      (\p -> p.pagoResumen <-. val_ (fmap PgJSONB resumen))
      (\p -> p.pagoId ==. val_ pagoId)
  runDelete $
    delete
      db.pago_netos
      (\pagoNeto -> pagoNeto.pago ==. val_ (Schema.PagoId pagoId))

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
