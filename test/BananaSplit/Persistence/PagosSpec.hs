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
      shallowBefore <- runDb $ fetchShallowPagos grupo.id Nothing
      fmap esValido shallowBefore `shouldBe` [False]

      -- Claiming the whole item makes the montos add up. Saving the claim must
      -- update the stored flag on its own, since the resumen no longer recomputes
      -- validity on read.
      _ <- runDb $ saveRepartijaClaim repartija.id (RepartijaClaim nullUlid (participanteDe grupo) (primerItem repartija).id Nothing)
      shallowAfter <- runDb $ fetchShallowPagos grupo.id Nothing
      fmap esValido shallowAfter `shouldBe` [True]

    it "deleting a claim turns a valid repartija pago invalid again" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Test Grupo" "alguien"
      pago <- runDb $ saveInvalidRepartijaPago grupo
      let repartija = repartijaDe pago

      claim <- runDb $ saveRepartijaClaim repartija.id (RepartijaClaim nullUlid (participanteDe grupo) (primerItem repartija).id Nothing)
      shallowValid <- runDb $ fetchShallowPagos grupo.id Nothing
      fmap esValido shallowValid `shouldBe` [True]

      runDb $ deleteRepartijaClaim claim.id
      shallowInvalid <- runDb $ fetchShallowPagos grupo.id Nothing
      fmap esValido shallowInvalid `shouldBe` [False]

    -- Al terminar la transacción el cache tiene que estar bien, sin depender de
    -- que alguien lo lea después: cualquier cosa que sume 'pago_netos' por su
    -- cuenta tiene que ver datos correctos.
    it "guardar un claim deja el cache al dia sin que nadie lea" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Test Grupo" "alguien"
      pago <- runDb $ saveInvalidRepartijaPago grupo
      let repartija = repartijaDe pago

      -- Sin claims el gasto es inválido, así que no deja filas.
      runDb (contarNetosDe pago.pagoId) `shouldReturn` 0

      _ <- runDb $ saveRepartijaClaim repartija.id (RepartijaClaim nullUlid (participanteDe grupo) (primerItem repartija).id Nothing)

      -- Sin ninguna lectura por el medio, el cache ya refleja el claim.
      runDb (resumenCrudo pago.pagoId) `shouldNotReturn` Nothing
      runDb (contarNetosDe pago.pagoId) `shouldReturn` 1

    it "borrar un claim tambien deja el cache al dia" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Test Grupo" "alguien"
      pago <- runDb $ saveInvalidRepartijaPago grupo
      let repartija = repartijaDe pago

      claim <- runDb $ saveRepartijaClaim repartija.id (RepartijaClaim nullUlid (participanteDe grupo) (primerItem repartija).id Nothing)
      runDb (contarNetosDe pago.pagoId) `shouldReturn` 1

      runDb $ deleteRepartijaClaim claim.id
      -- Vuelve a ser inválido, y el cache lo dice ya mismo.
      runDb (resumenCrudo pago.pagoId) `shouldNotReturn` Nothing
      runDb (contarNetosDe pago.pagoId) `shouldReturn` 0

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

    it "leer no toca los gastos que ya estan calculados" $ \(RunDb runDb) -> do
      (grupo, uno, otro) <- runDb grupoConDosParticipantes
      _ <- runDb $ savePago grupo.id $ gastoEntre ARS 100 uno otro

      antes <- runDb $ netosDeGrupo grupo.id
      _ <- runDb $ fetchShallowPagos grupo.id Nothing
      runDb (netosDeGrupo grupo.id) `shouldReturn` antes

    -- Mientras corre el backfill de un cambio de formato, los blobs viejos no
    -- decodifican. Eso no puede voltear el listado del grupo: el gasto se
    -- reporta inválido con ese motivo, y sus netos, que salen de las filas y no
    -- del blob, siguen siendo correctos.
    it "un resumen con formato viejo no rompe la lectura" $ \(RunDb runDb) -> do
      (grupo, uno, otro) <- runDb grupoConDosParticipantes
      pago <- runDb $ savePago grupo.id $ gastoEntre ARS 100 uno otro

      runDb $ ensuciarResumen pago.pagoId $ Aeson.String "un formato que ya no existe"

      gastos <- runDb $ fetchShallowPagos grupo.id Nothing
      fmap (fmap (.tipo) . (.errores) . (.resumen)) gastos `shouldBe` [[ErrorNoCalculado]]
      fmap esValido gastos `shouldBe` [False]
      fmap ((.pagado) . (.resumen)) gastos `shouldBe` [netos [(uno, 100), (otro, 0)]]
      runDb (netosDeGrupo grupo.id)
        `shouldReturn` (netos [(uno, 100), (otro, -100)] `enMoneda` ARS)

    it "un resumen sin calcular se reporta igual" $ \(RunDb runDb) -> do
      (grupo, uno, otro) <- runDb grupoConDosParticipantes
      pago <- runDb $ savePago grupo.id $ gastoEntre ARS 100 uno otro

      runDb $ borrarResumen pago.pagoId
      gastos <- runDb $ fetchShallowPagos grupo.id Nothing
      fmap (fmap (.tipo) . (.errores) . (.resumen)) gastos `shouldBe` [[ErrorNoCalculado]]

    it "con participante trae solo sus netos" $ \(RunDb runDb) -> do
      (grupo, uno, otro) <- runDb grupoConDosParticipantes
      _ <- runDb $ savePago grupo.id $ gastoEntre ARS 100 uno otro

      gastos <- runDb $ fetchShallowPagos grupo.id (Just uno)
      fmap ((.pagado) . (.resumen)) gastos `shouldBe` [netos [(uno, 100)]]
      -- Con cero, no ausente: se guarda una fila por cada participante que
      -- aparece de alguno de los dos lados, con cero en el que no le toca.
      fmap ((.consumido) . (.resumen)) gastos `shouldBe` [netos [(uno, 0)]]

    -- Filtrar las filas por participante no puede hacer que un gasto donde esa
    -- persona no aparece se vea como cache frío: si no, se recalcularía entero
    -- (siete queries) en cada lectura.
    it "leer filtrando por alguien que no participa no recalcula el gasto" $ \(RunDb runDb) -> do
      (grupo, uno, otro) <- runDb grupoConDosParticipantes
      tercero <-
        runDb (addParticipante grupo.id "tercero") >>= \case
          Right participante -> pure $ ParticipanteId participante.id
          Left e -> panic e
      pago <- runDb $ savePago grupo.id $ gastoEntre ARS 100 uno otro

      -- Se ensucia el cache a mano: si la lectura lo recalculara, lo pisaría.
      runDb $ ensuciarNetos pago.pagoId
      gastos <- runDb $ fetchShallowPagos grupo.id (Just tercero)
      fmap esValido gastos `shouldBe` [True]

      -- Si hubiera recalculado, la suciedad se habría ido y los netos darían
      -- los de verdad.
      netosDespues <- runDb $ netosDeGrupo grupo.id
      netosDespues `shouldNotBe` (netos [(uno, 100), (otro, -100)] `enMoneda` ARS)

    it "un gasto invalido no se recalcula en cada lectura" $ \(RunDb runDb) -> do
      (grupo, uno, otro) <- runDb grupoConDosParticipantes
      -- Un gasto inválido no deja filas, así que "sin filas" por sí solo no
      -- puede significar "frío": lo que lo distingue es el resumen guardado.
      _ <- runDb $ savePago grupo.id $ (gastoEntre ARS 100 uno otro){deudores = distribucionVacia}

      gastos <- runDb $ fetchShallowPagos grupo.id Nothing
      fmap esValido gastos `shouldBe` [False]
      fmap ((.errores) . (.resumen)) gastos `shouldNotBe` [[]]

esValido :: ShallowPago -> Bool
esValido = gastoEsValido . (.resumen)

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
  shallowPagos <- fetchShallowPagos grupo.id Nothing
  pagos <- traverse (fetchPago . (.pagoId)) shallowPagos
  pure $ calcularNetosTotales grupo{pagos = pagos}

contarNetosDe :: ULID -> Pg Int
contarNetosDe pagoId =
  fmap length $ runSelectReturningList $ select $ do
    pagoNeto <- all_ db.pago_netos
    guard_ (pagoNeto.pago ==. val_ (Schema.PagoId pagoId))
    pure pagoNeto.participante

-- | Deja en el resumen un jsonb que el formato de hoy no puede decodificar,
-- que es lo que se ve mientras corre el backfill de un cambio de formato.
ensuciarResumen :: ULID -> Aeson.Value -> Pg ()
ensuciarResumen pagoId value =
  runUpdate $
    update
      db.pagos
      (\p -> p.pagoResumen <-. val_ (Just (PgJSONB value)))
      (\p -> p.pagoId ==. val_ pagoId)

-- | Como queda un gasto recién migrado, antes del backfill.
borrarResumen :: ULID -> Pg ()
borrarResumen pagoId =
  runUpdate $
    update
      db.pagos
      (\p -> p.pagoResumen <-. val_ Nothing)
      (\p -> p.pagoId ==. val_ pagoId)

-- | El jsonb del resumen tal cual está guardado, para poder distinguir
-- "invalidado" de "calculado" sin pasar por la reparación.
resumenCrudo :: ULID -> Pg (Maybe Aeson.Value)
resumenCrudo pagoId = do
  guardado <- runSelectReturningOne $ select $ do
    pago <- all_ db.pagos
    guard_ (pago.pagoId ==. val_ pagoId)
    pure pago.pagoResumen
  pure $ fmap (\(PgJSONB value) -> value) (join guardado)

-- | Le mete un valor reconocible al cache de un gasto, para poder distinguir
-- una lectura que lo usa de una que lo recalcula por atrás.
ensuciarNetos :: ULID -> Pg ()
ensuciarNetos pagoId =
  runUpdate $
    update
      db.pago_netos
      (\pagoNeto -> pagoNeto.pagado_en_unidades_minimas <-. val_ 999900)
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
