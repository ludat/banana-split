{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module BananaSplit.Persistence.PagosSpec (
  spec,
) where

import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Data.Time (fromGregorian)
import Database.Beam
import Database.Beam.Postgres (Pg, PgJSONB (..), liftIOWithHandle)
import Database.PostgreSQL.Simple qualified as Simple
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
      fetchedPago <- runDb $ fetchPago grupo.id savedPago.pagoId
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

      repartijaConClaim <- runDb $ saveRepartijaClaim repartija.id (RepartijaClaim nullUlid (participanteDe grupo) (primerItem repartija).id Nothing)
      shallowValid <- runDb $ fetchShallowPagos grupo.id Nothing
      fmap esValido shallowValid `shouldBe` [True]

      runDb $ deleteRepartijaClaim (primerClaim repartijaConClaim).id
      shallowInvalid <- runDb $ fetchShallowPagos grupo.id Nothing
      fmap esValido shallowInvalid `shouldBe` [False]

    -- Al terminar la transacción el cache tiene que estar bien, sin depender de
    -- que alguien lo lea después: cualquier cosa que sume 'pagado_y_consumido_en_gasto' por su
    -- cuenta tiene que ver datos correctos.
    it "guardar un claim deja el cache al dia sin que nadie lea" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Test Grupo" "alguien"
      pago <- runDb $ saveInvalidRepartijaPago grupo
      let repartija = repartijaDe pago

      -- Sin claims el gasto es inválido, así que no deja filas.
      runDb (contarFilasDe pago.pagoId) `shouldReturn` 0

      _ <- runDb $ saveRepartijaClaim repartija.id (RepartijaClaim nullUlid (participanteDe grupo) (primerItem repartija).id Nothing)

      -- Sin ninguna lectura por el medio, el cache ya refleja el claim.
      runDb (resumenCrudo pago.pagoId) `shouldNotReturn` Just sinCalcularCrudo
      runDb (contarFilasDe pago.pagoId) `shouldReturn` 1

    it "borrar un claim tambien deja el cache al dia" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Test Grupo" "alguien"
      pago <- runDb $ saveInvalidRepartijaPago grupo
      let repartija = repartijaDe pago

      repartijaConClaim <- runDb $ saveRepartijaClaim repartija.id (RepartijaClaim nullUlid (participanteDe grupo) (primerItem repartija).id Nothing)
      runDb (contarFilasDe pago.pagoId) `shouldReturn` 1

      runDb $ deleteRepartijaClaim (primerClaim repartijaConClaim).id
      -- Vuelve a ser inválido, y el cache lo dice ya mismo.
      runDb (resumenCrudo pago.pagoId) `shouldNotReturn` Just sinCalcularCrudo
      runDb (contarFilasDe pago.pagoId) `shouldReturn` 0

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

    -- La UI manda siempre la escala de la moneda y el reparto sale en esa misma
    -- escala, así que un monto con más decimales sólo puede llegar de un cliente
    -- que no sea el nuestro. Guardarlo redondeado cambiaría lo que el gasto dice.
    it "un monto con mas precision que su moneda no se guarda redondeado" $ \(RunDb runDb) -> do
      (grupo, uno, otro) <- runDb grupoConDosParticipantes
      -- 5.005 en una moneda de dos decimales.
      let demasiadoPreciso = mkMonto 3 5005
      runDb (savePago grupo.id $ gastoEntre ARS demasiadoPreciso uno otro)
        `shouldThrow` (\(FatalError mensaje) -> "más precisión" `Text.isInfixOf` mensaje)

    it "un gasto invalido no deja filas en el cache" $ \(RunDb runDb) -> do
      (grupo, uno, otro) <- runDb grupoConDosParticipantes
      pago <- runDb $ savePago grupo.id $ gastoEntre ARS 100 uno otro
      runDb (contarFilasDe pago.pagoId) `shouldReturn` 2

      _ <- runDb $ updatePago grupo.id pago.pagoId pago{deudores = distribucionVacia}
      runDb (contarFilasDe pago.pagoId) `shouldReturn` 0

    it "borrar un gasto se lleva sus filas del cache" $ \(RunDb runDb) -> do
      (grupo, uno, otro) <- runDb grupoConDosParticipantes
      pago <- runDb $ savePago grupo.id $ gastoEntre ARS 100 uno otro
      runDb (contarFilasDe pago.pagoId) `shouldReturn` 2

      runDb $ deletePago pago.pagoId
      runDb (contarFilasDe pago.pagoId) `shouldReturn` 0

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

      -- Un objeto cuyos campos son de otra forma. Tiene que ser un objeto: la
      -- columna está tipada como 'ResumenGuardado', y ese decoder degrada campo
      -- por campo pero no rescata nada de un blob que no sea un objeto.
      runDb $
        ensuciarResumen pago.pagoId $
          Aeson.object [("errores", Aeson.String "un formato que ya no existe")]

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

    it "contarGastos cuenta los gastos y los invalidos" $ \(RunDb runDb) -> do
      (grupo, uno, otro) <- runDb grupoConDosParticipantes
      runDb (contarGastos grupo.id) `shouldReturn` ConteoDeGastos{total = 0, invalidos = 0}

      _ <- runDb $ savePago grupo.id $ gastoEntre ARS 100 uno otro
      runDb (contarGastos grupo.id) `shouldReturn` ConteoDeGastos{total = 1, invalidos = 0}

      _ <- runDb $ savePago grupo.id $ (gastoEntre ARS 70 uno otro){deudores = distribucionVacia}
      runDb (contarGastos grupo.id) `shouldReturn` ConteoDeGastos{total = 2, invalidos = 1}

    it "un gasto sin calcular cuenta como invalido" $ \(RunDb runDb) -> do
      (grupo, uno, otro) <- runDb grupoConDosParticipantes
      pago <- runDb $ savePago grupo.id $ gastoEntre ARS 100 uno otro
      runDb (contarGastos grupo.id) `shouldReturn` ConteoDeGastos{total = 1, invalidos = 0}

      -- Como queda tras la migración: sin resumen y sin filas.
      runDb $ borrarResumen pago.pagoId
      runDb $ borrarPagadoYConsumido pago.pagoId
      runDb (contarGastos grupo.id) `shouldReturn` ConteoDeGastos{total = 1, invalidos = 1}

    -- El criterio de la base mira el jsonb, igual que el decoder: un blob con
    -- formato viejo no se sabe si cierra, así que cuenta inválido aunque haya
    -- dejado filas en 'pagado_y_consumido_en_gasto'. Así el contador dice lo mismo que el
    -- triángulo que muestra la lista.
    it "un resumen ilegible cuenta como invalido aunque tenga netos" $ \(RunDb runDb) -> do
      (grupo, uno, otro) <- runDb grupoConDosParticipantes
      pago <- runDb $ savePago grupo.id $ gastoEntre ARS 100 uno otro
      runDb (contarGastos grupo.id) `shouldReturn` ConteoDeGastos{total = 1, invalidos = 0}

      runDb $ ensuciarResumen pago.pagoId $ Aeson.String "un formato que ya no existe"
      runDb (contarFilasDe pago.pagoId) `shouldReturn` 2
      runDb (contarGastos grupo.id) `shouldReturn` ConteoDeGastos{total = 1, invalidos = 1}

    it "no cuenta los gastos de otro grupo" $ \(RunDb runDb) -> do
      (grupo, uno, otro) <- runDb grupoConDosParticipantes
      (otroGrupo, unoDeAlla, otroDeAlla) <- runDb grupoConDosParticipantes
      _ <- runDb $ savePago grupo.id $ gastoEntre ARS 100 uno otro
      _ <- runDb $ savePago otroGrupo.id $ (gastoEntre ARS 70 unoDeAlla otroDeAlla){deudores = distribucionVacia}

      runDb (contarGastos grupo.id) `shouldReturn` ConteoDeGastos{total = 1, invalidos = 0}
      runDb (contarGastos otroGrupo.id) `shouldReturn` ConteoDeGastos{total = 1, invalidos = 1}

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
      runDb $ ensuciarPagadoYConsumido pago.pagoId
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
esValido = resumenGastoEsValido . (.resumen)

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
  pagos <- traverse (fetchPago grupo.id . (.pagoId)) shallowPagos
  pure $ calcularNetosTotales grupo{pagos = pagos}

contarFilasDe :: ULID -> Pg Int
contarFilasDe pagoId =
  fmap length $ runSelectReturningList $ select $ do
    fila <- all_ db.pagado_y_consumido_en_gasto
    guard_ (fila.gasto ==. val_ (Schema.PagoId pagoId))
    pure fila.participante

-- | Deja en el resumen un jsonb que el formato de hoy no puede decodificar,
-- que es lo que se ve mientras corre el backfill de un cambio de formato.
--
-- Va por SQL crudo porque la columna está tipada como 'ResumenGuardado': por
-- beam sólo se puede escribir algo que ya sea un resumen válido, que es
-- justamente lo que acá no queremos.
ensuciarResumen :: ULID -> Aeson.Value -> Pg ()
ensuciarResumen pagoId value =
  liftIOWithHandle $ \conn ->
    void $
      Simple.execute
        conn
        "UPDATE public.pagos SET resumen = ? WHERE id = ?"
        (value, show pagoId :: Text)

borrarPagadoYConsumido :: ULID -> Pg ()
borrarPagadoYConsumido pagoId =
  runDelete $
    delete
      db.pagado_y_consumido_en_gasto
      (\fila -> fila.gasto ==. val_ (Schema.PagoId pagoId))

-- | El jsonb de un gasto todavía sin calcular. La columna no acepta NULL, así
-- que "no lo calculé" es el objeto vacío: no se le puede leer ningún campo.
sinCalcularCrudo :: Aeson.Value
sinCalcularCrudo = Aeson.object []

-- | Como queda un gasto recién migrado, antes del backfill.
borrarResumen :: ULID -> Pg ()
borrarResumen pagoId = ensuciarResumen pagoId sinCalcularCrudo

-- | El jsonb del resumen tal cual está guardado, sin pasar por el decoder.
resumenCrudo :: ULID -> Pg (Maybe Aeson.Value)
resumenCrudo pagoId =
  liftIOWithHandle $ \conn -> do
    filas <-
      Simple.query
        conn
        "SELECT resumen FROM public.pagos WHERE id = ?"
        (Simple.Only (show pagoId :: Text))
    pure $ case filas of
      (Simple.Only value : _) -> Just value
      [] -> Nothing

-- | Le mete un valor reconocible al cache de un gasto, para poder distinguir
-- una lectura que lo usa de una que lo recalcula por atrás.
ensuciarPagadoYConsumido :: ULID -> Pg ()
ensuciarPagadoYConsumido pagoId =
  runUpdate $
    update
      db.pagado_y_consumido_en_gasto
      (\fila -> fila.pagado_en_unidades_minimas <-. val_ 999900)
      (\fila -> fila.gasto ==. val_ (Schema.PagoId pagoId))

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

-- | El único claim de la repartija que devolvió 'saveRepartijaClaim'.
primerClaim :: RepartijaForFrontend -> RepartijaClaim
primerClaim repartijaPage = case repartijaPage.repartija.claims of
  (claim : _) -> claim
  [] -> panic "la repartija deberia tener un claim"

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
