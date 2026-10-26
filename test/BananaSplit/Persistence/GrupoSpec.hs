module BananaSplit.Persistence.GrupoSpec (
  spec,
) where

import Data.Map.Strict qualified as Map
import Data.Time (fromGregorian)
import Database.PostgreSQL.Simple (SqlError (..))
import Protolude
import Test.Hspec

import BananaSplit
import BananaSplit.Persistence
import BananaSplit.Persistence.Pg (contandoQueries)
import BananaSplit.Persistence.SpecHook
import BananaSplit.ULID (nullUlid)
import BananaSplit.User (User (..))

spec :: SpecWith RunDb
spec = do
  describe "createGrupo" $ do
    it "creates a grupo and can fetch it back" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Test Grupo" "alguien"

      maybeGrupo <- runDb $ fetchGrupo grupo.id

      case maybeGrupo of
        Nothing -> expectationFailure "Grupo should exist"
        Just fetchedGrupo -> do
          fetchedGrupo.nombre `shouldBe` "Test Grupo"
          fetchedGrupo.id `shouldBe` grupo.id
          (fetchedGrupo.participantes & fmap (.nombre))
            `shouldBe` ["alguien"]

  describe "fetchGruposForUser" $ do
    it "lists only the grupos where the user claimed a participante" $ \(RunDb runDb) -> do
      user <- runDb $ createUser "yo@example.com" "Yo"
      grupoClaimed <- runDb $ createGrupo "Con claim" "yo"
      _grupoAjeno <- runDb $ createGrupo "Sin claim" "otre"
      grupoPropio <- runDb $ createGrupoForUser "Creado con cuenta" user

      participante <- case grupoClaimed.participantes of
        [p] -> pure p
        _ -> panic "expected exactly one participante"
      claimResult <- runDb $ claimParticipante grupoClaimed.id participante.id user.id
      claimResult `shouldSatisfy` isRight

      grupos <- runDb $ fetchGruposForUser user.id
      (grupos & fmap (.id)) `shouldMatchList` [grupoClaimed.id, grupoPropio.id]

    it "puts first the grupos with the most recent activity" $ \(RunDb runDb) -> do
      user <- runDb $ createUser "yo@example.com" "Yo"
      -- Los ULIDs ordenan por milisegundo; la pausa evita empates entre pasos.
      let pausa = threadDelay 2000
      viejo <- runDb $ createGrupoForUser "Viejo" user
      pausa
      conPagoViejo <- runDb $ createGrupoForUser "Con pago viejo" user
      pausa
      _ <- runDb $ savePago conPagoViejo.id (pagoDe conPagoViejo)
      pausa
      nuevo <- runDb $ createGrupoForUser "Nuevo" user
      pausa
      _ <- runDb $ savePago viejo.id (pagoDe viejo)

      grupos <- runDb $ fetchGruposForUser user.id
      (grupos & fmap (.id)) `shouldBe` [viejo.id, nuevo.id, conPagoViejo.id]

    it "returns nothing for a user without claims" $ \(RunDb runDb) -> do
      user <- runDb $ createUser "nadie@example.com" "Nadie"
      _grupo <- runDb $ createGrupo "Sin claim" "otre"

      grupos <- runDb $ fetchGruposForUser user.id
      grupos `shouldBe` []

  describe "addParticipanteForUser" $ do
    it "adds a participante already claimed by the user" $ \(RunDb runDb) -> do
      user <- runDb $ createUser "nueva@example.com" "Nueva"
      grupo <- runDb $ createGrupo "Ajeno" "otre"

      result <- runDb $ addParticipanteForUser grupo.id "Nueva" user.id

      (result & fmap (\p -> (p.nombre, fmap (.id) p.user))) `shouldBe` Right ("Nueva", Just user.id)
      grupos <- runDb $ fetchGruposForUser user.id
      (grupos & fmap (.id)) `shouldBe` [grupo.id]

    it "refuses when the user already owns a participante in the grupo" $ \(RunDb runDb) -> do
      user <- runDb $ createUser "yo@example.com" "Yo"
      grupo <- runDb $ createGrupoForUser "Propio" user

      result <- runDb $ addParticipanteForUser grupo.id "Otro yo" user.id

      result `shouldBe` Left AlreadyOwnAnotherParticipante
      Just despues <- runDb $ fetchGrupo grupo.id
      length despues.participantes `shouldBe` 1

    it "refuses a name another participante already has" $ \(RunDb runDb) -> do
      user <- runDb $ createUser "otre@example.com" "Otre"
      grupo <- runDb $ createGrupo "Ajeno" "otre"

      result <- runDb $ addParticipanteForUser grupo.id "otre" user.id

      result `shouldBe` Left NameAlreadyTaken

  describe "deleteShallowParticipante" $ do
    -- El handler cuenta con esto para responder un 409 en vez de un 500.
    it "tira una violación de foreign key si el participante está en un gasto" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Grupo" "Juan"
      participante <- unicoParticipante grupo
      _ <- runDb $ savePago grupo.id (pagoDe grupo)

      runDb (deleteShallowParticipante grupo.id participante.id)
        `shouldThrow` (\e -> sqlState e == "23503")

  describe "editarParticipante" $ do
    it "le cambia el nombre al participante" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Grupo" "Juan"
      participante <- unicoParticipante grupo

      result <- runDb $ editarParticipante grupo.id participante.id "Juan Pérez" Nothing

      fmap (.nombre) result `shouldBe` Right "Juan Pérez"
      Just despues <- runDb $ fetchGrupo grupo.id
      (despues.participantes & fmap (.nombre)) `shouldBe` ["Juan Pérez"]

    it "guarda y borra los datos de cobro" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Grupo" "Juan"
      participante <- unicoParticipante grupo

      _ <- runDb $ editarParticipante grupo.id participante.id "Juan" (Just "Alias: juan.mp")
      Just conDatos <- runDb $ fetchGrupo grupo.id
      (conDatos.participantes & fmap (.datosDeCobro)) `shouldBe` [Just "Alias: juan.mp"]

      _ <- runDb $ editarParticipante grupo.id participante.id "Juan" Nothing
      Just sinDatos <- runDb $ fetchGrupo grupo.id
      (sinDatos.participantes & fmap (.datosDeCobro)) `shouldBe` [Nothing]

    it "trae los datos de cobro de la cuenta que lo reclamó" $ \(RunDb runDb) -> do
      user <- runDb $ createUser "juan@example.com" "Juan"
      grupo <- runDb $ createGrupoForUser "Grupo" user
      _ <- runDb $ updateUser user.id "Juan" (Just "CBU 123")

      Just despues <- runDb $ fetchGrupo grupo.id
      (despues.participantes & fmap (\p -> fmap (.datosDeCobro) p.user)) `shouldBe` [Just (Just "CBU 123")]

    it "deja poner el mismo nombre que ya tenía" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Grupo" "Juan"
      participante <- unicoParticipante grupo

      result <- runDb $ editarParticipante grupo.id participante.id "Juan" Nothing

      fmap (.nombre) result `shouldBe` Right "Juan"

    it "no deja usar el nombre de otro participante del grupo" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Grupo" "Juan"
      participante <- unicoParticipante grupo
      _ <- runDb $ addParticipante grupo.id "Ana"

      result <- runDb $ editarParticipante grupo.id participante.id "Ana" Nothing

      fmap (.nombre) result `shouldBe` Left NombreRepetido

    it "no encuentra un participante de otro grupo" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Grupo" "Juan"
      otroGrupo <- runDb $ createGrupo "Otro" "Ana"
      participante <- unicoParticipante grupo

      result <- runDb $ editarParticipante otroGrupo.id participante.id "Pepe" Nothing

      fmap (.nombre) result `shouldBe` Left ParticipanteNoExiste

  describe "fetchGruposForUser estado" $ do
    it "says there are no gastos in a new grupo" $ \(RunDb runDb) -> do
      user <- runDb $ createUser "yo@example.com" "Yo"
      _ <- runDb $ createGrupoForUser "Nuevo" user

      grupos <- runDb $ fetchGruposForUser user.id
      (grupos & fmap (.estado))
        `shouldBe` [EnGrupoAbierto AbiertoParaUsuario{neto = mempty, cantidadPagos = 0, cantidadPagosInvalidos = 0}]

    it "shows the user's neto in an open grupo" $ \(RunDb runDb) -> do
      user <- runDb $ createUser "yo@example.com" "Yo"
      _ <- runDb $ grupoAbierto user "Abierto"

      grupos <- runDb $ fetchGruposForUser user.id
      (grupos & fmap (.estado))
        `shouldBe` [EnGrupoAbierto AbiertoParaUsuario{neto = enMoneda 100 ARS, cantidadPagos = 1, cantidadPagosInvalidos = 0}]

    it "shows what is pending to collect once frozen" $ \(RunDb runDb) -> do
      user <- runDb $ createUser "yo@example.com" "Yo"
      _ <- runDb $ grupoCongelado user "Congelado"

      grupos <- runDb $ fetchGruposForUser user.id
      (grupos & fmap (.estado))
        `shouldBe` [EnGrupoCongelado CongeladoParaUsuario{aCobrar = enMoneda 100 ARS, aPagar = mempty}]

  -- Lo que muestra la home tiene que ser lo mismo que el usuario ve adentro
  -- del grupo, con cualquier mezcla de monedas.
  describe "fetchGruposForUser con varias monedas" $ do
    it "converts every moneda to the default one when there are tasas" $ \(RunDb runDb) -> do
      user <- runDb $ createUser "yo@example.com" "Yo"
      estado <- runDb $ do
        (grupo, yo, otro) <- grupoConDos user "Viaje"
        dolaresSonPesos grupo.id 1 1000
        -- Me deben 100 pesos y debo 10 dólares, que son 10.000 pesos.
        _ <- savePago grupo.id (gasto ARS yo [(otro, 100)])
        _ <- savePago grupo.id (gasto USD otro [(yo, 10)])
        estadoEnLaHome user

      estado
        `shouldBe` EnGrupoAbierto AbiertoParaUsuario{neto = enMoneda (-9900) ARS, cantidadPagos = 2, cantidadPagosInvalidos = 0}

    it "keeps a moneda without tasa apart instead of making up a conversion" $ \(RunDb runDb) -> do
      user <- runDb $ createUser "yo@example.com" "Yo"
      estado <- runDb $ do
        (grupo, yo, otro) <- grupoConDos user "Viaje"
        _ <- savePago grupo.id (gasto ARS yo [(otro, 100)])
        _ <- savePago grupo.id (gasto USD otro [(yo, 10)])
        estadoEnLaHome user

      estado
        `shouldBe` EnGrupoAbierto
          AbiertoParaUsuario
            { neto = enMoneda 100 ARS <> enMoneda (-10) USD
            , cantidadPagos = 2
            , cantidadPagosInvalidos = 0
            }

    it "converts only the monedas that have a tasa" $ \(RunDb runDb) -> do
      user <- runDb $ createUser "yo@example.com" "Yo"
      estado <- runDb $ do
        (grupo, yo, otro) <- grupoConDos user "Viaje"
        dolaresSonPesos grupo.id 1 1000
        _ <- savePago grupo.id (gasto ARS yo [(otro, 100)])
        _ <- savePago grupo.id (gasto USD otro [(yo, 10)])
        _ <- savePago grupo.id (gasto EUR yo [(otro, 5)])
        estadoEnLaHome user

      estado
        `shouldBe` EnGrupoAbierto
          AbiertoParaUsuario
            { neto = enMoneda (-9900) ARS <> enMoneda 5 EUR
            , cantidadPagos = 3
            , cantidadPagosInvalidos = 0
            }

    -- Con 3 USD = 10 ARS, convertir a cada uno por separado redondea mal:
    -- los dos que consumieron 0,02 USD darían -0,07 cada uno, el de 0,96 daría
    -- -3,20 y el que pagó +3,33, y entre todos sobra un centavo que no existe.
    -- Por eso se convierte el grupo entero y recién después se mira a cada uno.
    it "shows balances that add up to zero across the grupo even when converting rounds" $ \(RunDb runDb) -> do
      estados <- runDb $ do
        pagador <- createUser "pagador@example.com" "Pagador"
        grupo <- createGrupoForUser "Viaje" pagador
        dolaresSonPesos grupo.id 3 10
        consumidores <- forM [("b@example.com", "b"), ("c@example.com", "c"), ("d@example.com", "d")] $ \(email, nombre) -> do
          user <- createUser email nombre
          participante <-
            addParticipante grupo.id nombre >>= \case
              Right p -> pure p
              Left e -> panic e
          claimParticipante grupo.id participante.id user.id >>= \case
            Right _ -> pure ()
            Left e -> panic (show e)
          pure (user, ParticipanteId participante.id)
        let elQuePaga = case grupo.participantes of
              [p] -> ParticipanteId p.id
              _ -> panic "expected exactly one participante"
        _ <-
          savePago grupo.id $
            gasto USD elQuePaga $
              zip (fmap snd consumidores) [mkMonto 2 2, mkMonto 2 2, mkMonto 2 96]
        forM (pagador : fmap fst consumidores) estadoEnLaHome

      let netos =
            estados
              & fmap
                ( \case
                    EnGrupoAbierto abierto -> abierto.neto
                    EnGrupoCongelado _ -> panic "el grupo no está congelado"
                )
      -- Todo en pesos: hay tasa, así que nadie tiene una línea en dólares.
      netos `shouldSatisfy` all (\(PorMoneda porMoneda) -> Map.keys porMoneda == [ARS])
      sum (fmap (\(PorMoneda porMoneda) -> sum porMoneda) netos) `shouldBe` 0
      -- El que pagó ve el dólar entero convertido.
      take 1 netos `shouldBe` [enMoneda (mkMonto 2 333) ARS]

    it "asks to transfer the same that the open grupo showed once frozen" $ \(RunDb runDb) -> do
      user <- runDb $ createUser "yo@example.com" "Yo"
      (abierto, congelado) <- runDb $ do
        (grupo, yo, otro) <- grupoConDos user "Viaje"
        dolaresSonPesos grupo.id 1 1000
        _ <- savePago grupo.id (gasto ARS yo [(otro, 100)])
        _ <- savePago grupo.id (gasto USD otro [(yo, 10)])
        abierto <- estadoEnLaHome user
        congelarGrupo grupo.id >>= \case
          Right _ -> pure ()
          Left e -> panic (show e)
        congelado <- estadoEnLaHome user
        pure (abierto, congelado)

      abierto
        `shouldBe` EnGrupoAbierto AbiertoParaUsuario{neto = enMoneda (-9900) ARS, cantidadPagos = 2, cantidadPagosInvalidos = 0}
      congelado
        `shouldBe` EnGrupoCongelado CongeladoParaUsuario{aCobrar = mempty, aPagar = enMoneda 9900 ARS}

  -- Sin esto un N+1 no se nota: la lista sale igual de bien, solo que con una
  -- tanda de queries por grupo.
  describe "fetchGruposForUser queries" $ do
    it "does not grow with the number of grupos" $ \(RunDb runDb) -> do
      -- Los dos usuarios con la misma mezcla de abiertos y congelados: lo que
      -- se pide para cada clase solo se pide si hay alguno de esa clase, y la
      -- cuenta tiene que diferir únicamente en la cantidad.
      let queriesConGrupos email cantidad = runDb $ do
            user <- createUser email "Yo"
            forM_ [1 .. cantidad :: Int] $ \i -> do
              grupoAbierto user ("Abierto " <> show i)
              grupoCongelado user ("Congelado " <> show i)
            (grupos, queries) <- contandoQueries (fetchGruposForUser user.id)
            pure (length grupos, queries)

      (gruposPocos, queriesPocos) <- queriesConGrupos "pocos@example.com" 1
      (gruposMuchos, queriesMuchos) <- queriesConGrupos "muchos@example.com" 3

      (gruposPocos, gruposMuchos) `shouldBe` (2, 6)
      -- Si el contador no viera nada, las dos listas vacías serían iguales.
      queriesPocos `shouldNotBe` []
      queriesMuchos `shouldBe` queriesPocos

-- | Un grupo del usuario con otro participante más.
grupoConDos :: User -> Text -> Pg (Grupo, ParticipanteId, ParticipanteId)
grupoConDos user nombre = do
  grupo <- createGrupoForUser nombre user
  yo <- case grupo.participantes of
    [p] -> pure $ ParticipanteId p.id
    _ -> panic "expected exactly one participante"
  otro <-
    addParticipante grupo.id "otro" >>= \case
      Right p -> pure $ ParticipanteId p.id
      Left e -> panic e
  pure (grupo, yo, otro)

-- | Abierto y con un gasto en el que el usuario puso 100 y el otro los consumió.
grupoAbierto :: User -> Text -> Pg ()
grupoAbierto user nombre = do
  (grupo, yo, otro) <- grupoConDos user nombre
  void $ savePago grupo.id (pagoEntre yo otro)

-- | Como 'grupoAbierto', pero congelado: al usuario le queda 100 por cobrar.
grupoCongelado :: User -> Text -> Pg ()
grupoCongelado user nombre = do
  (grupo, yo, otro) <- grupoConDos user nombre
  void $ savePago grupo.id (pagoEntre yo otro)
  congelarGrupo grupo.id >>= \case
    Right _ -> pure ()
    Left e -> panic (show e)

-- | Un pago cualquiera, pagado y consumido por el primer participante.
pagoDe :: Grupo -> Pago
pagoDe grupo = pagoEntre uno uno
  where
    uno = case grupo.participantes of
      (p : _) -> ParticipanteId p.id
      [] -> panic "el grupo deberia tener un participante"

-- | 100 pesos que pone uno y consume el otro.
pagoEntre :: ParticipanteId -> ParticipanteId -> Pago
pagoEntre pagador deudor = gasto ARS pagador [(deudor, 100)]

-- | Un gasto que paga entero uno solo y que consumen los demás en esas partes.
gasto :: Moneda -> ParticipanteId -> [(ParticipanteId, Monto)] -> Pago
gasto moneda pagador consumos =
  Pago
    { pagoId = nullUlid
    , monto = total
    , moneda = moneda
    , nombre = "Cena"
    , fecha = fromGregorian 2026 1 1
    , pagadores = partes [(pagador, total)]
    , deudores = partes consumos
    }
  where
    total = sum (fmap snd consumos)
    partes montos =
      Distribucion nullUlid $
        TipoDistribucionPartes $
          DistribucionPartes nullUlid (fmap (\(participante, monto) -> MontoFijo monto participante) montos)

-- | La tasa del grupo: tantos dólares son tantos pesos.
dolaresSonPesos :: ULID -> Monto -> Monto -> Pg ()
dolaresSonPesos grupoId dolares pesos =
  void $
    guardarTasasDeCambio
      grupoId
      ARS
      [TasaDeCambio{id = nullUlid, unaMoneda = USD, otraMoneda = ARS, unMonto = dolares, otroMonto = pesos}]

unicoParticipante :: Grupo -> IO Participante
unicoParticipante grupo = case grupo.participantes of
  [p] -> pure p
  _ -> panic "expected exactly one participante"

-- | El estado que le muestra la home a ese usuario en su único grupo.
estadoEnLaHome :: User -> Pg EstadoParaUsuario
estadoEnLaHome user =
  fetchGruposForUser user.id >>= \case
    [grupo] -> pure grupo.estado
    grupos -> panic $ "se esperaba un grupo y hubo " <> show (length grupos)
