-- | Las queries que quedan colgadas de los spans de base de datos.
--
-- Vive acá y no bajo @BananaSplit.Persistence@ a propósito: el @SpecHook@ de ese
-- namespace levanta una base de datos para cada spec, y esto no la necesita.
module BananaSplit.Telemetry.QueryTracingSpec (
  spec,
) where

import Data.IORef (readIORef)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import OpenTelemetry.Attributes qualified as Attributes
import OpenTelemetry.Exporter.InMemory.Span (inMemoryListExporter)
import OpenTelemetry.Trace.Core (
  Event (..),
  ImmutableSpan (..),
  SpanHot (..),
 )
import OpenTelemetry.Trace.Core qualified as Otel
import OpenTelemetry.Util (appendOnlyBoundedCollectionValues)
import Protolude
import Test.Hspec

import BananaSplit.Persistence qualified as Persistence

-- | Corre una acción con un span y devuelve el span ya cerrado.
--
-- El span va por parámetro, igual que en producción: lo abre quien arranca la
-- transacción y se lo pasa al hook de Beam.
enUnSpan :: (Otel.Span -> IO a) -> IO SpanHot
enUnSpan accion = do
  (processor, ref) <- inMemoryListExporter
  provider <- Otel.createTracerProvider [processor] Otel.emptyTracerProviderOptions
  let tracer = Otel.makeTracer provider instrumentationLibrary Otel.tracerOptions
  _ <- Otel.inSpan' tracer "db.read" Otel.defaultSpanArguments accion
  spans <- readIORef ref
  case spans of
    [span] -> readIORef span.spanHot
    other -> panic $ "se esperaba un span y salieron " <> show (length other)

instrumentationLibrary :: Otel.InstrumentationLibrary
instrumentationLibrary =
  Otel.InstrumentationLibrary
    { libraryName = "test"
    , libraryVersion = ""
    , librarySchemaUrl = ""
    , libraryAttributes = Attributes.emptyAttributes
    }

eventos :: SpanHot -> [Event]
eventos hot = Vector.toList $ appendOnlyBoundedCollectionValues hot.hotEvents

textoDelEvento :: Event -> Maybe Text
textoDelEvento event = textoEn event.eventAttributes "db.query.summary"

textoEn :: Attributes.Attributes -> Text -> Maybe Text
textoEn attrs clave =
  case Attributes.lookupAttribute attrs clave of
    Just (Attributes.AttributeValue (Attributes.TextAttribute texto)) -> Just texto
    _ -> Nothing

enteroEn :: Attributes.Attributes -> Text -> Maybe Int64
enteroEn attrs clave =
  case Attributes.lookupAttribute attrs clave of
    Just (Attributes.AttributeValue (Attributes.IntAttribute n)) -> Just n
    _ -> Nothing

spec :: Spec
spec = do
  -- Los casos de abajo son SQL que generó Beam de verdad, sacado de un trace de
  -- @GET /api/grupo/:id/resumen@. Lo que se anota en el span es esto y no el SQL
  -- completo: contesta la pregunta que uno tiene, no lleva datos de usuario y
  -- está acotado.
  describe "resumirQuery" $ do
    it "saca el verbo y la tabla de un SELECT" $
      Persistence.resumirQuery
        "SELECT \"t0\".\"id\" AS \"res0\", \"t0\".\"nombre\" AS \"res1\" FROM \"grupos\" AS \"t0\" WHERE (\"t0\".\"id\") = ('01M2VY')"
        `shouldBe` "SELECT grupos"

    it "aguanta el DISTINCT" $
      Persistence.resumirQuery
        "SELECT DISTINCT \"t0\".\"moneda\" AS \"res0\" FROM \"pagos\" AS \"t0\" WHERE (\"t0\".\"grupo__id\") = ('01M2VY')"
        `shouldBe` "SELECT pagos"

    it "con una subquery en el FROM deja solo el verbo" $
      Persistence.resumirQuery
        "SELECT \"t0\".\"res0\" AS \"res0\" FROM (SELECT \"t1\".\"id\" AS \"res0\" FROM \"pagos\" AS \"t1\") AS \"t0\""
        `shouldBe` "SELECT"

    it "saca la tabla de los otros verbos" $ do
      Persistence.resumirQuery "INSERT INTO \"pagos\" (\"id\", \"monto\") VALUES ('a', 1)"
        `shouldBe` "INSERT pagos"
      Persistence.resumirQuery "UPDATE \"participantes\" SET \"nombre\" = 'x' WHERE \"id\" = 'y'"
        `shouldBe` "UPDATE participantes"
      Persistence.resumirQuery "DELETE FROM \"pagos\" WHERE \"id\" = 'x'"
        `shouldBe` "DELETE pagos"

    -- La razón principal de resumir: el SQL de Beam trae los valores adentro.
    it "no deja pasar ningún valor de la query" $ do
      let resumen =
            Persistence.resumirQuery
              "SELECT \"t0\".\"email\" FROM \"users\" AS \"t0\" WHERE (\"t0\".\"email\") = ('lucas@ejemplo.com')"
      resumen `shouldBe` "SELECT users"
      resumen `shouldSatisfy` not . Text.isInfixOf "@"

    it "no se rompe con basura" $ do
      Persistence.resumirQuery "" `shouldBe` "(vacía)"
      Persistence.resumirQuery "   " `shouldBe` "(vacía)"
      Persistence.resumirQuery "SELECT" `shouldBe` "SELECT"
      Persistence.resumirQuery "SELECT 1 FROM" `shouldBe` "SELECT"

  -- Los casos salen de renderizar valores con 'pgRenderSyntaxScript', que es lo
  -- que llega acá: así escribe Beam cada tipo.
  describe "taparValores" $ do
    it "tapa el texto y deja las tablas y columnas" $
      Persistence.taparValores
        "SELECT \"t0\".\"email\" FROM \"users\" AS \"t0\" WHERE (\"t0\".\"email\") = ('lucas@ejemplo.com')"
        `shouldBe` "SELECT \"t0\".\"email\" FROM \"users\" AS \"t0\" WHERE (\"t0\".\"email\") = (?)"

    it "tapa cada valor por separado" $
      Persistence.taparValores "INSERT INTO \"pagos\" VALUES ('01ABC', 'Cena', '2026-01-02')"
        `shouldBe` "INSERT INTO \"pagos\" VALUES (?, ?, ?)"

    -- Si cortara en la primera comilla, @brien@ quedaría a la vista.
    it "no corta en una comilla escapada adentro del valor" $
      Persistence.taparValores "SELECT 'o''brien' AS \"res0\""
        `shouldBe` "SELECT ? AS \"res0\""

    it "tapa un valor que es solo una comilla" $
      Persistence.taparValores "SELECT '''' AS \"res0\", 'x' AS \"res1\""
        `shouldBe` "SELECT ? AS \"res0\", ? AS \"res1\""

    it "tapa un valor vacío" $
      Persistence.taparValores "SELECT '' AS \"res0\"" `shouldBe` "SELECT ? AS \"res0\""

    it "la barra no escapa nada" $
      Persistence.taparValores "SELECT 'a \\ b' AS \"res0\", 'c' AS \"res1\""
        `shouldBe` "SELECT ? AS \"res0\", ? AS \"res1\""

    it "tapa un valor de varias líneas" $
      Persistence.taparValores "SELECT 'línea\nnueva' AS \"res0\"" `shouldBe` "SELECT ? AS \"res0\""

    it "deja el cast de un jsonb o un decimal" $
      Persistence.taparValores "SELECT '[1,2]'::jsonb AS \"res0\", '1.5'::double precision AS \"res1\""
        `shouldBe` "SELECT ?::jsonb AS \"res0\", ?::double precision AS \"res1\""

    it "deja los enteros, los booleanos y null" $
      Persistence.taparValores "SELECT 12345 AS \"res0\", true AS \"res1\", null AS \"res2\""
        `shouldBe` "SELECT 12345 AS \"res0\", true AS \"res1\", null AS \"res2\""

    it "sin valores devuelve la query tal cual" $
      Persistence.taparValores "SELECT \"t0\".\"id\" FROM \"grupos\" AS \"t0\""
        `shouldBe` "SELECT \"t0\".\"id\" FROM \"grupos\" AS \"t0\""

    -- Ante la duda no se muestra: si el valor no cierra, todo lo que sigue
    -- podría ser parte de él.
    it "si una comilla no cierra tapa todo lo que sigue" $
      Persistence.taparValores "SELECT 'lucas@ejemplo.com AS \"res0\"" `shouldBe` "SELECT ?"

  -- Esto existe porque el camino es indirecto: Beam avisa de cada statement por un
  -- callback, y si se rompe no hay error — simplemente los spans de base quedan sin
  -- queries adentro.
  describe "eventos por statement" $ do
    it "cuelga el SQL como evento del span" $ do
      hot <- enUnSpan $ \span -> do
        registro <- Persistence.nuevoRegistro span
        unaQuery registro "SELECT * FROM \"grupos\"" (Just 1)

      map (.eventName) (eventos hot) `shouldBe` ["db.query"]
      map textoDelEvento (eventos hot) `shouldBe` [Just "SELECT grupos"]

    it "deja un evento por statement, en orden" $ do
      hot <- enUnSpan $ \span -> do
        registro <- Persistence.nuevoRegistro span
        unaQuery registro "SELECT 1 FROM \"grupos\"" (Just 1)
        unaQuery registro "SELECT 2 FROM \"pagos\"" (Just 1)

      map textoDelEvento (eventos hot) `shouldBe` [Just "SELECT grupos", Just "SELECT pagos"]

    it "lleva la query entera, con los valores tapados" $ do
      hot <- enUnSpan $ \span -> do
        registro <- Persistence.nuevoRegistro span
        unaQuery
          registro
          "SELECT \"t0\".\"email\" FROM \"users\" AS \"t0\" WHERE (\"t0\".\"email\") = ('lucas@ejemplo.com')"
          (Just 1)

      map (\evento -> textoEn evento.eventAttributes "db.query.text") (eventos hot)
        `shouldBe` [Just "SELECT \"t0\".\"email\" FROM \"users\" AS \"t0\" WHERE (\"t0\".\"email\") = (?)"]
      map textoDelEvento (eventos hot) `shouldBe` [Just "SELECT users"]

    it "dice cuántas filas devolvió la query" $ do
      hot <- enUnSpan $ \span -> do
        registro <- Persistence.nuevoRegistro span
        unaQuery registro "SELECT * FROM \"pagos\"" (Just 42)

      map (\evento -> enteroEn evento.eventAttributes "db.response.returned_rows") (eventos hot)
        `shouldBe` [Just 42]

    -- Un INSERT sin RETURNING no devuelve filas: no es que haya devuelto cero.
    it "no inventa filas para un statement que no devuelve ninguna" $ do
      hot <- enUnSpan $ \span -> do
        registro <- Persistence.nuevoRegistro span
        unaQuery registro "INSERT INTO \"pagos\" VALUES (1)" Nothing

      map (\evento -> enteroEn evento.eventAttributes "db.response.returned_rows") (eventos hot)
        `shouldBe` [Nothing]

    -- Que el resumen esté acotado es media razón de ser, así que no puede
    -- depender de que el SQL venga con la forma esperada.
    it "acota el resumen aunque el SQL sea un solo token gigante" $ do
      hot <- enUnSpan $ \span -> do
        registro <- Persistence.nuevoRegistro span
        unaQuery registro (Text.replicate 5000 "x") Nothing

      case mapMaybe textoDelEvento (eventos hot) of
        [texto] -> Text.length texto `shouldBe` 40
        other -> expectationFailure $ "se esperaba un texto y hubo " <> show (length other)

  -- Los atributos son lo que Grafana muestra al clickear el span, que es donde
  -- uno mira primero; los eventos están dos expansiones más abajo.
  describe "atributos del span" $ do
    it "para una sola query deja su resumen" $ do
      hot <- enUnSpan $ \span -> do
        registro <- Persistence.nuevoRegistro span
        unaQuery registro "SELECT * FROM \"grupos\"" (Just 1)
        Persistence.anotarQueriesEnElSpan registro

      textoEn hot.hotAttributes "db.query.summary" `shouldBe` Just "SELECT grupos"
      enteroEn hot.hotAttributes "db.query.count" `shouldBe` Just 1

    it "para varias las une en orden y cuenta cuántas fueron" $ do
      hot <- enUnSpan $ \span -> do
        registro <- Persistence.nuevoRegistro span
        unaQuery registro "SELECT 1 FROM \"grupos\"" (Just 1)
        unaQuery registro "INSERT INTO \"pagos\" VALUES (1)" Nothing
        Persistence.anotarQueriesEnElSpan registro

      textoEn hot.hotAttributes "db.query.summary" `shouldBe` Just "SELECT grupos; INSERT pagos"
      enteroEn hot.hotAttributes "db.query.count" `shouldBe` Just 2

    it "suma las filas que devolvieron todas las queries" $ do
      hot <- enUnSpan $ \span -> do
        registro <- Persistence.nuevoRegistro span
        unaQuery registro "SELECT 1 FROM \"grupos\"" (Just 3)
        unaQuery registro "INSERT INTO \"pagos\" VALUES (1)" Nothing
        unaQuery registro "SELECT 2 FROM \"pagos\"" (Just 40)
        Persistence.anotarQueriesEnElSpan registro

      enteroEn hot.hotAttributes "db.response.returned_rows" `shouldBe` Just 43

    -- La que explota no llega a terminar: no tiene evento, pero tiene que
    -- quedar en el resumen, que es justo cuando más se la quiere ver.
    it "una query que no terminó queda en el resumen aunque no tenga evento" $ do
      hot <- enUnSpan $ \span -> do
        registro <- Persistence.nuevoRegistro span
        unaQuery registro "SELECT 1 FROM \"grupos\"" (Just 1)
        _ <- Persistence.registrarStatement registro "SELECT 2 FROM \"pagos\""
        Persistence.anotarQueriesEnElSpan registro

      textoEn hot.hotAttributes "db.query.summary" `shouldBe` Just "SELECT grupos; SELECT pagos"
      map textoDelEvento (eventos hot) `shouldBe` [Just "SELECT grupos"]
      -- Su SQL entero va en el span, al lado de la excepción.
      textoEn hot.hotAttributes "db.query.text" `shouldBe` Just "SELECT 2 FROM \"pagos\""

    it "si todas terminaron no deja SQL en el span" $ do
      hot <- enUnSpan $ \span -> do
        registro <- Persistence.nuevoRegistro span
        unaQuery registro "SELECT 1 FROM \"grupos\"" (Just 1)
        Persistence.anotarQueriesEnElSpan registro

      textoEn hot.hotAttributes "db.query.text" `shouldBe` Nothing

    it "sin queries no ensucia el span con atributos vacíos" $ do
      hot <- enUnSpan $ \span -> do
        registro <- Persistence.nuevoRegistro span
        Persistence.anotarQueriesEnElSpan registro

      textoEn hot.hotAttributes "db.query.summary" `shouldBe` Nothing
      enteroEn hot.hotAttributes "db.query.count" `shouldBe` Nothing
      enteroEn hot.hotAttributes "db.response.returned_rows" `shouldBe` Nothing

-- | Una query que se manda y termina, devolviendo esas filas.
unaQuery :: Persistence.RegistroDeQueries -> Text -> Maybe Int64 -> IO ()
unaQuery registro sql filas = do
  enCurso <- Persistence.registrarStatement registro sql
  Persistence.terminarStatement registro enCurso filas
