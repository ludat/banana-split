-- | Las queries que quedan colgadas de los spans de base de datos.
--
-- Vive acá y no bajo @BananaSplit.Persistence@ a propósito: el @SpecHook@ de ese
-- namespace levanta una base de datos para cada spec, y esto no la necesita.
module BananaSplit.Telemetry.QueryTracingSpec (
  spec,
) where

import Data.IORef (newIORef, readIORef)
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

-- | Corre una acción dentro de un span y devuelve el span ya cerrado.
enUnSpan :: IO a -> IO SpanHot
enUnSpan accion = do
  (processor, ref) <- inMemoryListExporter
  provider <- Otel.createTracerProvider [processor] Otel.emptyTracerProviderOptions
  let tracer = Otel.makeTracer provider instrumentationLibrary Otel.tracerOptions
  _ <- Otel.inSpan tracer "db.read" Otel.defaultSpanArguments accion
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

  -- Esto existe porque el camino es indirecto y silencioso: Beam avisa de cada
  -- statement por un callback, y el span al que se le cuelga todo sale del
  -- contexto thread-local. Si cualquiera de las dos mitades se rompe no hay
  -- error — simplemente los spans de base quedan sin queries adentro.
  describe "eventos por statement" $ do
    it "cuelga el SQL como evento del span activo" $ do
      hot <- enUnSpan $ do
        statements <- newIORef []
        Persistence.registrarStatement statements "SELECT * FROM \"grupos\""

      map (.eventName) (eventos hot) `shouldBe` ["db.query"]
      map textoDelEvento (eventos hot) `shouldBe` [Just "SELECT grupos"]

    it "deja un evento por statement, en orden" $ do
      hot <- enUnSpan $ do
        statements <- newIORef []
        Persistence.registrarStatement statements "SELECT 1 FROM \"grupos\""
        Persistence.registrarStatement statements "SELECT 2 FROM \"pagos\""

      map textoDelEvento (eventos hot) `shouldBe` [Just "SELECT grupos", Just "SELECT pagos"]

    -- Que el resumen esté acotado es media razón de ser, así que no puede
    -- depender de que el SQL venga con la forma esperada.
    it "acota el resumen aunque el SQL sea un solo token gigante" $ do
      hot <- enUnSpan $ do
        statements <- newIORef []
        Persistence.registrarStatement statements (replicate 5000 'x')

      case mapMaybe textoDelEvento (eventos hot) of
        [texto] -> Text.length texto `shouldBe` 40
        other -> expectationFailure $ "se esperaba un texto y hubo " <> show (length other)

  -- Los atributos son lo que Grafana muestra al clickear el span, que es donde
  -- uno mira primero; los eventos están dos expansiones más abajo.
  describe "atributos del span" $ do
    it "para una sola query deja su resumen" $ do
      hot <- enUnSpan $ do
        statements <- newIORef []
        Persistence.registrarStatement statements "SELECT * FROM \"grupos\""
        Persistence.anotarQueriesEnElSpan statements

      textoEn hot.hotAttributes "db.query.summary" `shouldBe` Just "SELECT grupos"
      enteroEn hot.hotAttributes "db.query.count" `shouldBe` Just 1

    it "para varias las une en orden y cuenta cuántas fueron" $ do
      hot <- enUnSpan $ do
        statements <- newIORef []
        Persistence.registrarStatement statements "SELECT 1 FROM \"grupos\""
        Persistence.registrarStatement statements "INSERT INTO \"pagos\" VALUES (1)"
        Persistence.anotarQueriesEnElSpan statements

      textoEn hot.hotAttributes "db.query.summary" `shouldBe` Just "SELECT grupos; INSERT pagos"
      enteroEn hot.hotAttributes "db.query.count" `shouldBe` Just 2

    it "sin queries no ensucia el span con atributos vacíos" $ do
      hot <- enUnSpan $ do
        statements <- newIORef []
        Persistence.anotarQueriesEnElSpan statements

      textoEn hot.hotAttributes "db.query.summary" `shouldBe` Nothing
      enteroEn hot.hotAttributes "db.query.count" `shouldBe` Nothing

  -- Las migraciones corren por el mismo camino y no tienen span activo.
  describe "sin span activo" $ do
    it "no hace nada y no rompe" $ do
      statements <- newIORef []
      Persistence.registrarStatement statements "SELECT 1 FROM \"grupos\""
      Persistence.anotarQueriesEnElSpan statements
      readIORef statements `shouldReturn` ["SELECT grupos"]
