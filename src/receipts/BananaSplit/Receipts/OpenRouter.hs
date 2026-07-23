{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}

module BananaSplit.Receipts.OpenRouter (
  ParsedReceipt (..),
  ParsedReceiptItem (..),
  analyzeReceiptImage,
  analyzeReceiptText,
  -- * Parsing a pago out of an email
  ParsedEmailPago (..),
  ParsedDistribucion (..),
  ParsedShare (..),
  EmailPagoContext (..),
  EmailPagoParticipante (..),
  analyzePagoFromEmail,
) where

import Control.Arrow (left)
import Control.Monad.Error.Class
import Data.Aeson
import Data.Scientific (Scientific)
import Data.String.Interpolate (__i)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Network.HTTP.Req
import Protolude

import BananaSplit.Receipts.Tesseract (extractTextFromImage)
import BananaSplit.Receipts.Types

newtype ParsedReceipt = ParsedReceipt
  { items :: [ParsedReceiptItem]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ParsedReceiptItem = ParsedReceiptItem
  { nombre :: Text
  , monto :: Scientific
  , cantidad :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The single pago the AI parses out of an inbound email. All fields are the
-- "wire" shape the model returns (plain 'Text'/'Scientific'); the caller
-- resolves them against a real grupo (participante ids, moneda, fecha).
data ParsedEmailPago = ParsedEmailPago
  { nombre :: Text
  , monto :: Scientific
  , moneda :: Maybe Text
  , fecha :: Maybe Text
  -- ^ ISO day (@YYYY-MM-DD@); 'Nothing' means "use today".
  , pagadores :: ParsedDistribucion
  , deudores :: ParsedDistribucion
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | How one side of the pago is split, as chosen by the model per email:
--
--   * 'ParsedPartes' — a simple split: each person a fixed amount or a weight.
--   * 'ParsedRepartija' — an itemized list of products (like a receipt); people
--     are not assigned here, they claim items later (mirrors the receipt-image
--     parse), so a repartija-based pago is saved but only becomes valid once
--     items are claimed.
--
-- Serialised as a @tipo@-tagged object, but decoding does not rely on @tipo@:
-- the model routinely forgets it, so we infer the shape from whichever of the
-- @partes@ / @items@ keys it actually filled in (see 'FromJSON').
data ParsedDistribucion
  = ParsedPartes [ParsedShare]
  | ParsedRepartija [ParsedReceiptItem]
  deriving stock (Show, Eq, Generic)

instance ToJSON ParsedDistribucion where
  toJSON = \case
    ParsedPartes partes ->
      object ["tipo" .= ("partes" :: Text), "partes" .= partes]
    ParsedRepartija items ->
      object ["tipo" .= ("repartija" :: Text), "items" .= items]

-- | Tolerant decoder. An explicit @tipo@ still wins when present, but when it is
-- missing (a frequent model slip) we fall back to the presence of the
-- shape-specific key: @items@ means a repartija, @partes@ means a parts split.
-- Anything else decodes to an empty parts split rather than failing, in keeping
-- with the permissive parsing this feature wants.
instance FromJSON ParsedDistribucion where
  parseJSON = withObject "ParsedDistribucion" $ \o -> do
    tipo <- o .:? "tipo"
    items <- o .:? "items"
    partes <- o .:? "partes"
    pure $ case (tipo :: Maybe Text, items, partes) of
      (Just "repartija", _, _) -> ParsedRepartija (fromMaybe [] items)
      (Just "partes", _, _) -> ParsedPartes (fromMaybe [] partes)
      (_, Just is, _) -> ParsedRepartija is
      (_, _, Just ps) -> ParsedPartes ps
      _ -> ParsedPartes []

-- | One person's involvement in a parts split. Exactly one of 'monto' (a fixed
-- share) or 'partes' (an integer weight for an even/proportional split) is
-- expected; if neither is given the caller defaults to one part.
data ParsedShare = ParsedShare
  { participanteId :: Text
  , monto :: Maybe Scientific
  , partes :: Maybe Integer
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The (single, already-authorized) grupo context handed to the model so it
-- can map names it reads to real participante ids and pick a currency.
data EmailPagoContext = EmailPagoContext
  { grupoNombre :: Text
  , monedaPorDefecto :: Text
  , monedasPermitidas :: [Text]
  , participantes :: [EmailPagoParticipante]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data EmailPagoParticipante = EmailPagoParticipante
  { id :: Text
  , nombre :: Text
  , esRemitente :: Bool
  -- ^ True for the participante who wrote the email, so the model can resolve
  -- first-person references ("yo pagué", "I paid") to their id.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data OpenRouterRequest = OpenRouterRequest
  { model :: Text
  , messages :: [OpenRouterMessage]
  , models :: [Text]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data OpenRouterMessage = OpenRouterMessage
  { role :: Text
  , content :: [MessageContent]
  }
  deriving (Show, Generic)
  deriving anyclass (ToJSON)

data MessageContent
  = TextContent Text
  | ImageContent ImageUrl
  deriving (Show, Generic)

instance ToJSON MessageContent where
  toJSON (TextContent txt) =
    object
      [ "type" .= ("text" :: Text)
      , "text" .= txt
      ]
  toJSON (ImageContent imgUrl) =
    object
      [ "type" .= ("image_url" :: Text)
      , "image_url" .= imgUrl
      ]

newtype ImageUrl = ImageUrl
  { url :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

newtype OpenRouterResponse = OpenRouterResponse
  { choices :: [Choice]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

newtype Choice = Choice
  { message :: ResponseMessage
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

newtype ResponseMessage = ResponseMessage
  { content :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

analyzeReceiptImage :: ReceiptsReaderConfig -> Text -> IO (Either Text ParsedReceipt)
analyzeReceiptImage config base64Image =
  analyzeReceiptFromMessage config $ ImageContent (ImageUrl $ "data:" <> base64Image)

analyzeReceiptText :: ReceiptsReaderConfig -> Text -> IO (Either Text ParsedReceipt)
analyzeReceiptText config base64Image = runExceptT $ do
  ocrText <- ExceptT $ liftIO $ extractTextFromImage base64Image

  ExceptT $ analyzeReceiptFromMessage config $ TextContent ocrText

analyzeReceiptFromMessage :: ReceiptsReaderConfig -> MessageContent -> IO (Either Text ParsedReceipt)
analyzeReceiptFromMessage config msg =
  callOpenRouterJson config systemPrompt [msg]
  where
    systemPrompt =
      [__i|
      You are a receipt parser. Analyze receipt text extracted via OCR and extract the items in JSON format.
      Return EXACTLY this JSON structure with the extracted data:

      #{encode $ ParsedReceipt [ParsedReceiptItem "food" 10.50 1]}

      CRITICAL INSTRUCTIONS:
      - Sometimes the reciept mentions both total price and unitary price for an item, make sure to always pick the total
      - lugaresDespuesDeLaComa must always be 2 for currency
      - cantidad is the quantity of each item (default to 1 if not specified)
      - Extract ALL items from the receipt
      - Return ONLY the raw JSON object starting with { and ending with }
      - DO NOT wrap the JSON in markdown code blocks
      - DO NOT include ```json or ``` markers
      - DO NOT add any explanatory text before or after the JSON
      - Your entire response must be valid, parseable JSON
      - If you encounter a problem with the parsing just return a plain text BRIEF explanation for the enduser without json at all
      - The error text MUST be in spanish always
      |]

-- | Parse a single pago out of an inbound email. Text-only for now (subject +
-- body); image attachments are added in a later step. Returns 'Left' with a
-- (Spanish) explanation when the model can't confidently parse the pago.
analyzePagoFromEmail ::
  ReceiptsReaderConfig ->
  EmailPagoContext ->
  -- | Email subject
  Text ->
  -- | Email body (plaintext)
  Text ->
  IO (Either Text ParsedEmailPago)
analyzePagoFromEmail config context subject body =
  callOpenRouterJson config systemPrompt [TextContent userText]
  where
    contextJson = Text.decodeUtf8 $ toS $ encode context
    exampleJson = Text.decodeUtf8 $ toS $ encode exampleParsedEmailPago
    monedasList = Text.intercalate ", " context.monedasPermitidas
    defaultMoneda = context.monedaPorDefecto
    userText = "Asunto: " <> subject <> "\n\n" <> body
    systemPrompt =
      [__i|
      You parse a single shared expense ("pago") out of an email for a shared-expenses app, and return it as JSON.

      You are given exactly ONE grupo and its participantes. Map the people mentioned in the email to these participante ids. You MUST only ever use ids from this list:

      #{contextJson}

      The email was written by the participante marked "esRemitente": true (if any). Resolve every first-person reference in the email ("yo", "pagué", "me deben", "I paid", "I'm owed") to THAT participante's id.

      Return EXACTLY this JSON shape, with your extracted data:

      #{exampleJson}

      Always make a best-effort attempt and RETURN THE JSON even if the email is vague, incomplete or a bit inconsistent (amounts that don't quite add up, only one side mentioned, people you can't fully identify). Do NOT refuse or ask for clarification: fill in what you can and leave the rest as sensible defaults (null fecha, default moneda, an empty "partes"/"items" list, or a best guess). A partial pago is expected and useful — the user reviews and fixes it afterwards. Only return a plain-text error for a message that is clearly NOT about a shared expense at all.

      Field meaning:
      - nombre: a short human description of the expense.
      - monto: the total amount, a number with 2 decimals.
      - moneda: one of [#{monedasList}]; if unsure use "#{defaultMoneda}".
      - fecha: ISO date YYYY-MM-DD; if unknown use null.
      - pagadores: who PAID. deudores: who OWES / benefited from the expense.
      - pagadores and deudores are each a distribution object, and you pick ONE of these two shapes for each based on what the email describes:
        - Parts split: {"tipo": "partes", "partes": [{ "participanteId", "monto", "partes" }, ...]}. For each person give EITHER a fixed "monto" (their exact share, null "partes") OR an integer "partes" weight (null "monto"), never both. For an even split give each person "partes": 1 and null "monto". The montos of a "partes" pagadores list must add up to the total monto.
        - Itemized repartija: {"tipo": "repartija", "items": [{ "nombre", "monto", "cantidad" }, ...]}. Use this when the email itemizes the individual products bought (like a receipt). List each product with its TOTAL price and its quantity; the item montos MUST add up to the total monto. Do NOT assign people to items here — consumers claim items separately later.
      - Prefer "partes" for pagadores; use "repartija" (usually for deudores) only when the email actually lists individual products/items.

      CRITICAL INSTRUCTIONS:
      - Every participanteId in a "partes" split MUST be exactly one of the ids listed above. Never invent ids or names.
      - Return ONLY the raw JSON object starting with { and ending with }.
      - DO NOT wrap the JSON in markdown code blocks or include ```json markers.
      - DO NOT add any explanatory text before or after the JSON.
      - Missing amounts, unknown people or ambiguity are NOT reasons to refuse — fill in defaults and still return the JSON. Only return a plain-text explanation (no JSON at all) when the email is clearly not about a shared expense.
      - Any such error text MUST be in spanish always.
      |]

-- | A worked example handed to the model so it sees the exact JSON shape.
exampleParsedEmailPago :: ParsedEmailPago
exampleParsedEmailPago =
  ParsedEmailPago
    { nombre = "Cena"
    , monto = 1000.00
    , moneda = Just "ARS"
    , fecha = Just "2026-01-15"
    , pagadores =
        ParsedPartes [ParsedShare "01ARZ3NDEKTSV4RRFFQ69G5FAV" (Just 1000.00) Nothing]
    , deudores =
        ParsedRepartija
          [ ParsedReceiptItem "Pizza" 800.00 1
          , ParsedReceiptItem "Vino" 200.00 1
          ]
    }

-- | Shared OpenRouter round-trip: send a system prompt plus user message parts,
-- expect the model to answer with a single raw JSON object, and decode it. A
-- plain-text (non-JSON) answer is surfaced as 'Left' — that is how the prompts
-- ask the model to report a problem it can't resolve.
callOpenRouterJson ::
  (FromJSON a) =>
  ReceiptsReaderConfig ->
  Text ->
  [MessageContent] ->
  IO (Either Text a)
callOpenRouterJson config systemPrompt userContent =
  runReq defaultHttpConfig{httpConfigCheckResponse = \_ _ _ -> Nothing} $ runExceptT $ do
    (model, fallbackModels) <- case config.models of
      [] -> throwError "Reading receipts is not properly configured. Talk to an admin."
      (model : fallback) -> pure (model, fallback)
    let requestBody =
          OpenRouterRequest
            { model = model
            , models = fallbackModels
            , messages =
                [ OpenRouterMessage
                    { role = "system"
                    , content = [TextContent systemPrompt]
                    }
                , OpenRouterMessage
                    { role = "user"
                    , content = userContent
                    }
                ]
            }
    liftIO $ putText $ "[openrouter] request " <> Text.decodeUtf8 (toS (encode requestBody))
    response <-
      req
        POST
        (https "openrouter.ai" /: "api" /: "v1" /: "chat" /: "completions")
        (ReqBodyJson requestBody)
        bsResponse
        ( mconcat
            [ header "Authorization" ("Bearer " <> Text.encodeUtf8 config.apiKey)
            , header "HTTP-Referer" "https://split.ludat.io"
            , header "X-Title" "Banana Split"
            ]
        )
    liftIO $ putText $ "[openrouter] response " <> Text.decodeUtf8 (responseBody response)

    openRouterResp <- liftEither $ left Text.pack $ eitherDecodeStrict @OpenRouterResponse $ responseBody response

    Choice (ResponseMessage contentText) <- case openRouterResp.choices of
      [] -> throwError "No response from OpenRouter"
      (c : _) -> pure c

    let trimmed = Text.strip contentText
    -- The prompts tell the model to answer with a bare JSON object, or a
    -- plain-text explanation when it can't. Treat a non-@{@ answer as the latter.
    if not (Text.isPrefixOf "{" trimmed)
      then throwError trimmed
      else do
        let cleanedText =
              contentText
                & Text.dropWhile (/= '{')
                & Text.dropWhileEnd (/= '}')
        liftEither $ left Text.pack $ eitherDecodeStrict $ Text.encodeUtf8 cleanedText
