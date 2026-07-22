{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Inbound-email webhook (Maileroo): turn an email into a 'Pago'.
--
-- This is the provider-facing endpoint Maileroo POSTs to when a message lands
-- on @pagos+\<grupoId\>@\<domain\>@. It is unauthenticated in the servant sense
-- (Maileroo can't send our session cookie); its trust comes entirely from the
-- guards below, in order:
--
--   1. Round-trip the payload's @validation_url@ (proves the POST really came
--      from Maileroo — it only answers @success:true@ once).
--   2. Reject spam and messages that fail sender authentication (DMARC, or
--      SPF+DKIM). Since the sender's From address is the /only/ thing tying the
--      message to an account, this is what stops spoofing.
--   3. Resolve the From address to a user.
--   4. Take the grupoId from the recipient's @+@ tag and require it to be one of
--      /that user's/ grupos.
--
-- This module implements rollout steps 1 and 2: the authorization gate, then a
-- text-only AI parse of the pago, resolved against the grupo and saved with
-- 'savePago'. Image attachments and confirmation emails land in later steps.
module Site.Handler.InboundEmail (
  WebhookApi,
  MailerooInbound (..),
  MailerooAttachment (..),
  handleInboundEmail,
  -- * Pure helpers (exported for testing)
  extractFromEmail,
  extractGrupoId,
  resolvePago,
) where

import Data.Aeson (FromJSON (..), eitherDecode, withObject, (.!=), (.:), (.:?))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time (Day, defaultTimeLocale, getCurrentTime, parseTimeM, utctDay)
import Network.HTTP.Client (Manager, httpLbs, parseRequest, responseBody)
import Protolude
import Servant

import BananaSplit
import BananaSplit.Persistence (fetchGruposForUser, fetchUserByEmail, savePago)
import BananaSplit.Receipts (
  EmailPagoContext (..),
  EmailPagoParticipante (..),
  ParsedDistribucion (..),
  ParsedEmailPago (..),
  ParsedReceiptItem (..),
  ParsedShare (..),
  ReceiptsReaderConfig (..),
  analyzePagoFromEmail,
 )
import Site.Handler.Utils (runBeam)
import Site.Types

-- | The provider-facing API, deliberately kept /out/ of 'Site.Api.Api' so it is
-- not walked by the servant-elm code generator (the frontend never calls it).
-- It is mounted under @\/api@ next to the main API in "Site.Server".
type WebhookApi =
  "webhooks"
    :> "email"
    :> "inbound"
    :> ReqBody '[JSON] MailerooInbound
    :> Post '[JSON] NoContent

-- | The subset of Maileroo's inbound JSON we care about. Everything else in the
-- payload is ignored. Auth-relevant booleans fail /closed/ when absent
-- (@is_spam@ defaults to 'True', @is_dmarc_aligned@ to 'False') so a truncated
-- or unexpected payload can never be mistaken for an authenticated message.
data MailerooInbound = MailerooInbound
  { envelopeSender :: Text
  , recipients :: [Text]
  , headers :: Map Text [Text]
  -- ^ Header name -> values (e.g. @"From"@, @"To"@, @"Subject"@).
  , plaintext :: Maybe Text
  , strippedPlaintext :: Maybe Text
  , isSpam :: Bool
  , isDmarcAligned :: Bool
  , spfResult :: Maybe Text
  , dkimResult :: Maybe Text
  , validationUrl :: Text
  , attachments :: [MailerooAttachment]
  }
  deriving (Show, Eq, Generic)

instance FromJSON MailerooInbound where
  parseJSON = withObject "MailerooInbound" $ \o ->
    MailerooInbound
      <$> o .:? "envelope_sender" .!= ""
      <*> o .:? "recipients" .!= []
      <*> o .:? "headers" .!= mempty
      <*> o .:? "plaintext"
      <*> o .:? "stripped_plaintext"
      <*> o .:? "is_spam" .!= True
      <*> o .:? "is_dmarc_aligned" .!= False
      <*> o .:? "spf_result"
      <*> o .:? "dkim_result"
      <*> o .: "validation_url"
      <*> o .:? "attachments" .!= []

data MailerooAttachment = MailerooAttachment
  { filename :: Text
  , contentType :: Text
  , url :: Text
  , size :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON MailerooAttachment where
  parseJSON = withObject "MailerooAttachment" $ \o ->
    MailerooAttachment
      <$> o .:? "filename" .!= ""
      <*> o .:? "content_type" .!= ""
      <*> o .:? "url" .!= ""
      <*> o .:? "size" .!= 0

-- | Maileroo's @validation_url@ answers with this the first (and only) time it
-- is hit for a given message.
newtype MailerooValidation = MailerooValidation
  { success :: Bool
  }
  deriving (Show, Generic)

instance FromJSON MailerooValidation where
  parseJSON = withObject "MailerooValidation" $ \o ->
    MailerooValidation <$> o .:? "success" .!= False

handleInboundEmail :: MailerooInbound -> AppHandler NoContent
handleInboundEmail payload = do
  outcome <- runExceptT $ processInbound payload
  case outcome of
    Left reason -> putText $ "[inbound-email] dropped: " <> reason
    Right report -> putText $ "[inbound-email] " <> report
  -- Always 200: never surface processing failures to the provider, so Maileroo
  -- doesn't retry-storm. User-facing failures will go out as email later.
  pure NoContent

-- | The guard-then-parse pipeline. A 'Left' is a reason we dropped the message
-- (logged, and later emailed back); a 'Right' describes the pago we saved.
processInbound :: MailerooInbound -> ExceptT Text AppHandler Text
processInbound payload = do
  -- 1. Prove the POST came from Maileroo.
  config <- lift $ asks (.receipts)
  validated <- liftIO $ validateWebhook config.manager payload.validationUrl
  unless validated $
    throwError "validation_url did not confirm the webhook (success was not true)"

  -- 2. Reject spam / unauthenticated senders. This is the anti-spoofing gate.
  when payload.isSpam $
    throwError "message was flagged as spam"
  let spfPass = payload.spfResult == Just "pass"
      dkimPass = payload.dkimResult == Just "pass"
  unless (payload.isDmarcAligned || (spfPass && dkimPass)) $
    throwError "message failed sender authentication (not DMARC-aligned, and SPF+DKIM did not both pass)"

  -- 3. Resolve the From address to a user.
  fromEmail <- either throwError pure $ extractFromEmail payload
  maybeUser <- lift $ runBeam $ fetchUserByEmail fromEmail
  user <-
    maybe (throwError $ "no account for address " <> unEmail fromEmail) pure maybeUser

  -- 4. Authorize the grupo: the +tag grupoId must be one of this user's grupos.
  grupoId <- either throwError pure $ extractGrupoId payload
  grupos <- lift $ runBeam $ fetchGruposForUser user.id
  grupo <-
    maybe
      (throwError $ "grupo " <> show grupoId <> " is not one of " <> unEmail user.email <> "'s grupos")
      pure
      (find (\g -> g.id == grupoId) grupos)

  -- 5. Ask the AI to parse exactly one pago within this grupo (text only for
  -- now). A 'Left' here is the model's own (Spanish) explanation of why it
  -- couldn't parse the message.
  let subject = fromMaybe "" (firstHeader "Subject" payload.headers)
  parsedResult <-
    liftIO $ analyzePagoFromEmail config (mkPagoContext grupo) subject (bodyText payload)
  parsed <- either (\err -> throwError $ "AI could not parse a pago: " <> err) pure parsedResult

  -- 6. Resolve the model's output into a real Pago, validating every referenced
  -- participante against the grupo. 7. Persist it.
  today <- liftIO $ utctDay <$> getCurrentTime
  pago <- either throwError pure $ resolvePago grupo today parsed
  saved <- lift $ runBeam $ savePago grupo.id pago

  pure $
    "saved pago "
      <> show saved.pagoId
      <> " (\""
      <> saved.nombre
      <> "\", "
      <> monto2Text saved.monto
      <> " "
      <> show saved.moneda
      <> (if saved.isValid then "" else ", INVALID")
      <> ") for "
      <> unEmail user.email
      <> " in grupo \""
      <> grupo.nombre
      <> "\""

-- | Build the compact grupo context the AI needs: its participantes (id +
-- nombre), the default currency, and the set of accepted currency codes.
mkPagoContext :: ShallowGrupo -> EmailPagoContext
mkPagoContext grupo =
  EmailPagoContext
    { grupoNombre = grupo.nombre
    , monedaPorDefecto = show grupo.monedaPorDefecto
    , monedasPermitidas = fmap show todasLasMonedas
    , participantes =
        grupo.participantes
          <&> \p -> EmailPagoParticipante{id = show p.id, nombre = p.nombre}
    }

-- | Turn the AI's parsed pago into a real 'Pago', or explain why it can't.
-- Every referenced participante id must resolve to a real participante of the
-- grupo; the currency and date fall back to the grupo default / today.
resolvePago :: ShallowGrupo -> Day -> ParsedEmailPago -> Either Text Pago
resolvePago grupo today parsed = do
  let validIds = Set.fromList $ fmap (.id) grupo.participantes
  moneda <- resolveMoneda grupo.monedaPorDefecto parsed.moneda
  fecha <- resolveFecha today parsed.fecha
  pagadores <- resolveDistribucion "pagadores" parsed.nombre validIds parsed.pagadores
  deudores <- resolveDistribucion "deudores" parsed.nombre validIds parsed.deudores
  pure
    Pago
      { pagoId = nullUlid
      , monto = scientificToMonto parsed.monto
      , moneda = moneda
      , isValid = False -- recomputed by savePago via addIsValidPago
      , nombre = parsed.nombre
      , fecha = fecha
      , pagadores = pagadores
      , deudores = deudores
      }

resolveMoneda :: Moneda -> Maybe Text -> Either Text Moneda
resolveMoneda def Nothing = Right def
resolveMoneda def (Just raw)
  | Text.null (Text.strip raw) = Right def
  | otherwise =
      maybe (Left $ "unknown moneda: " <> raw) Right $
        readMaybe (Text.toUpper $ Text.strip raw)

resolveFecha :: Day -> Maybe Text -> Either Text Day
resolveFecha today Nothing = Right today
resolveFecha today (Just raw)
  | Text.null (Text.strip raw) = Right today
  | otherwise =
      maybe (Left $ "invalid fecha (expected ISO YYYY-MM-DD): " <> raw) Right $
        parseTimeM True defaultTimeLocale "%Y-%m-%d" (toS $ Text.strip raw)

-- | Build one side's 'Distribucion' from whichever shape the AI chose: a parts
-- split (participante ids validated against the grupo) or an itemized repartija
-- (items only, no claims — like the receipt parse, so the pago stays invalid
-- until items are claimed). @side@ names the side for error messages, and
-- @nombre@ seeds the repartija's name.
resolveDistribucion :: Text -> Text -> Set ULID -> ParsedDistribucion -> Either Text Distribucion
resolveDistribucion side nombre validIds parsed =
  case parsed of
    ParsedPartes shares -> resolvePartes side validIds shares
    ParsedRepartija items -> resolveRepartija side nombre items

resolvePartes :: Text -> Set ULID -> [ParsedShare] -> Either Text Distribucion
resolvePartes side _ [] = Left $ "the AI returned no " <> side
resolvePartes _ validIds shares = do
  partes <- traverse (resolveShare validIds) shares
  pure
    Distribucion
      { id = nullUlid
      , tipo = TipoDistribucionPartes (DistribucionPartes nullUlid partes)
      }

resolveRepartija :: Text -> Text -> [ParsedReceiptItem] -> Either Text Distribucion
resolveRepartija side _ [] = Left $ "the AI returned an itemized " <> side <> " with no items"
resolveRepartija _ nombre items =
  Right
    Distribucion
      { id = nullUlid
      , tipo =
          TipoDistribucionRepartija
            Repartija
              { id = nullUlid
              , nombre = nombre
              , extra = 0
              , distribucionDeSobras = SobrasNoDistribuir
              , items =
                  items
                    <&> \item ->
                      RepartijaItem
                        { id = nullUlid
                        , nombre = item.nombre
                        , monto = scientificToMonto item.monto
                        , cantidad = item.cantidad
                        }
              , claims = [] -- filled in later when consumers claim items
              }
      }

resolveShare :: Set ULID -> ParsedShare -> Either Text Parte
resolveShare validIds share = do
  ulid <-
    maybe (Left $ "invalid participante id: " <> share.participanteId) Right $
      readMaybe share.participanteId
  unless (ulid `Set.member` validIds) $
    Left $ "participante " <> share.participanteId <> " is not in the grupo"
  let pid = ParticipanteId ulid
  pure $ case (share.monto, share.partes) of
    (Just m, _) -> MontoFijo (scientificToMonto m) pid
    (Nothing, Just n) -> Ponderado n pid
    (Nothing, Nothing) -> Ponderado 1 pid

-- | Prefer the @From@ header address, falling back to the SMTP envelope sender.
extractFromEmail :: MailerooInbound -> Either Text Email
extractFromEmail payload =
  let raw = fromMaybe payload.envelopeSender (firstHeader "From" payload.headers)
      address = addressFromHeader raw
  in if Text.null address
       then Left "could not determine a From address"
       else Right (mkEmail address)

-- | Pull the grupoId from the @+@ tag of a recipient. Considers both the
-- @recipients@ list and the @To@ header, taking the first that yields a valid
-- ULID.
extractGrupoId :: MailerooInbound -> Either Text ULID
extractGrupoId payload =
  let candidates = payload.recipients <> concat (maybeToList (Map.lookup "to" (ciHeaders payload.headers)))
      tags = mapMaybe (grupoIdFromAddress . addressFromHeader) candidates
  in case tags of
       (grupoId : _) -> Right grupoId
       [] -> Left "no recipient carried a valid +grupoId tag"

-- | Extract the ULID from an address' @+@ tag, e.g.
-- @pagos+01ABC...@split.ludat.io@ -> the ULID @01ABC...@.
grupoIdFromAddress :: Text -> Maybe ULID
grupoIdFromAddress address = do
  let localPart = Text.takeWhile (/= '@') address
  guard (Text.isInfixOf "+" localPart)
  let tag = localPart & Text.dropWhile (/= '+') & Text.drop 1
  readMaybe tag

-- | Reduce a raw header value like @Lucas \<lucas\@example.com\>@ to just the
-- bare address; a header that is already bare passes through untouched.
addressFromHeader :: Text -> Text
addressFromHeader raw =
  case Text.breakOn "<" raw of
    (_, rest)
      | not (Text.null rest) ->
          rest & Text.drop 1 & Text.takeWhile (/= '>') & Text.strip
    _ -> Text.strip raw

-- | First value of a header, looked up case-insensitively.
firstHeader :: Text -> Map Text [Text] -> Maybe Text
firstHeader name hs = Map.lookup (Text.toLower name) (ciHeaders hs) >>= head

-- | Re-key a header map by lowercased name for case-insensitive lookups. Header
-- names are case-insensitive per RFC 5322, and Maileroo's casing isn't
-- guaranteed.
ciHeaders :: Map Text [Text] -> Map Text [Text]
ciHeaders = Map.mapKeys Text.toLower

bodyText :: MailerooInbound -> Text
bodyText payload =
  fromMaybe "" (payload.strippedPlaintext <|> payload.plaintext)

-- | Hit @validation_url@ once and return whether it confirmed the webhook. Any
-- network/parse failure is treated as "not validated" (fail closed).
validateWebhook :: Manager -> Text -> IO Bool
validateWebhook manager url = do
  result <- try $ do
    request <- parseRequest (toS url)
    response <- httpLbs request manager
    pure $ eitherDecode @MailerooValidation (responseBody response)
  pure $ case result of
    Left (_ :: SomeException) -> False
    Right (Left _) -> False
    Right (Right validation) -> validation.success
