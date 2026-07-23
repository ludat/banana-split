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
import Preludat
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
import Control.Monad.Error.Class (liftEither)

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
-- payload is ignored. Fields Maileroo always sends are parsed as required, so a
-- malformed/truncated payload fails loudly at decode time (servant answers 400,
-- the handler never runs) rather than degrading to a default that fails later.
data MailerooInbound = MailerooInbound
  { envelopeSender :: Text
  , recipients :: [Text]
  , headers :: Map Text [Text]
  -- ^ Header name -> values (e.g. @"From"@, @"To"@, @"Subject"@).
  , plaintext :: Maybe Text
  -- ^ Nested under @body@; absent for an HTML-only message, hence 'Maybe'.
  , strippedPlaintext :: Maybe Text
  -- ^ Nested under @body@; absent for an HTML-only message, hence 'Maybe'.
  , isSpam :: Bool
  , isDmarcAligned :: Bool
  , spfResult :: Text
  -- ^ A status string like @"pass"@.
  , dkimResult :: Bool
  -- ^ A plain JSON boolean (@true@/@false@), unlike 'spfResult'.
  , validationUrl :: Text
  , attachments :: [MailerooAttachment]
  }
  deriving (Show, Eq, Generic)

instance FromJSON MailerooInbound where
  parseJSON = withObject "MailerooInbound" $ \o -> do
    -- The message bodies live in a nested @body@ object; everything else is top
    -- level.
    body <- o .: "body"
    MailerooInbound
      <$> o .: "envelope_sender"
      <*> o .: "recipients"
      <*> o .: "headers"
      <*> body .:? "plaintext"
      <*> body .:? "stripped_plaintext"
      <*> o .: "is_spam"
      <*> o .: "is_dmarc_aligned"
      <*> o .: "spf_result"
      <*> o .: "dkim_result"
      <*> o .: "validation_url"
      -- @attachments@ is always present but null when there are none.
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
      <$> o .: "filename"
      <*> o .: "content_type"
      <*> o .: "url"
      <*> o .: "size"

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
  let spfPass = payload.spfResult == "pass"
  unless (payload.isDmarcAligned || (spfPass && payload.dkimResult)) $
    throwError "message failed sender authentication (not DMARC-aligned, and SPF+DKIM did not both pass)"

  -- 3. Resolve the From address to a user.
  fromEmail <- either throwError pure $ extractFromEmail payload
  maybeUser <- lift $ runBeam $ fetchUserByEmail fromEmail
  user <-
    maybe (throwError $ "no account for address " <> unEmail fromEmail) pure maybeUser

  -- 4. Authorize the grupo: the +tag grupoId must be one of this user's grupos.
  grupoId <- liftEither $ extractGrupoId payload
  grupos <- lift $ runBeam $ fetchGruposForUser user.id
  grupo <-
    pure (find (\g -> g.id == grupoId) grupos)
    `orElseMay`
      (throwError $ "grupo " <> show grupoId <> " is not one of " <> unEmail user.email <> "'s grupos")

  -- 5. Ask the AI to parse exactly one pago within this grupo (text only for
  -- now). A 'Left' here is the model's own (Spanish) explanation of why it
  -- couldn't parse the message.
  let subject = fromMaybe "" (firstHeader "Subject" payload.headers)
  parsed <-
    liftIO (analyzePagoFromEmail config (mkPagoContext user grupo) subject (bodyText payload))
      `orElse` (\err -> throwError $ "AI could not parse a pago: " <> err)

  -- 6. Resolve the model's output into a real Pago. This is deliberately
  -- permissive: we would rather save a partial pago (which savePago will flag as
  -- invalid for the user to fix) than reject the whole message over an
  -- unrecognised person, currency or date. 7. Persist it.
  today <- liftIO $ utctDay <$> getCurrentTime
  let pago = resolvePago grupo today parsed
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
-- nombre), the default currency, and the set of accepted currency codes. The
-- participante linked to @sender@ is flagged @esRemitente@ so the model can
-- resolve first-person references in the email to a real id.
mkPagoContext :: User -> ShallowGrupo -> EmailPagoContext
mkPagoContext sender grupo =
  EmailPagoContext
    { grupoNombre = grupo.nombre
    , monedaPorDefecto = show grupo.monedaPorDefecto
    , monedasPermitidas = fmap show todasLasMonedas
    , participantes =
        grupo.participantes
          <&> \p ->
            EmailPagoParticipante
              { id = show p.id
              , nombre = p.nombre
              , esRemitente = fmap (.id) p.user == Just sender.id
              }
    }

-- | Turn the AI's parsed pago into a real 'Pago'. This never fails: anything the
-- model got wrong (an unknown currency or date, a person that isn't in the
-- grupo) is dropped rather than rejected, so we always produce /some/ pago. It
-- is born invalid ('isValid' is recomputed by savePago), so a partial result
-- simply surfaces to the user as an invalid pago to finish editing.
resolvePago :: ShallowGrupo -> Day -> ParsedEmailPago -> Pago
resolvePago grupo today parsed =
  let validIds = Set.fromList $ fmap (.id) grupo.participantes
  in Pago
      { pagoId = nullUlid
      , monto = scientificToMonto parsed.monto
      , moneda = resolveMoneda grupo.monedaPorDefecto parsed.moneda
      , isValid = False -- recomputed by savePago via addIsValidPago
      , nombre = parsed.nombre
      , fecha = resolveFecha today parsed.fecha
      , pagadores = resolveDistribucion parsed.nombre validIds parsed.pagadores
      , deudores = resolveDistribucion parsed.nombre validIds parsed.deudores
      }

-- | Resolve the currency, falling back to the grupo default for anything blank
-- or unrecognised.
resolveMoneda :: Moneda -> Maybe Text -> Moneda
resolveMoneda def raw =
  fromMaybe def $ do
    code <- Text.strip <$> raw
    guard (not (Text.null code))
    readMaybe (Text.toUpper code)

-- | Resolve the date, falling back to today for anything blank or not ISO
-- @YYYY-MM-DD@.
resolveFecha :: Day -> Maybe Text -> Day
resolveFecha today raw =
  fromMaybe today $ do
    day <- Text.strip <$> raw
    guard (not (Text.null day))
    parseTimeM True defaultTimeLocale "%Y-%m-%d" (toS day)

-- | Build one side's 'Distribucion' from whichever shape the AI chose: a parts
-- split (participante ids validated against the grupo) or an itemized repartija
-- (items only, no claims — like the receipt parse, so the pago stays invalid
-- until items are claimed). @nombre@ seeds the repartija's name. Shares for
-- unknown participantes are silently dropped, so the resulting distribución may
-- end up empty — that is fine, the pago is just invalid until fixed.
resolveDistribucion :: Text -> Set ULID -> ParsedDistribucion -> Distribucion
resolveDistribucion nombre validIds parsed =
  case parsed of
    ParsedPartes shares -> resolvePartes validIds shares
    ParsedRepartija items -> resolveRepartija nombre items

resolvePartes :: Set ULID -> [ParsedShare] -> Distribucion
resolvePartes validIds shares =
  Distribucion
    { id = nullUlid
    , tipo = TipoDistribucionPartes (DistribucionPartes nullUlid (mapMaybe (resolveShare validIds) shares))
    }

resolveRepartija :: Text -> [ParsedReceiptItem] -> Distribucion
resolveRepartija nombre items =
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

-- | Resolve a single share to a 'Parte', or 'Nothing' if its participante id is
-- malformed or not a member of the grupo (such shares are dropped).
resolveShare :: Set ULID -> ParsedShare -> Maybe Parte
resolveShare validIds share = do
  ulid <- readMaybe share.participanteId
  guard (ulid `Set.member` validIds)
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
  fromMaybe "" payload.plaintext

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
