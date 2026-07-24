{-# LANGUAGE OverloadedStrings #-}

module Site.Handler.InboundEmailSpec (
  spec,
) where

import Data.Map.Strict qualified as Map
import Data.Time (Day, fromGregorian)
import Protolude
import Test.Hspec

import BananaSplit (
  Distribucion (..),
  DistribucionPartes (..),
  Moneda (..),
  Pago (..),
  Parte (..),
  Participante (..),
  ParticipanteId (..),
  Repartija (..),
  RepartijaItem (..),
  ShallowGrupo (..),
  TipoDistribucion (..),
  ULID,
  mkEmail,
  nullUlid,
  scientificToMonto,
  unEmail,
 )
import BananaSplit.Receipts (
  ParsedDistribucion (..),
  ParsedEmailPago (..),
  ParsedReceiptItem (..),
  ParsedShare (..),
 )
import Site.Handler.InboundEmail (
  MailerooInbound (..),
  extractFromEmail,
  extractGrupoId,
  resolvePago,
 )

-- | A real, well-formed ULID used as the grupoId in the +tag.
grupoUlid :: Text
grupoUlid = "01ARZ3NDEKTSV4RRFFQ69G5FAV"

-- | An inbound payload with empty/benign defaults; tests override just the
-- fields they exercise.
baseInbound :: MailerooInbound
baseInbound =
  MailerooInbound
    { envelopeSender = ""
    , recipients = []
    , headers = Map.empty
    , plaintext = Nothing
    , strippedPlaintext = Nothing
    , isSpam = False
    , isDmarcAligned = True
    , spfResult = "pass"
    , dkimResult = True
    , validationUrl = ""
    , attachments = []
    }

spec :: Spec
spec = do
  describe "extractFromEmail" $ do
    it "reads a display-name From header" $ do
      let payload =
            baseInbound{headers = Map.fromList [("From", ["Lucas <lucas@example.com>"])]}
      fmap unEmail (extractFromEmail payload) `shouldBe` Right "lucas@example.com"

    it "reads a bare From header" $ do
      let payload =
            baseInbound{headers = Map.fromList [("From", ["lucas@example.com"])]}
      extractFromEmail payload `shouldBe` Right (mkEmail "lucas@example.com")

    it "looks up the header case-insensitively" $ do
      let payload =
            baseInbound{headers = Map.fromList [("from", ["lucas@example.com"])]}
      extractFromEmail payload `shouldBe` Right (mkEmail "lucas@example.com")

    it "normalises the address (lowercased, trimmed) via mkEmail" $ do
      let payload =
            baseInbound{headers = Map.fromList [("From", ["  Lucas <Lucas@Example.COM>  "])]}
      fmap unEmail (extractFromEmail payload) `shouldBe` Right "lucas@example.com"

    it "falls back to the envelope sender when there is no From header" $ do
      let payload = baseInbound{envelopeSender = "bounce@example.com"}
      extractFromEmail payload `shouldBe` Right (mkEmail "bounce@example.com")

    it "prefers the From header over the envelope sender" $ do
      let payload =
            baseInbound
              { envelopeSender = "bounce@example.com"
              , headers = Map.fromList [("From", ["real@example.com"])]
              }
      extractFromEmail payload `shouldBe` Right (mkEmail "real@example.com")

    it "fails when neither a From header nor an envelope sender is present" $ do
      extractFromEmail baseInbound `shouldSatisfy` isLeft

  describe "extractGrupoId" $ do
    it "reads the grupoId from a recipient's +tag" $ do
      let payload =
            baseInbound{recipients = ["pagos+" <> grupoUlid <> "@split.ludat.io"]}
      fmap show (extractGrupoId payload) `shouldBe` Right grupoUlid

    it "reads the grupoId from the To header when recipients is empty" $ do
      let payload =
            baseInbound
              { headers =
                  Map.fromList
                    [("To", ["Pagos <pagos+" <> grupoUlid <> "@split.ludat.io>"])]
              }
      fmap show (extractGrupoId payload) `shouldBe` Right grupoUlid

    it "fails when no recipient carries a +tag" $ do
      let payload = baseInbound{recipients = ["pagos@split.ludat.io"]}
      extractGrupoId payload `shouldSatisfy` isLeft

    it "fails when the +tag is not a valid ULID" $ do
      let payload = baseInbound{recipients = ["pagos+not-a-ulid@split.ludat.io"]}
      extractGrupoId payload `shouldSatisfy` isLeft

    it "skips a tagless recipient and uses the tagged one" $ do
      let payload =
            baseInbound
              { recipients =
                  [ "someone@example.com"
                  , "pagos+" <> grupoUlid <> "@split.ludat.io"
                  ]
              }
      fmap show (extractGrupoId payload) `shouldBe` Right grupoUlid

  describe "resolvePago" $ do
    it "resolves a valid pago (fixed payer, evenly-split debtors)" $ do
      let pago = resolvePago testGrupo today baseParsed
      pago.nombre `shouldBe` "Cena"
      pago.monto `shouldBe` scientificToMonto 1000
      pago.moneda `shouldBe` USD
      pago.fecha `shouldBe` fromGregorian 2026 1 15
      pago.pagoId `shouldBe` nullUlid
      pago.isValid `shouldBe` False
      partesOf pago.pagadores
        `shouldBe` [MontoFijo (scientificToMonto 1000) (ParticipanteId p1)]
      partesOf pago.deudores
        `shouldBe` [Ponderado 1 (ParticipanteId p1), Ponderado 1 (ParticipanteId p2)]

    it "defaults the moneda to the grupo default when omitted" $ do
      let parsed = baseParsed{moneda = Nothing} :: ParsedEmailPago
      (resolvePago testGrupo today parsed).moneda `shouldBe` ARS

    it "reads a lowercase moneda code" $ do
      let parsed = baseParsed{moneda = Just "usd"} :: ParsedEmailPago
      (resolvePago testGrupo today parsed).moneda `shouldBe` USD

    it "falls back to the grupo default for an unknown moneda" $ do
      let parsed = baseParsed{moneda = Just "XYZ"} :: ParsedEmailPago
      (resolvePago testGrupo today parsed).moneda `shouldBe` ARS

    it "defaults the fecha to today when omitted" $ do
      let parsed = baseParsed{fecha = Nothing} :: ParsedEmailPago
      (resolvePago testGrupo today parsed).fecha `shouldBe` today

    it "falls back to today for a non-ISO fecha" $ do
      let parsed = baseParsed{fecha = Just "15/01/2026"} :: ParsedEmailPago
      (resolvePago testGrupo today parsed).fecha `shouldBe` today

    it "drops a participante that is not in the grupo" $ do
      let parsed = baseParsed{pagadores = [ParsedShare strangerText (Just 1000) Nothing]} :: ParsedEmailPago
      partesOf (resolvePago testGrupo today parsed).pagadores `shouldBe` []

    it "drops a malformed participante id" $ do
      let parsed = baseParsed{pagadores = [ParsedShare "not-a-ulid" (Just 1000) Nothing]} :: ParsedEmailPago
      partesOf (resolvePago testGrupo today parsed).pagadores `shouldBe` []

    it "keeps an empty parts split empty rather than failing" $ do
      let parsed = baseParsed{pagadores = []} :: ParsedEmailPago
      partesOf (resolvePago testGrupo today parsed).pagadores `shouldBe` []

    it "defaults a share with neither monto nor partes to one part" $ do
      let parsed = baseParsed{deudores = ParsedPartes [ParsedShare p1Text Nothing Nothing]} :: ParsedEmailPago
      partesOf (resolvePago testGrupo today parsed).deudores
        `shouldBe` [Ponderado 1 (ParticipanteId p1)]

    it "builds an itemized repartija distribution (items only, no claims)" $ do
      let parsed =
            baseParsed
              { deudores =
                  ParsedRepartija
                    [ ParsedReceiptItem "Pizza" 800 1
                    , ParsedReceiptItem "Vino" 200 1
                    ]
              } ::
              ParsedEmailPago
      case repartijaOf (resolvePago testGrupo today parsed).deudores of
        Nothing -> expectationFailure "expected a repartija distribution"
        Just r -> do
          r.nombre `shouldBe` "Cena"
          r.claims `shouldBe` []
          fmap (.nombre) r.items `shouldBe` ["Pizza", "Vino"]
          fmap (.monto) r.items `shouldBe` [scientificToMonto 800, scientificToMonto 200]
          fmap (.cantidad) r.items `shouldBe` [1, 1]

    it "keeps an itemized side with no items empty rather than failing" $ do
      let parsed = baseParsed{deudores = ParsedRepartija []} :: ParsedEmailPago
      case repartijaOf (resolvePago testGrupo today parsed).deudores of
        Nothing -> expectationFailure "expected a repartija distribution"
        Just r -> r.items `shouldBe` []

-- | Extract the parts of a partes-based distribución.
partesOf :: Distribucion -> [Parte]
partesOf d = case d.tipo of
  TipoDistribucionPartes dp -> dp.partes
  _ -> []

repartijaOf :: Distribucion -> Maybe Repartija
repartijaOf d = case d.tipo of
  TipoDistribucionRepartija r -> Just r
  _ -> Nothing

mkUlid :: Text -> ULID
mkUlid t = fromMaybe (panic $ "bad test ulid: " <> t) (readMaybe t)

p1Text, p2Text, strangerText :: Text
p1Text = "01ARZ3NDEKTSV4RRFFQ69G5FAV"
p2Text = "01BX5ZZKBKACTAV9WEVGEMMVRZ"
strangerText = "01BX5ZZKBKACTAV9WEVGEMMV00"

p1, p2 :: ULID
p1 = mkUlid p1Text
p2 = mkUlid p2Text

today :: Day
today = fromGregorian 2026 7 22

testGrupo :: ShallowGrupo
testGrupo =
  ShallowGrupo
    { id = nullUlid
    , nombre = "Viaje"
    , participantes =
        [ Participante{id = p1, nombre = "Ana", user = Nothing}
        , Participante{id = p2, nombre = "Beto", user = Nothing}
        ]
    , isFrozen = False
    , monedaPorDefecto = ARS
    }

baseParsed :: ParsedEmailPago
baseParsed =
  ParsedEmailPago
    { nombre = "Cena"
    , monto = 1000
    , moneda = Just "USD"
    , fecha = Just "2026-01-15"
    , pagadores = [ParsedShare p1Text (Just 1000) Nothing]
    , deudores =
        ParsedPartes
          [ ParsedShare p1Text Nothing (Just 1)
          , ParsedShare p2Text Nothing (Just 1)
          ]
    }
