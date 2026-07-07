module BananaSplit.Persistence.GrupoSpec (
  spec,
) where

import Protolude
import Test.Hspec

import BananaSplit.Core
import BananaSplit.Participante
import BananaSplit.Persistence
import BananaSplit.Persistence.SpecHook
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
      user <- runDb $ findOrCreateUser "yo@example.com"
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

    it "returns nothing for a user without claims" $ \(RunDb runDb) -> do
      user <- runDb $ findOrCreateUser "nadie@example.com"
      _grupo <- runDb $ createGrupo "Sin claim" "otre"

      grupos <- runDb $ fetchGruposForUser user.id
      grupos `shouldBe` []
