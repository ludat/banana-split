module BananaSplit.Persistence.GrupoSpec
    ( spec
    ) where

import BananaSplit.Core
import BananaSplit.Participante
import BananaSplit.Persistence
import BananaSplit.Persistence.SpecHook

import Protolude

import Test.Hspec

spec :: SpecWith RunDb
spec =
  describe "createGrupo" $ do
    it "creates a grupo and can fetch it back" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Test Grupo" "alguien"

      maybeGrupo <- runDb $ fetchGrupo grupo.id

      case maybeGrupo of
        Nothing -> expectationFailure "Grupo should exist"
        Just fetchedGrupo -> do
          fetchedGrupo.nombre `shouldBe` "Test Grupo"
          fetchedGrupo.id `shouldBe` grupo.id
          (fetchedGrupo.participantes & fmap (.participanteNombre))
            `shouldBe` ["alguien"]
