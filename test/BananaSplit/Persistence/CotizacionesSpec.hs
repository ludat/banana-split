module BananaSplit.Persistence.CotizacionesSpec (
  spec,
) where

import Protolude
import Test.Hspec

import BananaSplit.Core
import BananaSplit.Moneda
import BananaSplit.Persistence
import BananaSplit.Persistence.SpecHook

spec :: SpecWith RunDb
spec = do
  describe "saveCotizacionesCongeladas" $ do
    it "guarda cotizaciones y las devuelve al leerlas" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Test Grupo" "alguien"
      let cotizaciones = 1200 `enMoneda` USD <> 1300 `enMoneda` EUR

      runDb $ saveCotizacionesCongeladas grupo.id cotizaciones

      fetched <- runDb $ fetchCotizacionesCongeladas grupo.id
      fetched `shouldBe` cotizaciones

    it "reemplaza las cotizaciones anteriores al volver a guardar" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Test Grupo" "alguien"
      runDb $ saveCotizacionesCongeladas grupo.id (1200 `enMoneda` USD)

      runDb $ saveCotizacionesCongeladas grupo.id (1500 `enMoneda` USD)

      fetched <- runDb $ fetchCotizacionesCongeladas grupo.id
      fetched `shouldBe` 1500 `enMoneda` USD

    it "devuelve vacío para un grupo sin cotizaciones" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Test Grupo" "alguien"

      fetched <- runDb $ fetchCotizacionesCongeladas grupo.id
      fetched `shouldBe` mempty

  describe "freezeGrupo / unfreezeGrupo" $ do
    it "congelar sin consolidar borra cotizaciones viejas" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Test Grupo" "alguien"
      runDb $ saveCotizacionesCongeladas grupo.id (1200 `enMoneda` USD)

      runDb $ freezeGrupo grupo.id mempty

      fetched <- runDb $ fetchCotizacionesCongeladas grupo.id
      fetched `shouldBe` mempty

    it "descongelar borra las cotizaciones" $ \(RunDb runDb) -> do
      grupo <- runDb $ createGrupo "Test Grupo" "alguien"
      runDb $ freezeGrupo grupo.id mempty
      runDb $ saveCotizacionesCongeladas grupo.id (1200 `enMoneda` USD)

      runDb $ unfreezeGrupo grupo.id

      fetched <- runDb $ fetchCotizacionesCongeladas grupo.id
      fetched `shouldBe` mempty
