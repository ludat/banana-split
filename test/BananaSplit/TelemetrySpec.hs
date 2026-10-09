module BananaSplit.TelemetrySpec (
  spec,
) where

import GHC.Stats (getRTSStatsEnabled)
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  -- Esto existe porque la falla es silenciosa: 'registerGHCMetrics' devuelve
  -- una lista vacía, sin error, cuando el programa no corre con @+RTS -T@. Sin
  -- este test, sacar el @-T@ del @ghc-options@ no rompe nada — solo dejan de
  -- aparecer las métricas de runtime, y te enterás cuando las vas a buscar a
  -- Grafana y no están.
  --
  -- Corre contra el mismo @common threaded-rts@ que usa el executable, así que
  -- cubre el flag con el que se compila el servidor.
  describe "estadísticas del RTS" $ do
    it "están habilitadas, que es lo que necesitan las métricas de runtime" $ do
      getRTSStatsEnabled `shouldReturn` True
