module Main (
  main,
) where

import Protolude

import BananaSplit.Elm qualified as Elm
import BananaSplit.Persistence qualified as Persistence
import BananaSplit.PgRoll qualified as PgRoll
import BananaSplit.Seed qualified as Seed
import RunServer qualified
import Site.Config (createConfig)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      Elm.generateElmFiles
      RunServer.runBackend
    ["server"] -> do
      RunServer.runBackend
    ["generate"] -> do
      Elm.generateElmFiles
    "migrations" : rest -> do
      config <- createConfig "dev"
      PgRoll.rawCall config rest
    "run-migration" : rest -> do
      config <- createConfig "dev"
      Persistence.runMigration config rest
    -- Herramientas de desarrollo para medir el costo de calcular netos.
    ["seed-gastos", participantes, gastos] -> do
      conn <- createConfig "dev" >>= Persistence.openConnection
      case (readMaybe participantes, readMaybe gastos) of
        (Just p, Just g) -> void $ Seed.seedGrupo conn p g
        _ -> putText "uso: seed-gastos <participantes> <gastos>" >> exitFailure
    ["bench-netos", grupoId, vueltas] -> do
      conn <- createConfig "dev" >>= Persistence.openConnection
      case (readMaybe grupoId, readMaybe vueltas) of
        (Just g, Just v) -> Seed.benchNetos conn g v
        _ -> putText "uso: bench-netos <grupoId> <vueltas>" >> exitFailure
    _ -> do
      putText $ "Unknown command: " <> show args
      exitFailure
