module BananaSplit.Persistence.Migration_2026_05_26_FixDates (run) where

import Data.Time (Day, UTCTime (..), fromGregorian)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.ULID (ULID, ulidToInteger)
import Database.Beam as Beam
import Database.Beam.Postgres

import BananaSplit.Persistence.Schema
import Preludat

ulidToDay :: ULID -> Day
ulidToDay ulid =
  let millis = ulidToInteger ulid `shiftR` 80
      UTCTime day _ = posixSecondsToUTCTime (fromInteger millis / 1000)
  in day

defaultFecha :: Day
defaultFecha = fromGregorian 2020 1 1

run :: Pg ()
run = do
  pagos <- runSelectReturningList $ select $ all_ db.pagos
  forM_ pagos $ \pago -> do
    when (pago.fecha == defaultFecha) $ do
      let day = ulidToDay pago.pagoId
      runUpdate
        $ update
          db.pagos
          (\p -> p.fecha <-. val_ day)
          (\p -> p.pagoId ==. val_ pago.pagoId)
