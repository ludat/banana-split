{-# LANGUAGE StrictData #-}

module Repartix.Receipts.Types (ReceiptsReaderConfig (..)) where

import Network.HTTP.Client (Manager)
import Protolude

data ReceiptsReaderConfig = ReceiptsReaderConfig
  { apiKey :: Text
  , models :: [Text]
  , manager :: Manager
  }
  deriving (Generic)
