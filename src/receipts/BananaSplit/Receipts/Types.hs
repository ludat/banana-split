{-# LANGUAGE StrictData #-}
module BananaSplit.Receipts.Types (ReceiptsReaderConfig(..)) where

import Protolude

import Network.HTTP.Client (Manager)


data ReceiptsReaderConfig = ReceiptsReaderConfig
  { apiKey :: Text
  , models :: [Text]
  , manager :: Manager
  }
  deriving (Generic)
