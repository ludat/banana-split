module Site.Config where

import Conferer qualified

import Protolude

createConfig :: IO Conferer.Config
createConfig =
  Conferer.mkConfig "bananasplit"
