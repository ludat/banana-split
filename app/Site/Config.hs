module Site.Config where

import Conferer qualified

createConfig :: IO Conferer.Config
createConfig =
  Conferer.mkConfig "bananasplit"
