module Site.Config
    ( createConfig
    ) where

import Conferer qualified
import Conferer.Source.CLIArgs as Cli
import Conferer.Source.Env qualified as Env
import Conferer.Source.PropertiesFile qualified as PropertiesFile

import Data.Dynamic (toDyn)
import Data.String

import Protolude

createConfig :: String -> IO Conferer.Config
createConfig env =
  Conferer.mkConfig'
    [("env", toDyn env)
    ]
    [ Cli.fromConfig
    , Env.fromConfig "bananasplit"
    , PropertiesFile.fromConfig "config.file"
    ]
