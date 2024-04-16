module BananaSplit.Elm
    ( generateElmFiles
    ) where

import BananaSplit (Deudas, Grupo, Monto, Pago, Parte, Participante, ParticipanteId, Transaccion)

import Data.Data
import Data.ULID (ULID)

import Elm.TyRep

import Servant
import Servant.Elm

import Site.Api (Api (..), Netos, ParticipanteAddParams (ParticipanteAddParams))
import Site.Handler.Grupos (CreateGrupoParams)



generateElmFiles :: IO ()
generateElmFiles = do
  putStrLn "Generating elm files..."
  generateElmModuleWith
    (defElmOptions
      { elmToString = \case
        ETyCon (ETCon "Bool")             -> "(\\value -> if value then \"true\" else \"false\")"
        ETyCon (ETCon "Float")            -> "String.fromFloat"
        ETyCon (ETCon "Char")             -> "String.fromChar"
        ETyApp (ETyCon (ETCon "Maybe")) v -> "(Maybe.map " <> defaultElmToString v <> " >> Maybe.withDefault \"\")"
        ETyCon (ETCon "ULID") -> ""
        e -> error $ show e
       , urlPrefix = Static "/api"
      })
    [ "Generated",
      "Api"
    ]
    defElmImports
    "ui/src/"
    [ DefineElm (Proxy :: Proxy CreateGrupoParams)
    , DefineElm (Proxy :: Proxy ParticipanteAddParams)
    , DefineElm (Proxy :: Proxy Grupo)
    , DefineElm (Proxy :: Proxy Participante)
    , DefineElm (Proxy :: Proxy Transaccion)
    , DefineElm (Proxy :: Proxy Netos)
    , DefineElm (Proxy :: Proxy Pago)
    , DefineElm (Proxy :: Proxy Parte)
    , DefineElm (Proxy :: Proxy ParticipanteId)
    , DefineElm (Proxy :: Proxy ULID)
    , DefineElm (Proxy :: Proxy (Deudas Monto))
    , DefineElm (Proxy :: Proxy Monto)
    ]
    (Proxy :: Proxy (ToServantApi Api))
