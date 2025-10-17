module BananaSplit.Elm
    ( generateElmFiles
    ) where

import BananaSplit

import Data.Data

import Elm.TyRep

import Protolude
import Protolude.Error

import Servant
import Servant.Elm

import Site.Api

import Site.Handler.Grupos (CreateGrupoParams)


generateElmFiles :: IO ()
generateElmFiles = do
  putText "Generating elm files..."
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
    , DefineElm (Proxy :: Proxy ReceiptImageRequest)
    , DefineElm (Proxy :: Proxy ReceiptImageResponse)
    , DefineElm (Proxy :: Proxy ParsedReceiptItem)
    , DefineElm (Proxy :: Proxy ParticipanteAddParams)
    , DefineElm (Proxy :: Proxy ResumenGrupo)
    , DefineElm (Proxy :: Proxy ResumenPago)
    , DefineElm (Proxy :: Proxy ResumenDeudas)
    , DefineElm (Proxy :: Proxy ErrorResumen)
    , DefineElm (Proxy :: Proxy Grupo)
    , DefineElm (Proxy :: Proxy ShallowGrupo)
    , DefineElm (Proxy :: Proxy Participante)
    , DefineElm (Proxy :: Proxy Transaccion)
    , DefineElm (Proxy :: Proxy Netos)
    , DefineElm (Proxy :: Proxy Pago)
    , DefineElm (Proxy :: Proxy ShallowPago)
    , DefineElm (Proxy :: Proxy Parte)
    , DefineElm (Proxy :: Proxy ParticipanteId)
    , DefineElm (Proxy :: Proxy ULID)
    , DefineElm (Proxy :: Proxy (Deudas Monto))
    , DefineElm (Proxy :: Proxy Monto)
    , DefineElm (Proxy :: Proxy Distribucion)
    , DefineElm (Proxy :: Proxy TipoDistribucion)
    , DefineElm (Proxy :: Proxy DistribucionMontosEspecificos)
    , DefineElm (Proxy :: Proxy MontoEspecifico)
    , DefineElm (Proxy :: Proxy DistribucionMontoEquitativo)
    , DefineElm (Proxy :: Proxy Repartija)
    , DefineElm (Proxy :: Proxy RepartijaItem)
    , DefineElm (Proxy :: Proxy RepartijaClaim)
    , DefineElm (Proxy :: Proxy ShallowRepartija)
    ]
    (Proxy :: Proxy (ToServantApi Api))
