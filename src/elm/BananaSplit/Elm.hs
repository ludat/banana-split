{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module BananaSplit.Elm (
  generateElmFiles,
) where

import Data.Data
import Elm.TyRep
import Protolude
import Servant
import Servant.Elm
import Servant.Foreign (Foreign, HasForeign (..))

import BananaSplit
import Site.Api

-- | servant-elm has no 'HasForeign' instance for 'AuthProtect', so codegen
-- would stop compiling once an auth-protected route enters the API. The
-- session lives in an httpOnly cookie the browser sends automatically, so the
-- Elm side needs nothing extra: this pass-through simply skips the combinator.
instance
  forall lang ftype (tag :: Symbol) api.
  (HasForeign lang ftype api) =>
  HasForeign lang ftype (AuthProtect tag :> api)
  where
  type Foreign ftype (AuthProtect tag :> api) = Foreign ftype api
  foreignFor lang ftype Proxy req =
    foreignFor lang ftype (Proxy :: Proxy api) req

generateElmFiles :: IO ()
generateElmFiles = do
  putText "Generating elm files..."
  generateElmModuleWith
    ( defElmOptions
        { elmToString = \case
            ETyCon (ETCon "Bool") -> "(\\value -> if value then \"true\" else \"false\")"
            ETyCon (ETCon "Float") -> "String.fromFloat"
            ETyCon (ETCon "Char") -> "String.fromChar"
            ETyApp (ETyCon (ETCon "Maybe")) v -> "(Maybe.map " <> defaultElmToString v <> " >> Maybe.withDefault \"\")"
            ETyCon (ETCon "ULID") -> ""
            e -> panic $ show e
        , urlPrefix = Static "/api"
        }
    )
    [ "Generated"
    , "Api"
    ]
    ( [ defElmImports
      , "import Utils.Posix exposing (Posix, jsonDecPosix, jsonEncPosix)"
      , "import Date"
      , "import Utils.Day exposing (Day, jsonDecDay, jsonEncDay)"
      ]
        & intersperse "\n"
        & mconcat
    )
    "ui/generated-src/"
    [ DefineElm (Proxy :: Proxy CreateGrupoParams)
    , DefineElm (Proxy :: Proxy CreateGrupoAsUserParams)
    , DefineElm (Proxy :: Proxy SigninParams)
    , DefineElm (Proxy :: Proxy SignupParams)
    , DefineElm (Proxy :: Proxy LoginChallenge)
    , DefineElm (Proxy :: Proxy VerifyParams)
    , DefineElm (Proxy :: Proxy UpdateMeParams)
    , DefineElm (Proxy :: Proxy User)
    , DefineElm (Proxy :: Proxy UpdateGrupoParams)
    , DefineElm (Proxy :: Proxy ReceiptImageRequest)
    , DefineElm (Proxy :: Proxy ReceiptImageResponse)
    , DefineElm (Proxy :: Proxy ParticipanteAddParams)
    , DefineElm (Proxy :: Proxy ClaimRejection)
    , DefineElm (Proxy :: Proxy ClaimParticipanteResult)
    , DefineElm (Proxy :: Proxy ResumenGrupo)
    , DefineElm (Proxy :: Proxy (Netos Monto))
    , DefineElm (Proxy :: Proxy (PorMoneda (Netos Monto)))
    , DefineElm (Proxy :: Proxy Moneda)
    , DefineElm (Proxy :: Proxy ResumenPago)
    , DefineElm (Proxy :: Proxy ResumenNetos)
    , DefineElm (Proxy :: Proxy TipoErrorResumen)
    , DefineElm (Proxy :: Proxy ErrorResumen)
    , DefineElm (Proxy :: Proxy Grupo)
    , DefineElm (Proxy :: Proxy ShallowGrupo)
    , DefineElm (Proxy :: Proxy Participante)
    , DefineElm (Proxy :: Proxy Transaccion)
    , DefineElm (Proxy :: Proxy Pago)
    , DefineElm (Proxy :: Proxy ShallowPago)
    , DefineElm (Proxy :: Proxy Parte)
    , DefineElm (Proxy :: Proxy ParticipanteId)
    , DefineElm (Proxy :: Proxy ULID)
    , DefineElm (Proxy :: Proxy Monto)
    , DefineElm (Proxy :: Proxy Distribucion)
    , DefineElm (Proxy :: Proxy TipoDistribucion)
    , DefineElm (Proxy :: Proxy DistribucionPartes)
    , DefineElm (Proxy :: Proxy DistribucionDeSobras)
    , DefineElm (Proxy :: Proxy Repartija)
    , DefineElm (Proxy :: Proxy RepartijaItem)
    , DefineElm (Proxy :: Proxy RepartijaClaim)
    , DefineElm (Proxy :: Proxy ShallowRepartija)
    , DefineElm (Proxy :: Proxy RepartijaForFrontend)
    ]
    (Proxy :: Proxy (ToServantApi Api))
