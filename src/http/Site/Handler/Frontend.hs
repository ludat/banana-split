module Site.Handler.Frontend (
  handleFrontend,
  handleInitialDataScript,
) where

import Control.Monad.Reader
import Data.List qualified as List
import Network.HTTP.Types (Header, hCacheControl, hContentType, notFound404, ok200)
import Network.Wai
import Network.Wai.Application.Static (defaultWebAppSettings, staticApp)
import WaiAppStatic.Types (StaticSettings (..))

import Preludat
import Site.Auth (sessionTokenFromRequest, verifyToken)
import Site.Frontend (InitialData (..), initialDataScript, renderFrontendShell)
import Site.Types

-- | The static frontend, as the fallback after the API. Files under @./public@
-- are served as they are; any other path is a route of the Elm app and gets
-- the shell with the 'InitialData' injected.
handleFrontend :: Request -> (Response -> IO ResponseReceived) -> AppHandler ResponseReceived
handleFrontend req respond = do
  app <- ask
  if pathInfo req `elem` [[], ["index.html"]]
    then liftIO $ shellResponse app req >>= respond
    else
      liftIO
        $ staticApp
          (defaultWebAppSettings "./public")
            { ss404Handler = Just $ \r k -> shellResponse app r >>= k
            }
          req
          respond

shellResponse :: App -> Request -> IO Response
shellResponse app req = case app.frontendShell of
  Nothing -> pure $ responseLBS notFound404 [(hContentType, "text/plain")] "frontend not built"
  Just shell -> do
    initialData <- initialDataFor app req
    pure
      $ responseLBS
        ok200
        ((hContentType, "text/html; charset=utf-8") : privateHeaders)
        (renderFrontendShell shell initialData)

-- | The 'InitialData' for the elm-land dev server, which serves its own HTML
-- (see 'Site.Frontend.initialDataScriptPath'). The built shell never asks for it.
handleInitialDataScript :: Request -> (Response -> IO ResponseReceived) -> AppHandler ResponseReceived
handleInitialDataScript req respond = do
  app <- ask
  -- Any site can put this in a <script src>. The cookie is SameSite=Lax, so it
  -- doesn't travel on that request anyway; this is the second lock.
  let crossSite = maybe False (/= "same-origin") (List.lookup "Sec-Fetch-Site" (requestHeaders req))
  initialData <- if crossSite then pure InitialData{user = Nothing} else liftIO (initialDataFor app req)
  liftIO
    $ respond
    $ responseLBS
      ok200
      ( [ (hContentType, "text/javascript; charset=utf-8")
        , ("X-Content-Type-Options", "nosniff")
        ]
          <> privateHeaders
      )
      (initialDataScript initialData)

-- | Read-only on purpose: the session straight from the token, no database and
-- no @Set-Cookie@. Renewing it is @\/auth\/refresh@'s job.
initialDataFor :: App -> Request -> IO InitialData
initialDataFor app req = do
  user <- maybe (pure Nothing) (verifyToken app.jwk) (sessionTokenFromRequest req)
  pure InitialData{user = user}

-- | Both carry the user: no cache may keep them or share them.
privateHeaders :: [Header]
privateHeaders = [(hCacheControl, "private, no-cache"), ("Vary", "Cookie")]
