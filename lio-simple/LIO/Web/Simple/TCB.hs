{-# LANGUAGE Unsafe #-}

{- |
 
 This module exports a function 'run' for creating a runner that is
 used to run a "Web.Simple" 'SimpleApplication' in the 'LIO' monad.

 The runner is only available to trusted code since we do not impose
 any policy on how requests and responses should be handled.
 Middleware should be used on both ends to ensure safety. This module
 provides several such Middleware.

-}

module LIO.Web.Simple.TCB ( 
    -- * Runners
    run 
    -- * Middleware
  , browserLabelGuard
  , removeRequestHeaders 
  , removeResponseHeaders 
  ) where

import LIO
import LIO.TCB (ioTCB, getLIOStateTCB)

import qualified Data.List as List
import qualified Data.ByteString.Lazy.Char8 as L8

import Web.Simple

import Network.HTTP.Types
import Network.Wai.Internal
import Network.Wai.Handler.Warp hiding (run)
import qualified Network.Wai.Handler.Warp as Warp

-- | Run an LIO web app wrapped by some middleware. Since web servers
-- can be quite messy it is important that you provide middleware to
-- sanitize responses to prevent data leakage.
-- 
-- Since security properties vary across applications, we do not
-- impose any conditions on the requests and reponses. The latter can
-- be sanitized by supplying a middleware, while the former can simply
-- be baked-into the app (as 'SimpleMiddleware'.
--
run :: Label l => Port -> Middleware -> SimpleApplication (LIO l) -> LIO l ()
run port middleware app = do
  state <- getLIOStateTCB
  ioTCB $ Warp.run port $ middleware . filterFileResponses $ 
    \req -> evalLIO (app req) state

-- | Remove any responses that were built with 'responseFile' or
-- 'responseSource'.
filterFileResponses :: Middleware
filterFileResponses app req = do
  resp <- app req
  return $ case resp of
    ResponseBuilder _ _ _ -> resp
    _ -> serverError $ L8.pack "App should not read directly from files."

-- | Middleware that ensures the 'Response' from the
-- application is readable by the client's browser (as determined by the
-- result label of the app computation and the label of the browser). If
-- the response is not readable by the browser, the middleware sends a
-- 403 (unauthorized) response instead.
browserLabelGuard :: Label l => l -> SimpleMiddleware (LIO l)
browserLabelGuard browserLabel app req = do
  resp <- app req
  resultLabel <- getLabel
  return $ if resultLabel `canFlowTo` browserLabel
             then resp
             else forbidden

-- | Remove certain headers from the request.
removeRequestHeaders :: Monad m => [HeaderName] -> SimpleMiddleware m
removeRequestHeaders headers app req = do
  app $ foldr (\h r -> rmRequestHeader r h) req headers
   where rmRequestHeader r h = r { requestHeaders = rm h (requestHeaders r) }

-- | Remove certain headers from the response, e.g., Set-Cookie.
removeResponseHeaders :: Monad m => [HeaderName] -> SimpleMiddleware m
removeResponseHeaders headers app req = do
  resp <- app req
  return $ foldr (\h r -> rmResponseHeader r h) resp headers

rmResponseHeader :: Response -> HeaderName -> Response
rmResponseHeader (ResponseFile    x hs y z) h = ResponseFile    x hs' y z where hs' = rm h hs
rmResponseHeader (ResponseBuilder x hs y  ) h = ResponseBuilder x hs' y   where hs' = rm h hs
rmResponseHeader (ResponseSource  x hs y  ) h = ResponseSource  x hs' y   where hs' = rm h hs

rm :: HeaderName -> [Header] -> [Header]
rm h = List.filter ((/= h) . fst)
