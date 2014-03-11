{-# LANGUAGE Unsafe #-}
{-# LANGUAGE FlexibleContexts #-}

{- |
 
 This module exports a function 'run' for creating a runner that is
 used to run a "Web.Simple" 'SimpleApplication' in the 'LIO' monad.

 The runner is only available to trusted code since we do not impose
 any policy on how requests and responses should be handled.
 Middleware should be used on both ends to ensure safety. This module
 provides several such Middleware.

-}

module LIO.Web.Simple.TCB (
    -- * LIO applications
    SimpleLIOApplication, SimpleLIOMiddleware
    -- * Runners
  , run, runP
    -- * Middleware
  , browserLabelGuard
  , removeRequestHeaders
  , removeResponseHeaders
    -- * Templates
  , lioGetTemplateTCB
  ) where

import safe Control.Monad

import Data.Text.Encoding

import safe Control.Applicative

import safe LIO
import safe LIO.Error
import LIO.TCB (ioTCB, getLIOStateTCB)

import safe qualified Data.List as List
import safe qualified Data.ByteString.Char8 as S8
import safe qualified Data.ByteString.Lazy.Char8 as L8

import safe Web.Simple
import safe Web.Simple.Templates.Language

import Network.HTTP.Types
import Network.Wai.Internal
import Network.Wai.Handler.Warp hiding (run)
import qualified Network.Wai.Handler.Warp as Warp

import safe System.FilePath

-- | An LIO simple aplpication is an 'LIO' computation mapping a set
-- of privileges and request to a response. While privileges can be
-- provided in terms of a e.g., 'Reader' monad, in certain cases not
-- having the privilege as part of the sate is cleaner.
type SimpleLIOApplication p l = Priv p -> SimpleApplication (LIO l)

-- | Simple LIO middleware.
type SimpleLIOMiddleware p l = SimpleLIOApplication p l -> SimpleLIOApplication p l

-- | Run an LIO web app wrapped by some middleware. Since web servers
-- can be quite messy it is important that you provide middleware to
-- sanitize responses to prevent data leakage.
--
-- Since security properties vary across applications, we do not
-- impose any conditions on the requests and reponses. The latter can
-- be sanitized by supplying a middleware, while the former can simply
-- be baked-into the app (as 'SimpleMiddleware'.
run :: Label l => Port -> Middleware -> SimpleApplication (LIO l) -> LIO l ()
run port middleware app = runP port middleware noPrivs (const app)

-- | Same as 'run', but run 'SimpleLIOApplication's, i.e.,
-- applications that take privileges.
runP :: (PrivDesc l p, Label l)
     => Port -> Middleware -> Priv p -> SimpleLIOApplication p l -> LIO l ()
runP port middleware priv app = do
  state <- getLIOStateTCB
  ioTCB $ Warp.run port $ middleware . filterFileResponses $
    \req -> evalLIO (app priv req) state

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
browserLabelGuard :: MonadLIO l m => l -> SimpleMiddleware m
browserLabelGuard browserLabel app req = do
  resp <- app req
  resultLabel <- liftLIO $ getLabel
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

-- | Function to use to get a template. When the underlying monad is
-- 'LIO', it looks in the 'viewDirectory' for the given file name and
-- compiles the file into a template.
--
-- This function should be used only when the everything reachable
-- from the 'viewDirectory' is public.
--
-- To ensure that the function cannot be abused the function first
-- cleans up the file path: if it starts out with a @..@, we consider
-- this invalid as it can be used explore parts of the filesystem that
-- should otherwise be unaccessible. Similarly, we remove any @.@ from
-- the path.
--
-- Since this funciton does not use the 'lio-fs' filesystem @readFile@,
-- but rather the 'IO' @readFile@, it should not be exposed to
-- untrusted code.
lioGetTemplateTCB :: Label l => FilePath -> LIO l Template
lioGetTemplateTCB fp = do
  fp'  <- cleanUpPath fp
  eres <- compileTemplate . decodeUtf8 <$> (ioTCB $ S8.readFile fp')
  case eres of
    Left str -> fail str
    Right tmpl -> return tmpl

-- | Cleanup a file path, if it starts out with a @..@, we consider this
-- invalid as it can be used explore parts of the filesystem that should
-- otherwise be unaccessible. Similarly, we remove any @.@ from the path.
cleanUpPath :: Label l => FilePath -> LIO l FilePath 
cleanUpPath path = withContext "cleanUpPath" $
                    doit . splitDirectories . normalise . stripSlash $ path
  where doit []          = return []
        doit ("..":_)    = throwLIO $ userError "Path cannot contain .."
        doit (_:"..":xs) = doit xs
        doit (".":xs)    = doit xs
        doit (x:xs)      = (x </>) `liftM` doit xs

-- | Remove any 'pathSeparator's from the front of a file path.
stripSlash :: FilePath -> FilePath 
stripSlash [] = []
stripSlash xx@(x:xs) | x == pathSeparator = stripSlash xs
                     | otherwise          = xx

