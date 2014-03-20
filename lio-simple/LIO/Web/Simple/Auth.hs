{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides generic and HTTP Basic authentication.
module LIO.Web.Simple.Auth ( basicAuth
                           , handleAuth
                           , requestLogin
                           -- * Helpers
                           , withUserOrLogin
                           , currentUser
                           ) where

import Data.Maybe
import Control.Monad
import qualified Data.ByteString.Char8 as S8
import Data.ByteString.Base64
import Network.HTTP.Types
import Network.Wai
import Web.Simple.Responses
import Web.Simple.Controller.Trans


-- | A middleware that uses HTTP basic authentication to authenticate
-- a request for a realm with the given username and password. The
-- request is rewritten with an @X-User@ request header containing the
-- authenticated username before being passed to the next
-- 'application'. Note that the HTTP basic authentication header is
-- only set if the executed app requests it, by setting the @X-Login@
-- response header (e.g., with 'requestLogin').
basicAuth :: Monad m
          => String                          -- ^ Realm
          -> (S8.ByteString -> S8.ByteString -> m Bool)
          -> SimpleMiddleware m
basicAuth realm auth app0 req0 = handleAuth authApp (mkApp app0) req0
  where authApp = const . return $ requireBasicAuth realm
        mkApp app req =  case getBasicAuthLogin req of
          Nothing -> app req
          Just (usr, pwd) -> do
            success <- auth usr pwd
            let req' = req { requestHeaders = ("X-User", usr) : 
                                              requestHeaders req }
            if success then app req' else app req


-- | Helper method for implementing basic authentication. Given a
-- 'Request' returns the (username, password) pair from the basic
-- authentication header if present.
getBasicAuthLogin :: Request -> Maybe (S8.ByteString, S8.ByteString)
getBasicAuthLogin req = do
  authStr <- lookup hAuthorization $ requestHeaders req
  unless ("Basic" `S8.isPrefixOf` authStr) $ fail "Not basic auth."
  let up = fmap (S8.split ':') $ decode $ S8.drop 6 authStr
  case up of
     Right (user:pwd:[]) -> return (user, pwd)
     _ -> fail "Malformed basic auth header."

-- | Executes the app and if the app 'Response' has header
-- @X-Login@ and the user is not logged in, i.e., the @X-User@ request
-- header is not present, execute the login application.
handleAuth :: Monad m => SimpleApplication m -> SimpleMiddleware m
handleAuth loginApp app req = do
  resp <- app req
  if hasLogin resp && notLoggedIn
    then loginApp req
    else return resp
  where hasLogin r  = "X-Login" `isIn` responseHeaders r
        notLoggedIn = not $ "X-User" `isIn` requestHeaders req
        isIn n xs   = isJust $ lookup n xs

-- | Request authentication middleware to authenticate user
requestLogin :: Response
requestLogin = responseLBS status400 [("X-Login", "True")] ""

-- | Execute action with the current user's name. Otherwise, request
-- that the user authenticate.
withUserOrLogin :: Monad m 
                => (S8.ByteString -> ControllerT m r a)
                -> ControllerT m r a
withUserOrLogin act = currentUser >>= \muser ->
  maybe (respond requestLogin) act muser

-- | Get the current user.
currentUser :: Monad m => ControllerT m r (Maybe S8.ByteString)
currentUser = requestHeader "X-User"
