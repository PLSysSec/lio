{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides HTTP Basic Authentication.
module LIO.Web.Simple.Auth ( basicAuth) where

import Control.Monad
import qualified Data.ByteString.Char8 as S8
import Data.ByteString.Base64
import Network.HTTP.Types
import Network.Wai
import Web.Simple.Responses
import Web.Simple.Middleware


-- | A 'Route' that uses HTTP basic authentication to authenticate a
-- request for a realm with the given username ans password. The
-- request is rewritten with an 'X-User' header containing the
-- authenticated username before being passed to the next 'Route'.
basicAuth :: Monad m
          => String                          -- ^ Realm
          -> (S8.ByteString -> S8.ByteString -> m Bool)
          -> SimpleMiddleware m
basicAuth realm auth app req = 
  case getBasicAuthLogin req of
    Nothing -> return authResp
    Just (usr, pwd) -> do
      success <- auth usr pwd
      let req' = req { requestHeaders = ("X-User", usr) : requestHeaders req }
      if success
        then app req'
        else return authResp
    where authResp = requireBasicAuth realm


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
