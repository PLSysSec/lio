{-# LANGUAGE OverloadedStrings #-}
module Application where

import Memblog.Common

import LIO
import LIO.DCLabel
import LIO.Web.Simple
import LIO.Web.Simple.DCLabel
import Web.Frank

import Data.Aeson
import qualified Data.ByteString.Char8 as S8

app :: (SimpleApplication DC -> DC ()) -> DC ()
app runner = do
  settings <- newAppSettings

  runner $ controllerApp settings $ do
    get "/" $ do
      posts <- getAllPosts
      render "index.html" $ object ["posts" .= posts ]
    get "/new" $ do
      render "new.html" ()
    get "/:pId" $ do
      pId  <- queryParam' "pId"
      post <- getPostById pId
      render "show.html" post
    post "/" $ do
      (params, _) <- parseForm
      let mpost = do
            title <- lookup "title" params
            body  <- lookup "body" params
            if S8.null title || S8.null body
              then fail "Invalid form"
              else return $ Post { postId    = undefined
                                 , postTitle = S8.unpack title
                                 , postBody  = S8.unpack body }
      case mpost of
        Just post -> do pId <- insertPost post
                        respond . redirectTo . S8.pack $ "/" ++ pId
        _         -> redirectBack 


