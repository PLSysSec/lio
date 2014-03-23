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
      pId     <- queryParam' "pId"
      curPost <- getPostById pId
      render "show.html" curPost
    post "/" $ do
      (params, _) <- parseForm
      let mpost :: Maybe Post
          mpost = do
            pTitle <- lookup "title" params
            pBody  <- lookup "body" params
            if S8.null pTitle || S8.null pBody
              then fail "Invalid form"
              else return $ Post { postId    = undefined
                                 , postTitle = S8.unpack pTitle
                                 , postBody  = S8.unpack pBody }
      case mpost of
        Just p -> do pId <- insertPost p
                     respond . redirectTo . S8.pack $ "/" ++ pId
        _      -> redirectBack 


