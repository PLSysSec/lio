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
import Data.Maybe

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
    get "/:pId/edit" $ do
      pId  <- queryParam' "pId"
      post <- getPostById pId
      render "edit.html" post
    post "/" $ do
      post <- formToPost Nothing
      pId  <- insertPost post
      respond . redirectTo . S8.pack $ "/" ++ pId
    post "/:pId" $ do
      pId  <- queryParam' "pId"
      post <- formToPost (Just pId)
      updatePost post
      respond . redirectTo . S8.pack $ "/" ++ pId

formToPost :: Maybe PostId -> DCController AppSettings Post
formToPost mpId = do
  (params, _) <- parseForm
  let mpost = do
        title <- lookup "title" params
        body  <- lookup "body" params
        let pub = isJust $ lookup "publish" params
        if S8.null title || S8.null body
          then fail "Invalid form"
          else return $ Post { postId    = pId
                             , postTitle = S8.unpack title
                             , postBody  = S8.unpack body 
                             , postIsPublished = pub
                             }
  case mpost of
    Nothing   -> redirectBack'
    Just post -> return post
  where pId = fromMaybe undefined mpId
        redirectBack' =  do 
          redirectBack 
          return $ error "future version of simple won't need this type fix"
