{-# LANGUAGE OverloadedStrings #-}
module Application where

import Memblog.Common

import LIO
import LIO.DCLabel
import LIO.Web.Simple
import LIO.Web.Simple.Auth
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
    get "/login" $ withUser $ \user ->
      render "login.html" $ object ["user" .= user]
    get "/:pId" $ do
      pId  <- queryParam' "pId"
      post <- getPostById pId
      render "show.html" post
    get "/:pId/edit" $ do
      pId  <- queryParam' "pId"
      post <- getPostById pId
      render "edit.html" post
    post "/" $ withUser $ \user -> do
      post <- formToPost user Nothing
      pId  <- insertPost post
      respond . redirectTo . S8.pack $ "/" ++ pId
    post "/:pId" $ withUser $ \user -> do
      pId  <- queryParam' "pId"
      post <- formToPost user (Just pId)
      updatePost pId post
      respond . redirectTo . S8.pack $ "/" ++ pId

formToPost :: UserName -> Maybe PostId -> DCController AppSettings Post
formToPost user mpId = do
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
                             , postAuthor = user
                             }
  case mpost of
    Nothing   -> redirectBack'
    Just post -> return post
  where pId = fromMaybe undefined mpId
        redirectBack' =  do 
          redirectBack 
          return $ error "future version of simple won't need this type fix"

withBasicAuth :: DCController AppSettings () -> DCController AppSettings ()
withBasicAuth controller = do
  s <- controllerState
  fromApp $ basicAuth "memblog-2" (\_ _ -> return True) $ 
    controllerApp s controller
    -- Since our controller state is really just an LMVar we can
    -- call the run function without having to restore state
    -- If the app gets more complicated: use an LIORef to store the
    -- end state and read the LIORef after the fromApp
    
withUser :: (UserName -> DCController AppSettings ()) 
         -> DCController AppSettings ()
withUser act = withBasicAuth $ do
   mu <- requestHeader "X-User"
   case mu of
     Just u  -> act (S8.unpack u)
     Nothing -> fail "withUser: expected X-User to be set"
