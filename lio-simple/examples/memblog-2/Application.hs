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

app :: (SimpleDCApplication -> DC ()) -> DC ()
app runner = do
  settings <- newAppSettings

  runner $ \currentPriv -> controllerApp settings $ do
    get "/" $ do
      posts <- getAllPosts
      ctx <- debugGetCtx
      render "index.html" $ object ["posts" .= posts, "ctx" .= ctx ]
    get "/new" $ do
      render "new.html" ()
    get "/login" $ withUser $ \user ->
      render "login.html" $ object ["user" .= user]
    get "/logout" $ do
      muser <- currentUser
      respond $ case muser of
                  Nothing -> redirectTo "/"
                  Just _  -> requestLogin
    get "/:pId" $ do
      pId   <- queryParam' "pId"
      post  <- getPostById pId
      muser <- currentUser
      let showEdit = Just (S8.pack $ postAuthor post) == muser
      render "show.html" $ object [ "post" .= post, "showEdit" .= showEdit]
    get "/:pId/edit" $ do
      pId     <- queryParam' "pId"
      curPost <- getPostById pId
      render "edit.html" curPost
    post "/" $ withUser $ \user -> do
      curPost <- formToPost user Nothing
      pId     <- insertPost currentPriv curPost
      respond . redirectTo . S8.pack $ "/" ++ pId
    post "/:pId" $ withUser $ \user -> do
      pId     <- queryParam' "pId"
      curPost <- formToPost user (Just pId)
      updatePost currentPriv curPost
      respond . redirectTo . S8.pack $ "/" ++ pId

formToPost :: UserName -> Maybe PostId -> DCController AppSettings Post
formToPost user mpId = do
  (params, _) <- parseForm
  let mpost :: Maybe Post
      mpost = do
        pTitle <- lookup "title" params
        pBody  <- lookup "body" params
        let pub = isJust $ lookup "publish" params
        if S8.null pTitle || S8.null pBody
          then fail "Invalid form"
          else return $ Post { postId          = pId
                             , postTitle       = S8.unpack pTitle
                             , postBody        = S8.unpack pBody 
                             , postIsPublished = pub
                             , postAuthor      = user }
  case mpost of
    Nothing -> redirectBack'
    Just p  -> return p
  where pId = fromMaybe undefined mpId
        redirectBack' =  do 
          redirectBack 
          return $ error "future version of simple won't need this type fix"

withUser :: (UserName -> DCController AppSettings ()) 
         -> DCController AppSettings ()
withUser act = withUserOrLogin $ \user -> act (S8.unpack user)
