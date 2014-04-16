{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}

module Memblog.Common where

import Prelude hiding (readFile, writeFile, appendFile, catch)
import LIO
import LIO.Error
import LIO.DCLabel

import LIO.Web.Simple
import LIO.Web.Simple.DCLabel
import Web.Simple.Templates
import Control.Applicative
import System.FilePath

import LIO.Web.Simple.TCB (lioGetTemplateTCB)

import GHC.Generics
import Data.Aeson

import LIO.Concurrent

data AppSettings = AppSettings { db :: LMVar DCLabel [Post]}

newAppSettings :: DC AppSettings
newAppSettings = do
  mv <- newLMVar dcPublic [post0, post1]
  return $ AppSettings { db = mv }


instance HasTemplates DC AppSettings where
  viewDirectory = return $ "liofs" </> "views"
  defaultLayout = Just <$> getTemplate ("liofs" </> "layouts" </> "main.html")
  getTemplate = liftLIO . lioGetTemplateTCB
  -- NOTE: We assume that "liofs" only contains public data, DO NOT
  -- store any sensitive data in this directory

-- | Post Id's are stringified Ints
type PostId = String

-- | Data-type representing a blog post
data Post = Post { postId    :: PostId
                 , postTitle :: String
                 , postBody  :: String }
                 deriving (Show, Read)

deriving instance Generic Post
instance ToJSON Post

getAllPosts :: DCController AppSettings [Post]
getAllPosts = do
  settings <- controllerState
  liftLIO . withContext "getAllPosts" $ readLMVar $ db settings

getPostById :: PostId -> DCController AppSettings Post
getPostById idNr = do
  posts <- getAllPosts
  case filter ((== idNr) . postId) posts of
    [post] -> return post
    _      -> fail "No such post"

insertPost :: Post -> DCController AppSettings PostId
insertPost post = do
  settings <- controllerState
  liftLIO . withContext "insertPost" $ do
    posts <- takeLMVar $ db settings
    let pId   = show $ length posts
        post' = post { postId = pId }
    putLMVar (db settings) $ post' : posts
    return pId


--
-- Dummy posts
--

post0 :: Post
post0 = Post {
    postId    = "0"
  , postTitle = "The Title of Your First Post on a Single Line"
  , postBody  = unlines 
     [ "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
     , "Etiam vitae interdum sapien. In congue..." ] 
  }

post1 :: Post
post1 = Post {
    postId    = "1"
  , postTitle = "The Title of Your Second Post"
  , postBody  = unlines 
     [ "Aliquam tempor varius justo vitae bibendum! Duis vitae rutrum"
     , "neque. Sed ut sed..." ]
  }
