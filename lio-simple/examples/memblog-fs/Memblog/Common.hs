{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Memblog.Common where

import Prelude hiding (readFile, writeFile, appendFile, catch)
import LIO
import LIO.Error
import LIO.DCLabel

import LIO.Web.Simple
import LIO.Web.Simple.DCLabel
import Web.Simple.Templates
import Control.Applicative
import Control.Monad
import System.FilePath

import LIO.FS.Simple
import Data.Text.Encoding
import Web.Simple.Templates.Language

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Char8 as S8

import LIO.Concurrent

data AppSettings = AppSettings { db :: LMVar DCLabel [Post]}

newAppSettings :: DC AppSettings
newAppSettings = do
  -- Try to restore database from file, use dummy posts otherwise
  initPosts <- (read . S8.unpack) `liftM` readFile "db"
    `catch` (\(_ :: SomeException) -> return [post0, post1])
  mv <- newLMVar dcPublic initPosts
  return $ AppSettings { db = mv }


instance HasTemplates DC AppSettings where
  viewDirectory = return $ "views"
  defaultLayout = Just <$> getTemplate ("layouts" </> "main.html")
  getTemplate fp = do
    eres <- compileTemplate . decodeUtf8 <$> liftLIO (liftLIO $ readFile fp)
    case eres of
      Left str -> fail str
      Right tmpl -> return tmpl


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
  settings  <- controllerState
  liftLIO . withContext "insertPost" $ do
    posts     <- takeLMVar $ db settings
    let pId    = show $ length posts
        posts' = post { postId = pId } : posts
    writeFile (Just dcPublic) "db" (S8.pack $ show posts') 
       `catch` (\(_ :: SomeException) -> return ())
    putLMVar (db settings) $ posts'
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
