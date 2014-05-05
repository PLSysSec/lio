{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Control.Monad (liftM)
import System.FilePath

import LIO.Web.Simple.TCB (lioGetTemplateTCB)

import GHC.Generics
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable

import LIO.Concurrent
import LIO.TCB (ioTCB)

data AppSettings = AppSettings { 
  db :: LMVar DCLabel (Map PostId (Labeled DCLabel Post))
}

newAppSettings :: DC AppSettings
newAppSettings = do
  lpost0 <- label (postPolicy post0 `lub` dcPublic) post0
  lpost1 <- label (postPolicy post1 `lub` dcPublic) post1
  mv <- newLMVar dcPublic $ Map.fromList 
    [(postId post0, lpost0), (postId post1, lpost1)]
  return $ AppSettings { db = mv }


instance HasTemplates DC AppSettings where
  viewDirectory = return $ "liofs" </> "views"
  defaultLayout = Just <$> getTemplate ("liofs" </> "layouts" </> "main.html")
  getTemplate = liftLIO . lioGetTemplateTCB
  -- NOTE: We assume that "liofs" only contains public data, DO NOT
  -- store any sensitive data in this directory


-- | UserName's are just Strings
type UserName = String

-- | Post Id's are stringified Ints
type PostId = String

-- | Data-type representing a blog post
data Post = Post { postId          :: PostId
                 , postTitle       :: String
                 , postBody        :: String 
                 , postIsPublished :: Bool
                 , postAuthor      :: UserName
                 } deriving (Show, Read)

deriving instance Generic Post
instance ToJSON Post

postPolicy :: Post -> DCLabel
postPolicy post = let author = postAuthor post
                  in  if postIsPublished post
                        then True   %% author
                        else author %% author

labelPost :: DCPriv -> Post -> DC (Labeled DCLabel Post)
labelPost priv post = withContext "labelPost" $ labelP priv (postPolicy post) post

getAllPosts :: DCController AppSettings [Post]
getAllPosts = do
  settings <- controllerState
  liftLIO . withContext "getAllPosts" $ do
    lposts <- readLMVar $ db settings
    foldlM f [] lposts
   where f posts lpost = do handle (handler posts) $ do
                              post <- unlabel lpost
                              return $ post : posts
         handler :: [Post] -> LabelError DCLabel -> DC [Post]
         handler ps _ = return ps

getPostById :: PostId -> DCController AppSettings Post
getPostById idNr = do
  posts <- getAllPosts
  case filter ((== idNr) . postId) posts of
    [post] -> return post
    _      -> fail "No such post"

insertPost :: DCPriv -> Post -> DCController AppSettings PostId
insertPost priv post = do
  settings  <- controllerState
  liftLIO . withContext "insertPost" $ do
    lposts <- takeLMVar $ db settings
    let pId = show $ Map.size lposts
    lpost <- labelPost priv post { postId = pId }
              `onException` putLMVar (db settings) lposts
    putLMVar (db settings) $ Map.insert pId lpost lposts 
    return pId

updatePost :: DCPriv -> Post -> DCController AppSettings ()
updatePost priv post = do
  settings  <- controllerState
  liftLIO . withContext "updatePost" $ do
    lpost  <- labelPost priv post
    lposts <- takeLMVar $ db settings
    guardUpdate (postId post) lposts
      `onException` putLMVar (db settings) lposts
    putLMVar (db settings) $ Map.adjust (const lpost) (postId post) lposts
  where guardUpdate pId lposts = do
          case Map.lookup pId lposts of
            Nothing    -> fail "updatePost: post does not exist"
            Just lpost -> guardAllocP priv (labelOf lpost)


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
  , postIsPublished = True
  , postAuthor = "cicero"
  }

post1 :: Post
post1 = Post {
    postId    = "1"
  , postTitle = "The Title of Your Second Post"
  , postBody  = unlines 
     [ "Aliquam tempor varius justo vitae bibendum! Duis vitae rutrum"
     , "neque. Sed ut sed..." ]
  , postIsPublished = False
  , postAuthor = "cicero"
  }

--
-- Debug helpers
--


-- | Get the current label and clearance as a JSON value
debugGetCtx :: DCController AppSettings Value
debugGetCtx = liftLIO $ do
  l <- getLabel
  c <- getClearance
  return $ object [ "label"     .= show l
                  , "clearance" .= show c ]

-- | Print the current label and clearance
debugPrintCtx :: MonadLIO DCLabel m => m ()
debugPrintCtx = liftLIO $ do
  l <- getLabel
  c <- getClearance
  ioTCB . putStrLn $ "Current label = " ++ show l
  ioTCB . putStrLn $ "Current clearance = " ++ show c
