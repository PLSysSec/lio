{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Application where

import Prelude hiding (writeFile, readFile, appendFile)
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Blog.Common
import Data.Aeson
import qualified Data.ByteString.Char8 as S8
import Data.List
import Network.HTTP.Types
import System.FilePath
import Web.Simple
import Web.Simple.Templates
import Web.Frank

import LIO
import LIO.DCLabel
import LIO.FS.Simple
import LIO.FS.Simple.DCLabel

--
-- Data model
--

type PostId = Int

-- | Datatype representing a blog post
data Post = Post { postId    :: Maybe PostId
                 , postTitle :: String
                 , postBody  :: String }
                 deriving (Show, Read, Eq)

-- | Directory containing all posts
postsDir :: FilePath
postsDir = "data"

-- | Get post by id
findPost :: MonadLIO DCLabel m => PostId -> m Post
findPost pid = let postFile = postsDir </> show pid
               in liftLIO $ (read . S8.unpack) `liftM` readFile postFile

-- | Get all posts
findAllPosts :: MonadLIO DCLabel m => m [Post]
findAllPosts = do
  postIds <- getAllPostIds
  mapM findPost postIds

-- | Save post to file
insertPost :: Post -> ControllerM AppSettings DC ()
insertPost post' = do
  postNo <- getNextPostNr
  let post = post' { postId = Just postNo }
  lcurr <- liftLIO getLabel
  writeFile (Just lcurr) (postsDir </> show postNo) $ S8.pack (show post)

-- | Get all the post id's
getAllPostIds :: MonadLIO DCLabel m => m [PostId]
getAllPostIds = do
  liftLIO $ do lcurr <- getLabel
               createDirectory lcurr postsDir `LIO.catch` 
                  (\(e::SomeException) -> return ())
  dirs <- getDirectoryContents postsDir
  let pids = sort $ filter (\fp -> fp `notElem` [".", ".."]) dirs
  return $ map read pids

instance ToJSON Post where
  toJSON post = object [ "postId"    .= pid
                       , "postTitle" .= postTitle post
                       , "postBody"  .= postBody post ]
   -- convert postId to string since Aeson numbers are not integral
   where pid = maybe Null (toJSON . show) $ postId post


--
-- Controller
--

app :: (SimpleApplication DC -> DC ()) -> DC ()
app runner = do
  nrPosts  <- length `liftM` getAllPostIds 
  settings <- newAppSettings nrPosts

  runner $ controllerApp settings $ do
    get "/" $ do
      posts   <- findAllPosts
      render "index.html" $ object [ "posts"   .= posts ]

    -- Respond to "/new"
    get "/new" $ do
      render "new.html" ()

    -- Repond to GET "/:id"
    get "/:id" $ routeTop $ do
      postId <- read `liftM` queryParam' "id"
      post   <- findPost postId
      render "show.html" post

    -- Create form
    post "/" $ do
      (params, _) <- parseForm
      let notNull = not . S8.null
          mpost = do
            title <- notNull `mfilter` lookup "title" params
            body  <- notNull `mfilter` lookup "body" params
            return $ Post { postId    = Nothing
                          , postTitle = S8.unpack title
                          , postBody  = S8.unpack body }
      case mpost of
        Just post -> insertPost post
        _         -> redirectBack 
      respond $ redirectTo "/"
