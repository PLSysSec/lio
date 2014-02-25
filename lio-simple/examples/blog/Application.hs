{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Application where

import Prelude hiding (writeFile, readFile, appendFile)
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


data Post = Post { postTitle :: String
                 , postBody  :: String }
                 deriving (Show, Read, Eq)

app :: (SimpleApplication DC -> DC ()) -> DC ()
app runner = do
  settings <- newAppSettings

  runner $ controllerApp settings $ do
    get "/" $ do
      counter <- incCounter
      posts <- liftLIO $ do
        dataDir <- getDirectoryContents' "data"
        let postFiles = sort $
              filter (not . isPrefixOf ".") dataDir
        forM postFiles $ \postFile -> do
          obj <- (read . S8.unpack) `liftM` readFile ("data" </> postFile)
          return $ object ["id"     .= postFile
                          , "title" .= postTitle obj]
      lcurr <- liftLIO $ getLabel
      render "index.html" $ object [ "posts"   .= posts
                                   , "counter" .= counter
                                   , "label"   .= show lcurr]

    -- Respond to "/new"
    get "/new" $ do
      render "new.html" ()

    -- Repond to "/:post_id"
    get "/:post_id" $ routeTop $ do
      liftLIO $ do lclear <- getClearance 
                   taint lclear
      postId <- queryParam' "post_id"
      let postFile = "data" </> (takeFileName postId)
      post <- liftLIO $ do
        lcurr <- getLabel
        obj <- (read . S8.unpack) `liftM` readFile postFile
        return $ object ["title"   .= postTitle obj
                        , "body"   .= postBody obj
                        , "label"  .= show lcurr]
      render "show.html" post
    -- Create form
    post "/" $ do
      (params, _) <- parseForm
      let notNull = not . S8.null
      let mpost = do
            title <- notNull `mfilter` lookup "title" params
            body <- notNull `mfilter` lookup "body" params
            return $ Post { postTitle = S8.unpack title,
                            postBody  = S8.unpack body }
      case mpost of
        Nothing -> redirectBack
        Just post -> liftLIO $ do
          files <- filter (\(c:_) -> c /= '.') `fmap`
            getDirectoryContents' "data"
          let lastFileNum = show $ length files + 1
          let fileName =
                take (5 - length lastFileNum)
                  [z | _ <- [0..], let z = '0'] ++
                lastFileNum
          lcurr <- liftLIO getLabel
          writeFile (Just lcurr) ("data" </> fileName) $ S8.pack (show post)
      respond $ redirectTo "/"

  where getDirectoryContents' dir = do
          lcurr <- getLabel
          createDirectory lcurr dir
            `LIO.catch` (\(e::SomeException) -> return ())
          getDirectoryContents dir

