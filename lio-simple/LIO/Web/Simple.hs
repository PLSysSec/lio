{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
 
 This module defines several instances needed to use 'LIO' as the
 underlying monad with "Web.Simple", the simple web framework.
 Additionally, we provide some middleware for executing apps in a safe
 manner.

 "LIO.Web.Simple.TCB" defines several functions that can be used to
 execute 'LIO' web applications with the Warp server.

-}
module LIO.Web.Simple (
    -- * Templates
    lioDefaultGetTemplate
--     -- * Static
--   , lioServeStatic
    -- * Helpers
  , labelDirectoryRecursively 
    -- * Middleware
  , browserLabelGuard
  , removeRequestHeaders
  , removeResponseHeaders 
  ) where

import Prelude hiding (readFile)

import safe Control.Monad
import safe Control.Applicative

import Data.Text.Encoding

import safe Web.Simple
import safe Web.Simple.Templates.Language

import safe LIO
import LIO.TCB (ioTCB)
import safe LIO.FS.Simple
import LIO.Web.Simple.TCB

import Network.Wai.Parse

import Data.Conduit
import qualified Data.Conduit.List as CL

import safe qualified Data.ByteString.Lazy.Char8 as L8

import safe System.FilePath
import safe System.Posix.Files
import safe qualified System.Directory as IO
import safe qualified System.IO.Error as IO

import LIO.FS.TCB (setPathLabelTCB)

--
-- Simple
--

-- | Function to use to get a template. When the underlying monad is
-- 'LIO', it looks in the 'viewDirectory' for the given file name and
-- compiles the file into a template. This can be overriden to, for
-- example, cache compiled templates in memory.
--
-- Make sure that you execute the application within a 'withLIOFS'
-- block.
--
-- To ensure that all the files in the 'viewDirector' are (publicly)
-- labeled use 'labelDirectoryRecursively'.
lioDefaultGetTemplate :: Label l => FilePath -> ControllerM hs (LIO l) Template
lioDefaultGetTemplate fp = do
  eres <- compileTemplate . decodeUtf8 <$> liftLIO (liftLIO $ readFile fp)
  case eres of
    Left str -> fail str
    Right tmpl -> return tmpl


-- | Provide an instance for MonadController in the LIO monad. Note
-- that th 'body' implementation consumes the body from a Source IO
-- Bytestring. Since the 'Request' constructor is exposed by
-- "Network.Wai.Internal", it's important to disallow construction of
-- such values when considering untrusted code.
instance Label l => MonadController (LIO l) where
  parseForm = do
    req <- request
    liftLIO . ioTCB $ parseRequestBody lbsBackEnd req
  body = do
    req <- request
    liftLIO . ioTCB $ L8.fromChunks `fmap` (requestBody req $$ CL.consume)
  liftController = liftLIO

instance Label l => MonadLIO l (ControllerM r (LIO l)) where
  liftLIO act = ControllerM $ \st -> 
      liftLIO act >>= \r -> return (Right r, st)

--
-- Static
--

-- -- | Serve static data.
-- -- 
-- -- NOTE: when executing an application it is important to ensure that
-- -- this function is the only one that is used to serve static data
-- -- (otherwise 'responseFile' may be used to leak arbitrary files).
-- --
-- lioServeStatic :: Label l => FilePath -> ControllerM r (LIO l) ()
-- lioServeStatic baseDir = do
--   req <- request
--   let fp = foldl (</>) baseDir (map T.unpack $ pathInfo req)
--   file <- liftLIO $ cleanUpPath fp
--   let containingDir = takeDirectory file
--       fName         = takeFileName  file
--   -- Taint up to containing dir:
--   path   <- liftLIO $ taintObjPathP noPrivs containingDir
--   let objPath = path </> fName
--   exists <- liftLIO . ioTCB $ doesFileExist objPath
--   when exists $ do
--     -- Get label of file:
--     l <- liftLIO . ioTCB $ getPathLabelTCB objPath
--     -- Make sure we can read from the file:
--     liftLIO $ taint l
--     liftLIO $ ioTCB $ do
--       modTime <- getModificationTime objPath
--       print modTime
--     respond $ responseFile status200
--       [(hContentType, defaultMimeLookup $ T.pack $ takeFileName objPath)]
--       fp Nothing

--
-- Helpers
--

-- | Label the directory and every file within recursively with the
-- supplied label. Note this funciton expects a full path.
labelDirectoryRecursively :: Label l => l -> FilePath -> IO ()
labelDirectoryRecursively l dir = do
  exists <- IO.doesDirectoryExist dir
  unless exists $ throwIO $ IO.mkIOError IO.doesNotExistErrorType
                                        ctx Nothing (Just dir)
  setPathLabelTCB dir l
  fs <- filter (\f -> f `notElem` [".", ".."]) <$> IO.getDirectoryContents dir
  forM_ fs $ \f -> do
    let file = dir </> f
    stat <- getFileStatus file
    case () of
      _ | isRegularFile stat -> setPathLabelTCB file l
      _ | isDirectory stat   -> labelDirectoryRecursively l file
      _ -> throwIO $ IO.mkIOError IO.illegalOperationErrorType ctx 
                                  Nothing (Just file)

  where ctx = "labelDirectoryRecursively"
