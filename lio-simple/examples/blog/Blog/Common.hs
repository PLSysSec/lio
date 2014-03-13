{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Blog.Common where

import Control.Monad
import Control.Applicative

import Data.Text.Encoding
import Web.Simple.Templates
import Web.Simple.Templates.Language

import LIO
import LIO.DCLabel
import LIO.Web.Simple
import LIO.Web.Simple.DCLabel
import LIO.Concurrent.LMVar

import qualified LIO.FS.Simple as FS

data AppSettings = AppSettings { postNr :: LMVar DCLabel Int }

newAppSettings :: Int -> DC AppSettings
newAppSettings nr = do
  lcurr <- getLabel
  counter <- newLMVar lcurr nr
  return $ AppSettings { postNr = counter }

instance HasTemplates DC AppSettings where
  defaultLayout = Just <$> getTemplate "layouts/main.html"
  getTemplate = lioDefaultGetTemplate

getNextPostNr :: DCController AppSettings Int
getNextPostNr = do
  as <- controllerState
  let mv = postNr as
  liftLIO $ do
    cnt <- (+1) <$> takeLMVar mv
    putLMVar mv cnt
    return cnt

-- | Function to use to get a template. When the underlying monad is
-- 'LIO', it looks in the 'viewDirectory' for the given file name and
-- compiles the file into a template.
--
-- Make sure that you execute the application within a 'withLIOFS'
-- block.
--
-- To ensure that all the files in the 'viewDirector' are (publicly)
-- labeled use 'labelDirectoryRecursively'.
lioDefaultGetTemplate :: Label l => FilePath -> LIOController l hs Template
lioDefaultGetTemplate fp = do
  eres <- compileTemplate . decodeUtf8 <$> liftLIO (liftLIO $ FS.readFile fp)
  case eres of
    Left str -> fail str
    Right tmpl -> return tmpl
