{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Blog.Common where

import Control.Applicative
import Web.Simple
import Web.Simple.Templates

import LIO
import LIO.DCLabel
import LIO.Web.Simple

import LIO.Concurrent.LMVar

data AppSettings = AppSettings { postNr :: LMVar DCLabel Int }

newAppSettings :: Int -> DC AppSettings
newAppSettings nr = do
  lcurr <- getLabel
  counter <- newLMVar lcurr nr
  return $ AppSettings { postNr = counter }

instance HasTemplates AppSettings DC where
  defaultLayout = Just <$> getTemplate "layouts/main.html"
  getTemplate = lioDefaultGetTemplate

getNextPostNr :: ControllerM AppSettings DC Int
getNextPostNr = do
  as <- controllerState
  let mv = postNr as
  liftLIO $ do
    cnt <- (+1) <$> takeLMVar mv
    putLMVar mv cnt
    return cnt
