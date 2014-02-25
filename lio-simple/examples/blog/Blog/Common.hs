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

data AppSettings = AppSettings { reqCounter :: LMVar DCLabel Int }

newAppSettings :: DC AppSettings
newAppSettings = do
  lcurr <- getLabel
  counter <- newLMVar lcurr 0
  return $ AppSettings { reqCounter = counter }

instance HasTemplates AppSettings DC where
  defaultLayout = Just <$> getTemplate "layouts/main.html"
  getTemplate = lioDefaultGetTemplate

incCounter :: ControllerM AppSettings DC Int
incCounter = do
  as <- controllerState
  let mv = reqCounter as
  liftLIO $ do
    cnt <- (+1) <$> takeLMVar mv
    putLMVar mv cnt
    return cnt
