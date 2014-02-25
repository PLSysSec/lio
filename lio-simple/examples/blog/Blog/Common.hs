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

data AppSettings = AppSettings {  }

newAppSettings :: DC AppSettings
newAppSettings = do
  return $ AppSettings


instance HasTemplates AppSettings DC where
  defaultLayout = Just <$> getTemplate "layouts/main.html"
  getTemplate = lioDefaultGetTemplate
