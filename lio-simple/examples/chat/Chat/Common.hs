{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Chat.Common where

import Prelude hiding (writeFile, readFile, appendFile)
import Control.Applicative
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.ByteString.Char8 as S8
import Control.Applicative
import Control.Monad (foldM)
import Web.Simple
import Web.Simple.Templates

import LIO
import LIO.DCLabel
import LIO.Web.Simple
import LIO.FS.Simple
import LIO.FS.Simple.DCLabel
import System.FilePath

import LIO.Concurrent.LMVar

import Data.Dynamic

data AppSettings = AppSettings { modelMap :: Map String Dynamic }

newAppSettings :: AppSettings
newAppSettings = AppSettings Map.empty

nextId :: MonadLIO DCLabel m => LMVar DCLabel Int -> m Int
nextId mv = liftLIO $ do
    cnt <- (+1) <$> takeLMVar mv
    putLMVar mv cnt
    return cnt

newModel :: Read t => String -> DC (Model t)
newModel name = liftLIO $ do
  let m = Model { modelNextId = undefined, modelName = name }
  oId   <- length `liftM` getAllIds m
  lcurr <- getLabel
  mv    <- newLMVar lcurr oId
  return $ m { modelNextId = nextId mv, modelName = name }

addModelToApp :: Typeable t => AppSettings -> Model t -> AppSettings
addModelToApp as m =
  let mm = Map.insert (modelName m) (toDyn m) $ modelMap as
  in as { modelMap = mm }

getModel :: Typeable t => String -> ControllerM AppSettings DC (Model t)
getModel name = do
 mm <- modelMap `liftM` controllerState
 return $ fromDyn  (mm Map.! name) $ error "Invalid type cast"

instance HasTemplates AppSettings DC where
  defaultLayout = Just <$> getTemplate "layouts/main.html"
  getTemplate = lioDefaultGetTemplate


--
-- Data model
--

type ObjId = Int

-- | Data type abstracting a model, @t@ is a phantom type
data Model t = Model { modelNextId :: ControllerM AppSettings DC ObjId
                     , modelName   :: FilePath } deriving Typeable


--
-- Simple DB-like interface
--

-- | Get object by id
findObj :: (Read t, MonadLIO DCLabel m) => Model t -> ObjId -> m t
findObj m oid = let objFile = "model" </> modelName m </> show oid
                in liftLIO $ (read . S8.unpack) `liftM` readFile objFile

-- | Get all objects
findAll :: (Read t, MonadLIO DCLabel m) => Model t -> m [t]
findAll m = do
  ids <- getAllIds m
  mapM (findObj m) ids

-- | Save object to file
insert :: Show t 
       => Model t -> (ObjId -> t) -> ControllerM AppSettings DC ObjId
insert m f = do
  oId <- modelNextId m
  let obj = f oId
  lcurr <- liftLIO getLabel
  writeFile (Just lcurr) ("model" </> modelName m </> show oId) $ 
    S8.pack (show obj)
  return oId

-- | Save object to file
update :: Show t => Model t -> t -> ObjId -> ControllerM AppSettings DC ()
update m obj oId = do
  lcurr <- liftLIO getLabel
  writeFile (Just lcurr) ("model" </> modelName m </> show oId) $ 
    S8.pack (show obj)

-- | Get all the post id's
getAllIds :: MonadLIO DCLabel m => Model t -> m [ObjId]
getAllIds m = do
  let modelDir = "model" </> modelName m
  liftLIO $ do lcurr <- getLabel
               createDirectory lcurr "model" `LIO.catch` 
                  (\(e::SomeException) -> return ())
               createDirectory lcurr modelDir `LIO.catch` 
                  (\(e::SomeException) -> return ())
  dirs <- getDirectoryContents modelDir
  let oids = List.sort $ filter (\fp -> fp `notElem` [".", ".."]) dirs
  return $ map read oids
