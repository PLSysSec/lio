{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Data.Maybe
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.ByteString.Char8 as S8
import Data.Text.Encoding
import Control.Applicative
import Control.Monad (foldM)

import LIO.Web.Simple
import LIO.Web.Simple.DCLabel
import Web.Simple.Templates.Language

import LIO
import LIO.LIORef
import LIO.DCLabel
import LIO.Run (privInit)
import LIO.TCB (ioTCB)
import LIO.Web.Simple
import LIO.Web.Simple.TCB (lioGetTemplateTCB)

import System.FilePath

import LIO.Concurrent.LMVar

import Data.Dynamic

data AppSettings = AppSettings { modelMap :: Map String Dynamic }

newAppSettings :: AppSettings
newAppSettings = AppSettings Map.empty

newEmptyModel :: (MonadLIO DCLabel m, Read t) => String -> m (Model t)
newEmptyModel name = liftLIO $ do
  lcurr <- getLabel
  mv    <- newLMVar lcurr 0
  db    <- newLMVar lcurr Map.empty
  return $ Model { modelNextId = nextId mv, modelDB = db, modelName = name }

nextId :: LMVar DCLabel Int -> DCPriv -> DC Int
nextId mv priv = do
    cnt <- (+1) <$> takeLMVarP priv mv
    putLMVarP priv mv cnt
    return cnt

addModelToApp :: Typeable t => AppSettings -> Model t -> AppSettings
addModelToApp as m =
  let mm = Map.insert (modelName m) (toDyn m) $ modelMap as
  in as { modelMap = mm }

getModel :: Typeable t => String -> DCController AppSettings (Model t)
getModel name = do
 mm <- modelMap `liftM` controllerState
 return $ fromDyn  (mm Map.! name) $ error "Invalid type cast"

instance HasTemplates DC AppSettings where
  viewDirectory = return "public/views"
  defaultLayout = Just <$> getTemplate "public/layouts/main.html"
  getTemplate = liftLIO . lioGetTemplateTCB


--
-- Data model
--

type DCMVar a = LMVar DCLabel a
type DCRef a = LIORef DCLabel a

-- | Object Ids. Use random integers when considering malicious code.
type ObjId = Int

-- | Data type abstracting a model
data Model t = Model { modelNextId :: DCPriv -> DC ObjId
                     , modelDB     :: DCMVar (Map ObjId (DCRef t))
                     , modelName   :: String  } deriving Typeable

{-
NB: if we didn't have nextId then, on insert, we would take a lock on
the whole "DB" and if the label function diverges the whole app
deadlocks.
-}

--
-- Simple DB-like interface
--

class LabelPolicy t where
  genLabel :: DCPriv -> t -> DCController AppSettings DCLabel

-- | Get object by id. The function throws an exception if the label of the
-- object is above the current label.
findObj :: (Read t, MonadLIO DCLabel m) => Model t -> ObjId -> m t
findObj = findObjP mempty

findObjP :: (Read t, MonadLIO DCLabel m) => DCPriv -> Model t -> ObjId -> m t
findObjP priv m oId = liftLIO $ do
  db <- readLMVarP priv $ modelDB m
  readLIORefP priv $ db Map.! oId


-- | Get all objects below the current clearance.
findAll :: (Read t, MonadLIO DCLabel m) => Model t -> m [t]
findAll = findAllP mempty

findAllP :: (Read t, MonadLIO DCLabel m) => DCPriv -> Model t -> m [t]
findAllP priv m = liftLIO $ do
  ids <- getAllIdsP priv m
  catMaybes `liftM` mapM findObj' ids
  where findObj' i = do
          (Just `liftM` findObjP priv m i) `LIO.catch` 
                  (\(e::SomeException) -> return Nothing)
 

-- | Save object to file.
insert :: (Show t, LabelPolicy t)
       => Model t -> (ObjId -> t) -> DCController AppSettings ObjId
insert = insertP mempty

insertP :: (Show t, LabelPolicy t)
       => DCPriv -> Model t -> (ObjId -> t) -> DCController AppSettings ObjId
insertP priv m f = do
  oId  <- liftLIO $ modelNextId m priv 
  let obj = f oId
  lobj <- genLabel priv obj
  liftLIO $ do
    lref <- newLIORefP priv lobj obj
    db   <- takeLMVarP priv $ modelDB m
    putLMVarP priv (modelDB m) $ Map.insert oId lref db
    return oId

-- | Save object to file; file elready exists
update :: (Show t, MonadLIO DCLabel m)
       => Model t -> t -> ObjId -> m ()
update = updateP mempty

updateP :: (Show t, MonadLIO DCLabel m)
        => DCPriv -> Model t -> t -> ObjId -> m ()
updateP priv m obj oId = liftLIO $ do
  db <- readLMVarP priv $ modelDB m
  writeLIORefP priv (db Map.! oId) obj

-- | Get all the post id's
getAllIds :: MonadLIO DCLabel m => Model t -> m [ObjId]
getAllIds = getAllIdsP mempty

getAllIdsP :: MonadLIO DCLabel m => DCPriv -> Model t -> m [ObjId]
getAllIdsP priv m = liftLIO $ do
  db <- readLMVarP priv $ modelDB m
  return $ Map.keys db


--
-- Users
--

type UserName = S8.ByteString

currentUser :: DCController r UserName
currentUser = do
  mu <- requestHeader "X-User"
  maybe (fail "User not logged-in") return mu

--
-- Groups
--

type GroupName = UserName

-- | Datatype representing a chat group
data Group = Group { groupId      :: Maybe ObjId
                   , groupName    :: GroupName
                   , groupMembers :: [UserName]
                   } deriving (Show, Read, Eq, Typeable)

instance LabelPolicy Group where
  genLabel _ group = 
    let c = case groupId group of
              Just gid -> ("#group-" ++ show gid) \/ admin
              _        -> cTrue
    in return $ c %% c

admin :: Principal
admin = principalBS "admin"

-- | Return the privileges corresponding to the groups that the user
-- is a member of. This fetches all the groups, so it's obviously very
-- slow, but since this is a toy application we will not optimize this.
getGroupPriv :: UserName -> Gate CNF (DCController AppSettings DCPriv)
getGroupPriv user = guardGate "getGroupPrivs" (toCNF $ principalBS user) $ do
  groupModel <- getModel "group"
  adminPriv  <- liftLIO . ioTCB . privInit $ toCNF admin
  groups     <- filter (elem user . groupMembers) `liftM` 
                  findAllP adminPriv groupModel
  let f group c = c /\ (("#group-"++ (show . fromJust . groupId $ group)) \/ admin)
  return $ delegate adminPriv $ foldr f cTrue groups

-- | Create a new group
createNewGroup :: DCPriv -> Group -> DCController AppSettings ObjId
createNewGroup upriv group' = do
  groupModel <- getModel "group"
  gId <- liftLIO $ modelNextId (groupModel :: Model Group) upriv
  let group = group' { groupId = Just gId }
  lgroup <- genLabel upriv group
  liftLIO $ do
    priv<- mappend upriv `liftM` 
               (ioTCB . privInit $ admin \/ ("#group-" ++ show gId))
    clr  <- getClearance
    withClearanceP priv (clr `lub` (privDesc priv %% cTrue)) $ do
        lref <- newLIORefP priv lgroup group
        db   <- takeLMVarP priv $ modelDB groupModel
        putLMVarP priv (modelDB groupModel) $ Map.insert gId lref db
    return gId

--
-- Posts
--

-- | Datatype representing a chat post
data Post = Post { postId      :: Maybe ObjId
                 , postBody    :: S8.ByteString
                 , postAuthor  :: UserName
                 , postGroupId :: ObjId
                 } deriving (Show, Read, Eq, Typeable)

instance LabelPolicy Post where
  genLabel priv post = do
    groupModel <- getModel "group"
    group <- findObjP priv groupModel $ postGroupId post
    -- Use the same label as the group
    genLabel priv (group :: Group)
