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
import Control.Applicative
import Control.Monad (foldM)
import Web.Simple
import Web.Simple.Templates
import LIO.Run (privInit)
import LIO.TCB (ioTCB)

import LIO
import qualified LIO as LIO
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
  let m = Model { modelName   = name
                , modelNextId = undefined
                , modelLocks  = undefined }
  oId   <- length `liftM` getAllIds m
  lcurr <- getLabel
  mv    <- newLMVar lcurr oId
  lock  <- newLMVar lcurr Map.empty
  return $ m { modelNextId = nextId mv
             , modelName   = name 
             , modelLocks  = lock }

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

-- | Object Ids. Use random integers when considering malicious code.
type ObjId = Int

-- | Lock map. When updating an object, we lock it to ensure that
-- concurrent requests do not modify the same object.
type LockMap = LMVar DCLabel (Map ObjId (LMVar DCLabel ()))


-- | Data type abstracting a model, @t@ is a phantom type
data Model t = Model { modelNextId :: DC ObjId
                     , modelName   :: FilePath 
                     , modelLocks  :: LockMap
                     } deriving Typeable


--
-- Simple DB-like interface
--

class LabelPolicy t where
  genLabel :: t -> DCLabel

-- | Get object by id. The function throws an exception if the label of the
-- object is above the current label.
findObj :: (Read t, MonadLIO DCLabel m) => Model t -> ObjId -> m t
findObj = findObjP mempty

findObjP :: (Read t, MonadLIO DCLabel m) => DCPriv -> Model t -> ObjId -> m t
findObjP priv m oid = let objFile = "model" </> modelName m </> show oid
                      in liftLIO $ (read . S8.unpack) `liftM` readFileP priv objFile


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
insert :: (Show t, LabelPolicy t, MonadLIO DCLabel m)
       => Model t -> (ObjId -> t) -> m ObjId
insert = insertP mempty

insertP :: (Show t, LabelPolicy t, MonadLIO DCLabel m)
       => DCPriv -> Model t -> (ObjId -> t) -> m ObjId
insertP priv m f = do
  oId <- liftLIO $ modelNextId m
  let obj  = f oId
      lobj = genLabel obj
  writeFileP priv (Just lobj) ("model" </> modelName m </> show oId) $ 
    S8.pack (show obj)
  return oId

-- | Save object to file; file elready exists
update :: (MonadLIO DCLabel m, Show t)
       => Model t -> t -> ObjId -> m ()
update = updateP mempty

updateP :: (MonadLIO DCLabel m, Show t) 
        => DCPriv -> Model t -> t -> ObjId -> m ()
updateP priv m obj oId = do
  writeFileP priv Nothing ("model" </> modelName m </> show oId) $ 
    S8.pack (show obj)

-- | Get all the object id's of a model
getAllIds :: MonadLIO DCLabel m => Model t -> m [ObjId]
getAllIds = getAllIdsP mempty

getAllIdsP :: MonadLIO DCLabel m => DCPriv -> Model t -> m [ObjId]
getAllIdsP priv m = do
  let modelDir = "model" </> modelName m
  liftLIO $ do lcurr <- getLabel
               createDirectoryP priv lcurr "model" `LIO.catch` 
                  (\(e::SomeException) -> return ())
               createDirectoryP priv lcurr modelDir `LIO.catch` 
                  (\(e::SomeException) -> return ())
  dirs <- getDirectoryContentsP priv modelDir
  let oids = List.sort $ filter (\fp -> fp `notElem` [".", ".."]) dirs
  return $ map read oids

-- | Execute an action, holding a lock on the object ID.
withObjId :: MonadLIO DCLabel m => Model t -> ObjId -> DC a -> m a
withObjId = withObjIdP mempty

withObjIdP :: MonadLIO DCLabel m => DCPriv -> Model t -> ObjId -> DC a -> m a
withObjIdP priv m oId act = liftLIO $ do
  -- 1. Get (create if doesn't exist) the object lock
  lockMap <- takeLMVarP priv $ modelLocks m
  (olock, lockMap') <- case Map.lookup oId lockMap of
    Just olock -> return (olock, lockMap)
    Nothing -> do olock <- doit $ \p l -> newLMVarP p l ()
                  return (olock, Map.insert oId olock lockMap)
  putLMVarP priv (modelLocks m) lockMap'
  LIO.bracket 
    (doit $ \p _ -> void $ takeLMVarP p olock)    -- 2. Aquire object lock
    (const . doit $ \p _ -> putLMVarP p olock ()) -- 4. Release object lock
    (const act)                                   -- 3. Perform action
  where doit act = do
          let c = admin \/ principalBS "#objectLock"
              l = c %% c
          lpriv <- liftLIO . ioTCB . privInit $ c
          clr  <- getClearance
          withClearanceP (priv `mappend` lpriv) (clr `lub` l) $ do
           act (priv `mappend` lpriv) l

--
-- Users
--

type UserName = S8.ByteString

currentUser :: MonadLIO DCLabel m => ControllerM r m UserName
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
                   , groupPosts   :: [ObjId]
                   } deriving (Show, Read, Eq, Typeable)

instance LabelPolicy Group where
  genLabel group = 
    let c = case groupId group of
              Just gid -> ("#group-" ++ show gid) \/ admin
              _        -> cTrue
    in c %% c

admin :: Principal
admin = principalBS "admin"

-- | Return the privileges corresponding to the groups that the user
-- is a member of. This fetches all the groups, so it's obviously very
-- slow, but since this is a toy application we will not optimize this.
getGroupPriv :: UserName -> Gate CNF (ControllerM AppSettings DC DCPriv)
getGroupPriv user = guardGate "getGroupPrivs" (toCNF $ principalBS user) $ do
  groupModel <- getModel "group"
  adminPriv  <- liftLIO . ioTCB . privInit $ toCNF admin
  groups     <- filter (elem user . groupMembers) `liftM` 
                  findAllP adminPriv groupModel
  let f group c = c /\ (("#group-"++ (show . fromJust . groupId $ group)) \/ admin)
  return $ delegate adminPriv $ foldr f cTrue groups

-- | Create a new group
createNewGroup :: DCPriv -> Group -> ControllerM AppSettings DC ObjId
createNewGroup upriv group' = do
  groupModel <- getModel "group"
  gId <- liftLIO $ modelNextId (groupModel :: Model Group)
  let group = group' { groupId = Just gId }
      lgroup = genLabel group
  liftLIO $ do
    priv<- mappend upriv `liftM` 
               (ioTCB . privInit $ admin \/ ("#group-" ++ show gId))
    clr  <- getClearance
    withClearanceP priv (clr `lub` (privDesc priv %% cTrue)) $ do
      writeFileP priv (Just lgroup) 
                 ("model" </> modelName groupModel </> show gId) $ S8.pack (show group)
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
  genLabel post = let c = ("#group-" ++ (show . postGroupId $ post)) \/ admin
                  in  c %% c
