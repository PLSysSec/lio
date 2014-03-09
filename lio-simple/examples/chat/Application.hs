{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Application where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Chat.Common
import Data.Monoid
import Data.Aeson
import Data.Typeable
import qualified Data.ByteString.Char8 as S8
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Network.HTTP.Types
import Web.Simple
import Web.Simple.Templates
import Web.Simple.Auth
import Web.Frank

import LIO
import LIO.DCLabel
import LIO.Web.Simple
import LIO.Web.Simple.DCLabel


--
-- Groups
--

instance ToJSON Group where
  toJSON group = object
     [ "groupId"      .= gid
     , "groupName"    .= (S8.unpack $ groupName group)
     , "groupMembers" .= (S8.unpack . S8.unwords $ groupMembers group)
     , "groupPosts"   .= (map show $ groupPosts group) ]
   -- convert groupId to string since Aeson numbers are not integral
   where gid = maybe Null (toJSON . show) $ groupId group

saveGroup :: DCPriv -> Model Group -> Group -> ControllerM AppSettings DC ObjId
saveGroup priv m g = case groupId g of
  Just gid -> updateP priv m g gid >> return gid
  _        -> createNewGroup priv g

--
-- Posts
--


instance ToJSON Post where
  toJSON post = object [ "postId"      .= pid
                       , "postBody"    .= (S8.unpack $ postBody post)
                       , "postAuthor"  .= (S8.unpack $ postAuthor post)
                       , "postGroupId" .= (show $ postGroupId post) ]
   -- convert postId to string since Aeson numbers are not integral
   where pid = maybe Null (toJSON . show) $ postId post

savePost :: DCPriv -> Model Post -> Post -> ControllerM AppSettings DC ObjId
savePost priv m p = case postId p of
  Just pid -> updateP priv m p pid >> return pid
  _        -> insertP priv m (\pid -> p { postId = Just pid })

--
-- Controller
--

app :: (SimpleDCApplication -> DC ()) -> DC ()
app runner = do
  groupModel <- liftLIO $ newModel "group"
  postModel  <- liftLIO $ newModel "post"

  let settings = newAppSettings `addModelToApp` groupModel
                                `addModelToApp` postModel

  runner $ \upriv -> controllerApp settings $ withUser upriv $ \user priv -> do
    get "/" $ do
      user  <- currentUser
      render "index.html" $ object [ "user" .= S8.unpack user ]

    --
    -- Groups
    --

    get "/group" $ do -- withUser upriv $ \user priv -> do
      groups <- findAllP priv groupModel
      render "group/index.html" $ object [ "groups" .= groups ]

    -- Respond to "/new"
    get "/group/new" $ do
      render "group/new.html" ()

    -- Repond to GET "/group/:id"
    get "/group/:id" $ do -- withUser upriv $ \user priv -> do
      gId   <- read `liftM` queryParam' "id"
      group <- findObjP priv groupModel gId
      posts <- findAllP priv postModel
      let posts' = filter ((== gId) . postGroupId) posts
      render "group/show.html" $ object [ "user"    .= S8.unpack user
                                        , "group"   .= group
                                        , "posts"   .= posts' ]

    -- Create new group
    post "/group" $ do -- withUser upriv $ \user priv -> do
      (params, _) <- parseForm
      let mgroup = do
            name     <- notNull `mfilter` lookup "name" params
            members' <- lookup "members" params
            let members = user : S8.words members'
            return $ Group { groupId      = Nothing
                           , groupName    = name
                           , groupMembers = List.nub $ members
                           , groupPosts   = [] }
      case mgroup of
        Just group -> do gId <- saveGroup priv groupModel group
                         respond $ redirectTo $ S8.pack $ "/group/" ++ show gId
        _          -> redirectBack

    -- Repond to GET "/group/:id/edit"
    get "/group/:id/edit" $ do -- withUser upriv $ \user priv -> do
      gId   <- read `liftM` queryParam' "id"
      group <- findObjP priv groupModel gId
      render "group/edit.html" $ 
        group { groupMembers = List.delete user $ groupMembers group }

    -- Update group
    post "/group/:id" $ do 
      gId   <- read `liftM` queryParam' "id"
      group <- findObjP priv groupModel gId
      (params, _) <- parseForm
      let name = maybe (groupName group) id $ 
                   notNull `mfilter` lookup "name" params
          members = maybe (groupMembers group) 
                          (\m -> user : S8.words m) $ lookup "members" params
      void $ saveGroup priv groupModel $ 
        group { groupName = name, groupMembers = List.nub $ members }
      respond $ redirectTo "/group"

    -- Create new post
    post "/group/:gid/newpost" $ do -- withUser upriv $ \user priv -> do
      gId  <- read `liftM` queryParam' "gid"
      (params, _) <- parseForm
      let mpost = do
            body     <- notNull `mfilter` lookup "body" params
            return $ Post { postId      = Nothing
                          , postBody    = body
                          , postAuthor  = user
                          , postGroupId = gId }
      case mpost of
        Just post -> do
          group <- findObjP priv groupModel gId
          pid <- savePost priv postModel post
          void $ saveGroup priv groupModel $
            group { groupPosts = groupPosts group ++ [pid] }
        _         -> redirectBack
      redirectBack

--
-- Helpers
--

notNull :: S8.ByteString -> Bool
notNull = not . S8.null

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . Maybe.listToMaybe . reads

withUser :: DCPriv 
         -> (UserName -> DCPriv -> ControllerM AppSettings DC a)
         -> ControllerM AppSettings DC a
withUser upriv act = do
  user <- currentUser
  priv <- mappend upriv `liftM` callGate (getGroupPriv user) upriv
  -- raise clearance
  -- since the action is in a lifted monad, we cannot generally
  -- restore the clearance, but runner does this anyway
  clr <- liftLIO getClearance
  liftLIO $ setClearanceP priv $ clr `lub` (privDesc priv %% cTrue)
  act user priv
