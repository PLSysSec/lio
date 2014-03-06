{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Application where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Chat.Common
import Data.Aeson
import Data.Typeable
import qualified Data.ByteString.Char8 as S8
import qualified Data.Maybe as Maybe
import Network.HTTP.Types
import Web.Simple
import Web.Simple.Templates
import Web.Simple.Auth
import Web.Frank

import LIO
import LIO.DCLabel

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
  genLabel post = return dcPublic

instance ToJSON Group where
  toJSON group = object 
     [ "groupId"      .= gid
     , "groupName"    .= (S8.unpack $ groupName group)
     , "groupMembers" .= (map S8.unpack $ groupMembers group)
     , "groupPosts"   .= (map show $ groupPosts group) ]
   -- convert groupId to string since Aeson numbers are not integral
   where gid = maybe Null (toJSON . show) $ groupId group

saveGroup :: Model Group -> Group -> ControllerM AppSettings DC ObjId
saveGroup m g = case groupId g of
                    Just gid -> update m g gid >> return gid
                    _        -> insert m (\gid -> g { groupId = Just gid })

--
-- Logs
--

-- | Datatype representing a chat log
data Post = Post { postId      :: Maybe ObjId
                 , postBody    :: S8.ByteString
                 , postAuthor  :: UserName
                 , postGroupId :: ObjId
                 } deriving (Show, Read, Eq, Typeable)


instance LabelPolicy Post where
  genLabel post = do
    groupModel <- getModel "group"
    group <- findObj groupModel $ postGroupId post
    let us = toCNF . dFromList . map principalBS . groupMembers $ group
    return $ us %% cTrue


instance ToJSON Post where
  toJSON post = object [ "postId"      .= pid
                       , "postBody"    .= (S8.unpack $ postBody post)
                       , "postAuthor"  .= (S8.unpack $ postAuthor post)
                       , "postGroupId" .= (show $ postGroupId post) ]
   -- convert postId to string since Aeson numbers are not integral
   where pid = maybe Null (toJSON . show) $ postId post

savePost :: Model Post -> Post -> ControllerM AppSettings DC ObjId
savePost m p = case postId p of
                   Just pid -> update m p pid >> return pid
                   _        -> insert m (\pid -> p { postId = Just pid })

--
-- Controller
--

app :: (SimpleApplication DC -> DC ()) -> DC ()
app runner = do
  groupModel <- liftLIO $ newModel "group"
  postModel  <- liftLIO $ newModel "post"

  let settings = newAppSettings `addModelToApp` groupModel
                                `addModelToApp` postModel

  runner $ controllerApp settings $ do
    get "/" $ do
      user  <- currentUser
      render "index.html" $ object [ "user" .= S8.unpack user ]

    --
    -- Groups
    --

    get "/group" $ do
      groups <- findAll groupModel
      render "group/index.html" $ object [ "groups"   .= groups ]

    -- Respond to "/new"
    get "/group/new" $ do
      render "group/new.html" ()

    -- Repond to GET "/group/:id"
    get "/group/:id" $ routeTop $ do
      user  <- currentUser
      gId   <- read `liftM` queryParam' "id"
      group <- findObj groupModel gId
      posts <- findAll postModel
      let posts' = filter ((== gId) . postGroupId) posts
      render "group/show.html" $ object [ "user"    .= S8.unpack user
                                        , "group"   .= group
                                        , "posts"   .= posts' ]

    -- Create new group
    post "/group" $ do
      user <- currentUser
      (params, _) <- parseForm
      let mgroup = do
            name     <- notNull `mfilter` lookup "name" params
            members' <- lookup "members" params
            let members = user : (filter notNull $ S8.split ',' members')
            return $ Group { groupId      = Nothing
                           , groupName    = name
                           , groupMembers = members 
                           , groupPosts   = [] }
      case mgroup of
        Just group -> do gId <- saveGroup groupModel group
                         respond $ redirectTo $ S8.pack $ "/group/" ++ show gId
        _          -> redirectBack 

    -- Create new post
    post "/group/:gid/newpost" $ do
      user <- currentUser
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
          group <- findObj groupModel gId
          pid <- savePost postModel post
          void $ saveGroup groupModel $ 
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
