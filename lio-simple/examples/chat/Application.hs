{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Application where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Chat.Common
import Data.Aeson
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
-- Data model
--

class LabelPolicy t where
  genLabel :: Model t -> t -> DCLabel

--
-- Users
--

type UserName = S8.ByteString

-- | Login without any username/passsword checking
withLogin :: MonadLIO DCLabel m
          => ControllerM r m a -> ControllerM r m a
withLogin = authRewriteReq (basicAuthRoute "chat") (\u p -> return True)


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
                   } deriving (Show, Read, Eq)

instance ToJSON Group where
  toJSON group = object 
     [ "groupId"      .= gid
     , "groupName"    .= (S8.unpack $ groupName group)
     , "groupMembers" .= (map S8.unpack $ groupMembers group)
     , "groupPosts"   .= (map show $ groupPosts group) ]
   -- convert groupId to string since Aeson numbers are not integral
   where gid = maybe Null (toJSON . show) $ groupId group

instance LabelPolicy Group where
  genLabel _ l = 
    let us = toCNF . dFromList . map principalBS . groupMembers $ l 
    in  us %% us

saveGroup :: Model Group -> Group -> ControllerM AppSettings DC ObjId
saveGroup m g = case groupId g of
                    Just gid -> update m g gid >> return gid
                    _        -> insert m (\gid -> g { groupId = Just gid })

--
-- Logs
--

-- | Datatype representing a chat log
data Post = Post { postId      :: Maybe ObjId
                 , postTitle   :: S8.ByteString
                 , postBody    :: S8.ByteString
                 , postAuthor  :: UserName
                 , postGroupId :: ObjId
                 } deriving (Show, Read, Eq)

--instance LabelPolicy Log where
--  genLabel l = let us = toCNF . dFromList . map principal . logOwners $ l 
--               in  us %% us


instance ToJSON Post where
  toJSON post = object [ "postId"      .= pid
                       , "postTitle"   .= (S8.unpack $ postTitle post)
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
  settings <- newAppSettings
  groupModel <- newModel "group"
  postModel  <- newModel "post"

  runner $ controllerApp settings $ withLogin $ do
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
      gId   <- read `liftM` queryParam' "id"
      group <- findObj groupModel gId
      posts <- findAll postModel
      let posts' = filter ((== gId) . postGroupId) posts
      render "group/show.html" $ object [ "group"   .= group
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
        Just group -> void $ saveGroup groupModel group
        _          -> redirectBack 
      respond $ redirectTo "/"

    --
    -- Posts
    --

    get "/post/:gid" $ do
      gId   <- read `liftM` queryParam' "gid"
      posts <- findAll postModel
      let posts' = filter ((==gId) . postGroupId) posts
      render "post/index.html" $ object [ "posts"   .= posts' ]

    -- Respond to "/new"
    get "/post/:gid/new" $ do
      gId <- read `liftM` queryParam' "gid"
      render "post/new.html" $ object [ "groupId" .= show (gId :: Int)]

    -- Repond to GET "/post/:id"
    get "/post/:gid/:pid" $ routeTop $ do
      pId   <- read `liftM` queryParam' "pid"
      post <- findObj postModel pId
      render "post/show.html" post

    -- Create new post
    post "/post/:gid" $ do
      gId   <- read `liftM` queryParam' "gid"
      user <- currentUser
      (params, _) <- parseForm
      let mpost = do
            title    <- notNull `mfilter` lookup "title" params
            body     <- notNull `mfilter` lookup "body" params
            return $ Post { postId      = Nothing
                          , postTitle   = title
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
      respond $ redirectTo "/"

--
-- Helpers
--

notNull :: S8.ByteString -> Bool
notNull = not . S8.null

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . Maybe.listToMaybe . reads
