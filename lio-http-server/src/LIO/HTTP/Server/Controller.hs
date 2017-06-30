{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

{- | 'Controller' provides a convenient syntax for writting
   'Application' code as a Monadic action with access to an HTTP
   request as well as app specific data (e.g., a database connection
   pool, app configuration, etc.). The convenience comes from the
   helper functions that this module exports.  For example,
   'redirectBack' reads the underlying request to extract the referer
   and returns a redirect response:

  @
    myController = do
      ...
      if badLogin
        then 'redirectBack'
        else 'respond' $ 'okHtml' "w00t!"
  @
-}
module LIO.HTTP.Server.Controller (
  -- * Request relate accessors
  request,
  requestHeader,
  queryParams, Parseable(..),
  -- * Response related accessors
  respond,
  redirectBack,
  redirectBackOr,
  -- * App-specific logging
  log,
  Logger(..), LogLevel(..),
  -- * App-specific state accessors
  getAppState, putAppState,
  -- * Internal controller monad
  fromApp, toApp,
  Controller(..),
  ControllerStatus(..),
  tryController,
  -- * DC Label specific type alias
  DCController
  ) where

import Prelude hiding (log)
import LIO.DCLabel
import LIO.Exception
import LIO.HTTP.Server
import LIO.HTTP.Server.Responses

import Control.Applicative ()
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Class

import Data.Maybe
import Data.Text (Text)
import Data.Typeable
import Text.Read (readMaybe)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text


-- | This encodes the controller state. When 'Done', the controller will
-- short-circuit and produce the 'Response'. Otherwise the controller is still
-- running and has an intermediate result encoded by 'Working'.
data ControllerStatus a = Done Response
                        | Working a
                        deriving (Eq)

instance Functor ControllerStatus where
  fmap f cs = case cs of
    Working a -> Working $ f a
    Done r    -> Done r

-- | The Controller monad is used to encode stateful HTTP controller
-- computations.  The monad is a reader monad that provides the current request
-- (via 'request' or 'get'). It is also a state monad that theads the
-- application-specific state's' throughout the computation (accessible via
-- 'getAppState' and 'putAppState').
--
-- Within the Controller monad, the remainder of the computation can be
-- short-circuited by 'respond'ing with a 'Response'.
data Controller s m a = Controller {
 runController :: s -> Logger m -> Request m -> m (ControllerStatus a, s)
} deriving (Typeable)

instance Functor m => Functor (Controller s m) where
  fmap f (Controller act) = Controller $ \s0 logger req ->
    go `fmap` act s0 logger req
    where go (cs, st) = (f `fmap` cs, st)

instance (Monad m, Functor m) => Applicative (Controller s m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (Controller s m) where
  return a = Controller $ \st _ _ -> return $ (Working a, st)
  (Controller act0) >>= fn = Controller $ \st0 logger req -> do
    (cs, st1) <- act0 st0 logger req
    case cs of
      Done resp -> return (Done resp, st1)
      Working v -> do
        let (Controller act1) = fn v
        act1 st1 logger req

instance Monad m => MonadState s (Controller s m) where
  get   = Controller $ \s _ _ -> return (Working s, s)
  put s = Controller $ \_ _ _ -> return (Working (), s)

instance Monad m => MonadReader (Request m) (Controller s m) where
  ask = Controller $ \st _ req -> return (Working req, st)
  local f (Controller act) = Controller $ \st logger req -> act st logger (f req)

instance MonadTrans (Controller s) where
  lift act = Controller $ \st _ _ -> act >>= \r -> return (Working r, st)

-- | Try executing the controller action, returning the result or raised
-- exception. Note that exceptions restore the state.
tryController :: WebMonad m
              => Controller s m a
              -> Controller s m (Either SomeException a)
tryController ctrl = Controller $ \s0 logger req -> do
  eres <- tryWeb $ runController ctrl s0 logger req
  case eres of
   Left err -> return (Working (Left err), s0)
   Right (stat, s1) ->
    case stat of
      Working a -> return (Working (Right a), s1)
      Done r    -> return (Done r, s1)

-- | DC-labeled controller
type DCController s = Controller s DC ()

--
-- requests/responses
--

-- | Extract the current request.
request :: Monad m => Controller s m (Request m)
request = ask

-- | Produce a response. Note that the first such response in a monadic
-- action wins and the remainder of the controller will not execute.
--
-- @respond r >>= f === respond r@
respond :: Monad m => Response -> Controller s m a
respond resp = Controller $ \s _ _ -> return (Done resp, s)

-- | Extract the application-specific state.
getAppState :: Monad m => Controller s m s
getAppState = get

-- | Set the application-specific state.
putAppState :: Monad m => s -> Controller s m ()
putAppState = put

-- | Convert an application to a controller. Internally, this uses
-- 'respond' to produce the response.
fromApp :: Monad m => Application m -> Controller s m ()
fromApp app = do
  req <- request
  resp <- lift $ app req
  respond resp

-- | Convert the controller into an 'Application'. This can be used to
-- directly run the controller with 'server', for example.
toApp :: WebMonad m => Controller s m () -> s -> Logger m -> Application m
toApp ctrl s0 logger req = do
  (cs, _) <- runController ctrl s0 logger req
  return $ case cs of
            Done resp -> resp
            _         -> notFound

--
-- query parameters
--

-- | Looks up the parameters in the request's query string and returns the
-- @Parseable@ values or 'Nothing'.
--
-- For example, for a request with query string: \"?foo=bar&baz=7\",
-- @queryParam \"foo\"@
-- would return @["bar"]@, but
-- @queryParam \"zap\"@
-- would return @[]@.
queryParams :: (WebMonad m, Parseable a)
            => Strict.ByteString -- ^ Parameter name
            -> Controller s m [a]
queryParams varName = do
  query <- liftM reqQueryString request
  return $ mapMaybe go query
    where go (name, mparam) = if name == varName
                                then mparam >>= parseBS
                                else Nothing

-- | The class of types into which query parameters and path parts may
-- be converted. We provide definitions for both parse functions in
-- terms of the other, so only one definition is necessary.
class Typeable a => Parseable a where
  -- | Try parsing 'Strict.ByteString' as @a@.
  parseBS   :: Strict.ByteString -> Maybe a
  parseBS bs  = case Text.decodeUtf8' bs of
                  Left _  -> Nothing
                  Right t -> parseText t
  -- | Try parsing 'Text' as @a@.
  parseText :: Text -> Maybe a
  parseText = parseBS . Text.encodeUtf8

instance Parseable Strict.ByteString where
  parseBS   = Just
  parseText = Just . Text.encodeUtf8
instance {-# INCOHERENT #-} Parseable String where
  parseBS   = Just . Char8.unpack
  parseText = Just . Text.unpack
instance Parseable Text where
  parseBS bs  = case Text.decodeUtf8' bs of
                  Left _  -> Nothing
                  Right t -> Just t
  parseText = Just
instance {-# OVERLAPPABLE #-} (Read a, Typeable a) => Parseable a where
  parseBS   = readMaybe . Char8.unpack
  parseText = readMaybe . Text.unpack

-- | Returns the value of the given request header or 'Nothing' if it is not
-- present in the HTTP request.
requestHeader :: WebMonad m
              => HeaderName -> Controller s m (Maybe Strict.ByteString)
requestHeader name = request >>= return . lookup name . reqHeaders

-- | Redirect back to the referer. If the referer header is not present
-- 'redirectTo' root (i.e., @\/@).
redirectBack :: WebMonad m => Controller s m ()
redirectBack = redirectBackOr (redirectTo "/")

-- | Redirect back to the referer. If the referer header is not present
-- fallback on the given 'Response'.
redirectBackOr :: WebMonad m
               => Response -- ^ Fallback response
               -> Controller s m ()
redirectBackOr def = do
  mrefr <- requestHeader "referer"
  case mrefr of
    Just refr -> respond $ redirectTo refr
    Nothing   -> respond def

-- | Log text using app-specific logger.
log :: WebMonad m => LogLevel -> String -> Controller s m ()
log level str = Controller $ \s0 (Logger logger) _ -> do
   logger level str
   return (Working (), s0)

-- | A logger is simply a function that takes the 'LogLevel' and string to
-- write, and produces an action which when executed may log the string. What
-- it means to log is by choice left up to the application.
newtype Logger m = Logger (LogLevel -> String -> m ())

-- | Severity of logging inforamation following RFC5424.
data LogLevel = EMERGENCY
              | ALERT
              | CRITICAL
              | ERROR
              | WARNING
              | NOTICE
              | INFO
              | DEBUG 
              deriving (Show, Eq, Ord)
