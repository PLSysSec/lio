{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LIO.HTTP.Server.Frankie (
  -- * Top-level interface
  FrankieConfig(..), runFrankieConfig,
  host, port,
  get, post, put, patch, delete,
  head, trace, connect, options,
  -- * Internal
  RequestHandler(..),
  regMethodHandler,
  ServerConfig(..), nullServerCfg,
  InvalidConfigException(..), 
  -- ** Path segment related
  PathSegment(..), toPathSegments,
  -- * Re-export LIO server
  module LIO.HTTP.Server,
  module LIO.HTTP.Server.Controller
) where
import Prelude hiding (head)
import LIO.HTTP.Server
import LIO.HTTP.Server.Controller

import Control.Exception
import Control.Monad.State hiding (get, put)
import qualified Control.Monad.State as State


import Data.Dynamic
import Data.Maybe
import Data.Map (Map)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Map as Map

get :: RequestHandler h => Text -> h -> FrankieConfig ()
get path handler = regMethodHandler methodGet path handler

post :: RequestHandler h => Text -> h -> FrankieConfig ()
post path handler = regMethodHandler methodPost path handler

put :: RequestHandler h => Text -> h -> FrankieConfig ()
put path handler = regMethodHandler methodPut path handler

patch :: RequestHandler h => Text -> h -> FrankieConfig ()
patch path handler = regMethodHandler methodPatch path handler

delete :: RequestHandler h => Text -> h -> FrankieConfig ()
delete path handler = regMethodHandler methodDelete path handler

head :: RequestHandler h => Text -> h -> FrankieConfig ()
head path handler = regMethodHandler methodHead path handler

trace :: RequestHandler h => Text -> h -> FrankieConfig ()
trace path handler = regMethodHandler methodTrace path handler

connect :: RequestHandler h => Text -> h -> FrankieConfig ()
connect path handler = regMethodHandler methodConnect path handler

options :: RequestHandler h => Text -> h -> FrankieConfig ()
options path handler = regMethodHandler methodOptions path handler


--
-- Underlying implementation of the methods
--

class Typeable h => RequestHandler h where
  toDynController :: h -> DynController
  toDynController = undefined

instance (Typeable s, Typeable m, Typeable a)
  => RequestHandler (Controller s m a)

instance (Parseable a, Typeable a, Typeable s, Typeable m, Typeable b)
  => RequestHandler (a -> Controller s m b)

regMethodHandler :: RequestHandler h => Method -> Text -> h -> FrankieConfig ()
regMethodHandler method path0 handler = do
  cfg <- State.get
  segments <- toPathSegments path0
  let map0 = cfgDispatchMap cfg
      key = (method, segments)
      dynHandler = toDynController handler
  when (isJust $ Map.lookup key map0) $
    cfgFail $ "Already have handler for: " ++ show (method, segments)
  State.put $ cfg { cfgDispatchMap = Map.insert key dynHandler map0 }

toPathSegments :: Monad m => Text -> m [PathSegment]
toPathSegments path = do
  -- TODO: decodePathSegments assumes valid path. We should make sure that
  -- the path is to spec <https://tools.ietf.org/html/rfc3986#section-3.3>
  let segments = decodePathSegments (Text.encodeUtf8 path)
  return $ map toPathSegment segments
    where toPathSegment seg = if ":" `Text.isPrefixOf` seg
                                then Var seg else Dir seg

-- | A path segment is a simple directory or a variable. Variables are always
-- prefixed by @:@ and considered the same (equal).
data PathSegment = Dir Text | Var Text

instance Show PathSegment where
  show (Dir s) = Text.unpack s
  show (Var s) = Text.unpack s

instance {-# INCOHERENT #-} Show [PathSegment] where
  show ps = "/" ++ (intercalate "/" $ map show ps) ++ "/"

instance Eq PathSegment where
  (Dir x) == (Dir y) = x == y
  (Var _) == (Var _) = True
  _ == _ = False

instance Ord PathSegment where
  compare (Dir x) (Dir y) = compare x y
  compare (Dir _) (Var _) = LT
  compare (Var _) (Dir _) = GT
  compare (Var _) (Var _) = EQ

-- | Set the app port.
port :: Port -> FrankieConfig ()
port p = do
  cfg <- State.get
  -- XXX can we use liquid types instead?
  when (isJust $ cfgPort cfg) $ cfgFail "port already set"
  State.put $ cfg { cfgPort = Just p }

-- | Set the app host preference.
host :: HostPreference -> FrankieConfig ()
host pref = do
  cfg <- State.get
  -- XXX can we use liquid types instead?
  when (isJust $ cfgHostPref cfg) $ cfgFail "host already set"
  State.put $ cfg { cfgHostPref = Just pref }

-- | Type used to encode a Frankie server configuration
newtype FrankieConfig a = FrankieConfig {
  unFrankieConfig :: StateT ServerConfig IO a
} deriving (Functor, Applicative, Monad, MonadState ServerConfig)

runFrankieConfig :: FrankieConfig () -> IO ServerConfig
runFrankieConfig (FrankieConfig act) = do
  (_, cfg) <- runStateT act nullServerCfg
  -- TODO: sanity check cfg
  return cfg

data ServerConfig = ServerConfig {
  cfgPort        :: Maybe Port,
  cfgHostPref    :: Maybe HostPreference,
  cfgDispatchMap :: Map (Method, [PathSegment]) DynController
}

instance Show ServerConfig where
  show cfg =
    "ServerConfig {"
    ++ "cfgPort = " ++ (show . cfgPort $ cfg)
    ++ ",cfgHostPref = " ++ (show . cfgHostPref $ cfg)
    ++ ",cfgDispatchMap = " ++ (show . Map.keys . cfgDispatchMap $ cfg)
    ++ "}"

-- | Configuration errors
data InvalidConfigException = InvalidConfig String
  deriving (Show, Typeable)
instance Exception InvalidConfigException

-- | Throw 'InvalidConfigException' error
cfgFail :: String -> FrankieConfig a
cfgFail msg = FrankieConfig $ lift . throwIO $ InvalidConfig msg

nullServerCfg :: ServerConfig
nullServerCfg = ServerConfig {
  cfgPort = Nothing,
  cfgHostPref = Nothing,
  cfgDispatchMap = Map.empty
}


type DynController = IO ()
