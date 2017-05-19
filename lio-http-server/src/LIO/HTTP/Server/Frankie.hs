{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  VarParser(..), DynController(..),
  -- ** Path segment related
  PathSegment(..), toPathSegments,
  isVar,
  -- * Re-export LIO server
  module LIO.HTTP.Server,
  module LIO.HTTP.Server.Controller
  -- , nullCtrl0
  -- , nullCtrl1
  -- , nullCtrl2
) where
import Prelude hiding (head)
import LIO.HTTP.Server
import LIO.HTTP.Server.Controller

import Control.Exception
import Control.Monad.State hiding (get, put)
import qualified Control.Monad.State as State


import GHC.Fingerprint.Type
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

-- | Dynamic controller
newtype DynController = DynController Dynamic
newtype VarParser = VarParser Dynamic

class Typeable h => RequestHandler h where
  -- | The argument, variable parsers
  varParser :: h -> [VarParser]
  varParser _ = []

  -- | Convert request handler to a 'Dynamic' controller
  toDynController :: h -> DynController
  toDynController = DynController . toDyn

  -- | The number of arguments the request handler expects
  reqHandlerNrArgs :: h -> Int
  reqHandlerNrArgs f = length $ fingerprintArgs f

  -- | The fingerprint of all the arguments
  fingerprintArgs :: h -> [Fingerprint]
  fingerprintArgs f = map typeRepFingerprint $ getArgs' $ typeRepArgs $ typeOf f
    where getArgs' :: [TypeRep] -> [TypeRep]
          getArgs' [a, b] = a : (getArgs' $ typeRepArgs b)
          getArgs' _      = []

instance (Typeable s, Typeable m, Typeable x)
  => RequestHandler (Controller s m x)
instance (Parseable a, Typeable a, Typeable s, Typeable m, Typeable x)
  => RequestHandler (a -> Controller s m x) where
  varParser _ = let pTa :: Text -> Maybe a
                    pTa = parseText
                in map VarParser [toDyn pTa]
instance (Parseable a, Typeable a, Parseable b, Typeable b,
          Typeable s, Typeable m, Typeable x)
  => RequestHandler (a -> b -> Controller s m x) where
  varParser _ = let pTa :: Text -> Maybe a
                    pTa = parseText
                    pTb :: Text -> Maybe b
                    pTb = parseText
                in map VarParser [toDyn pTa, toDyn pTb]
instance (Parseable a, Typeable a, Parseable b, Typeable b, 
          Parseable c, Typeable c,
          Typeable s, Typeable m, Typeable x)
  => RequestHandler (a -> b -> c -> Controller s m x) where
  varParser _ = let pTa :: Text -> Maybe a
                    pTa = parseText
                    pTb :: Text -> Maybe b
                    pTb = parseText
                    pTc :: Text -> Maybe c
                    pTc = parseText
                in map VarParser [toDyn pTa, toDyn pTb, toDyn pTc]
instance (Parseable a, Typeable a, Parseable b, Typeable b, 
          Parseable c, Typeable c, Parseable d, Typeable d,
          Typeable s, Typeable m, Typeable x)
  => RequestHandler (a -> b -> c -> d -> Controller s m x) where
  varParser _ = let pTa :: Text -> Maybe a
                    pTa = parseText
                    pTb :: Text -> Maybe b
                    pTb = parseText
                    pTc :: Text -> Maybe c
                    pTc = parseText
                    pTd :: Text -> Maybe d
                    pTd = parseText
                in map VarParser [toDyn pTa, toDyn pTb, toDyn pTc, toDyn pTd]
instance (Parseable a, Typeable a, Parseable b, Typeable b, 
          Parseable c, Typeable c, Parseable d, Typeable d, 
          Parseable e, Typeable e,
          Typeable s, Typeable m, Typeable x)
  => RequestHandler (a -> b -> c -> d -> e -> Controller s m x) where
  varParser _ = let pTa :: Text -> Maybe a
                    pTa = parseText
                    pTb :: Text -> Maybe b
                    pTb = parseText
                    pTc :: Text -> Maybe c
                    pTc = parseText
                    pTd :: Text -> Maybe d
                    pTd = parseText
                    pTe :: Text -> Maybe e
                    pTe = parseText
                in map VarParser [toDyn pTa, toDyn pTb, toDyn pTc
                                 , toDyn pTd, toDyn pTe]
instance (Parseable a, Typeable a, Parseable b, Typeable b, 
          Parseable c, Typeable c, Parseable d, Typeable d, 
          Parseable e, Typeable e, Parseable f, Typeable f,
          Typeable s, Typeable m, Typeable x)
  => RequestHandler (a -> b -> c -> d -> e -> f -> Controller s m x) where
  varParser _ = let pTa :: Text -> Maybe a
                    pTa = parseText
                    pTb :: Text -> Maybe b
                    pTb = parseText
                    pTc :: Text -> Maybe c
                    pTc = parseText
                    pTd :: Text -> Maybe d
                    pTd = parseText
                    pTe :: Text -> Maybe e
                    pTe = parseText
                    pTf :: Text -> Maybe f
                    pTf = parseText
                in map VarParser [toDyn pTa, toDyn pTb, toDyn pTc
                                 , toDyn pTd, toDyn pTe, toDyn pTf]

regMethodHandler :: RequestHandler h => Method -> Text -> h -> FrankieConfig ()
regMethodHandler method path handler = do
  cfg <- State.get
  segments <- toPathSegments path
  let map0 = cfgDispatchMap cfg
      key = (method, segments)
  -- Make sure the controller is not already registered (liquid?)
  when (isJust $ Map.lookup key map0) $
    cfgFail $ "Already have handler for: " ++ show (method, segments)
  -- Make sure that the controller and number of vars match (liquid?)
  let nrVars = length $ filter isVar segments
      nrArgs = reqHandlerNrArgs handler
  when (nrVars /= nrArgs) $
    cfgFail $ "Unexpected number of variables (" ++ show nrVars ++ 
              ") for handler (expected " ++ show nrArgs ++ ")"
  let varParsers = varParser handler
      dynHandler = toDynController handler
  State.put $ cfg { cfgDispatchMap = 
    Map.insert key (varParsers, dynHandler) map0 }

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

isVar :: PathSegment -> Bool
isVar (Var _) = True
isVar _       = False

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

-- | Run a config action to produce a server configuration
runFrankieConfig :: FrankieConfig () -> IO ServerConfig
runFrankieConfig (FrankieConfig act) = do
  (_, cfg) <- runStateT act nullServerCfg
  -- TODO: sanity check cfg
  return cfg

-- | A server configuration containts the port and host to run the server on.
-- It also contains the dispatch table.
-- TODO: add support for error handlers, loggers, dev vs. prod, etc.
data ServerConfig = ServerConfig {
  cfgPort        :: Maybe Port,
  cfgHostPref    :: Maybe HostPreference,
  cfgDispatchMap :: Map (Method, [PathSegment]) ([VarParser], DynController)
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


-- XXX remove:

nullCtrl0 :: DCController ()
nullCtrl0 = return ()

nullCtrl1 :: Int -> DCController ()
nullCtrl1 _ = return ()

nullCtrl2 :: Int -> String -> DCController ()
nullCtrl2 _ _ = return ()
