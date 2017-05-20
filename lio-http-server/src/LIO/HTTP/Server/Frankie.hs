{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module LIO.HTTP.Server.Frankie (
  -- * Top-level interface
  FrankieConfig(..), runFrankieConfig, runFrankieServer,
  host, port, appState,
  get, post, put, patch, delete,
  head, trace, connect, options,
  -- * Internal
  RequestHandler(..),
  regMethodHandler,
  ServerConfig(..), nullServerCfg,
  InvalidConfigException(..), 
  -- ** Path segment related
  PathSegment(..), toPathSegments,
  isVar, matchPath,
  -- * Re-export LIO server
  module LIO.HTTP.Server,
  module LIO.HTTP.Server.Responses,
  module LIO.HTTP.Server.Controller
) where
import Prelude hiding (head)
import LIO.HTTP.Server
import LIO.HTTP.Server.Responses
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

get :: RequestHandler h s m => Text -> h -> FrankieConfig s m ()
get path handler = regMethodHandler methodGet path handler

post :: RequestHandler h s m => Text -> h -> FrankieConfig s m ()
post path handler = regMethodHandler methodPost path handler

put :: RequestHandler h s m => Text -> h -> FrankieConfig s m ()
put path handler = regMethodHandler methodPut path handler

patch :: RequestHandler h s m => Text -> h -> FrankieConfig s m ()
patch path handler = regMethodHandler methodPatch path handler

delete :: RequestHandler h s m => Text -> h -> FrankieConfig s m ()
delete path handler = regMethodHandler methodDelete path handler

head :: RequestHandler h s m => Text -> h -> FrankieConfig s m ()
head path handler = regMethodHandler methodHead path handler

trace :: RequestHandler h s m => Text -> h -> FrankieConfig s m ()
trace path handler = regMethodHandler methodTrace path handler

connect :: RequestHandler h s m => Text -> h -> FrankieConfig s m ()
connect path handler = regMethodHandler methodConnect path handler

options :: RequestHandler h s m => Text -> h -> FrankieConfig s m ()
options path handler = regMethodHandler methodOptions path handler


--
-- Underlying implementation of the methods
--

parseFailed :: Monad m => Controller s m a
parseFailed = respond badRequest

invalidArgs :: Monad m => Controller s m ()
invalidArgs = respond $ serverError "BUG: controller called with invalid args"

-- | Get the path variable or fail with parser error
pathVarOrFail :: (WebMonad m, Parseable a)
              => PathSegment -- ^ Parameter name
              -> Controller s m a
pathVarOrFail ps = do
  pathInfo <- liftM reqPathInfo request
  case ps of
    (Var _ idx) | idx < length pathInfo -> 
      case parseText (pathInfo!!idx) of
        Just x -> return x
        _ -> parseFailed
    _ -> parseFailed


class (Monad m, Typeable h) => RequestHandler h s m | h -> s m where
  -- | Apply the request handler, parsing the supplied list of
  -- variables. The returned controller will respond with a
  -- 'parseFailed' error if parsing fails.
  handlerToController :: [PathSegment] -> h -> Controller s m ()

  -- | The types for the arugments the handler takes
  reqHandlerArgTy :: h -> FrankieConfig s m [TypeRep]
  reqHandlerArgTy f = return . getArgs' .  typeRepArgs $ typeOf f
    where getArgs' :: [TypeRep] -> [TypeRep]
          getArgs' [a, b] = a : (getArgs' $ typeRepArgs b)
          getArgs' _      = []

instance (Typeable s, WebMonad m, Typeable m)
  => RequestHandler (Controller s m ()) s m where
  handlerToController [] ctrl = ctrl
  handlerToController _ _ = invalidArgs

instance (Parseable a, Typeable a, Typeable s, WebMonad m, Typeable m)
  => RequestHandler (a -> Controller s m ()) s m where
  handlerToController [ta] ctrl = do
    a <- pathVarOrFail ta
    ctrl a
  handlerToController _ _ = invalidArgs

instance (Parseable a, Typeable a, Parseable b, Typeable b,
          Typeable s, WebMonad m, Typeable m)
  => RequestHandler (a -> b -> Controller s m ()) s m where
  handlerToController [ta, tb] ctrl = do
    a <- pathVarOrFail ta
    b <- pathVarOrFail tb
    ctrl a b
  handlerToController _ _ = invalidArgs

instance (Parseable a, Typeable a, Parseable b, Typeable b, 
          Parseable c, Typeable c,
          Typeable s, WebMonad m, Typeable m)
  => RequestHandler (a -> b -> c -> Controller s m ()) s m where
  handlerToController [ta, tb, tc] ctrl = do
    a <- pathVarOrFail ta
    b <- pathVarOrFail tb
    c <- pathVarOrFail tc
    ctrl a b c
  handlerToController _ _ = invalidArgs
instance (Parseable a, Typeable a, Parseable b, Typeable b, 
          Parseable c, Typeable c, Parseable d, Typeable d,
          Typeable s, WebMonad m, Typeable m)
  => RequestHandler (a -> b -> c -> d -> Controller s m ()) s m where
  handlerToController [ta, tb, tc, td] ctrl = do
    a <- pathVarOrFail ta
    b <- pathVarOrFail tb
    c <- pathVarOrFail tc
    d <- pathVarOrFail td
    ctrl a b c d
  handlerToController _ _ = invalidArgs
instance (Parseable a, Typeable a, Parseable b, Typeable b, 
          Parseable c, Typeable c, Parseable d, Typeable d, 
          Parseable e, Typeable e,
          Typeable s, WebMonad m, Typeable m)
  => RequestHandler (a -> b -> c -> d -> e -> Controller s m ()) s m where
  handlerToController [ta, tb, tc, td, te] ctrl = do
    a <- pathVarOrFail ta
    b <- pathVarOrFail tb
    c <- pathVarOrFail tc
    d <- pathVarOrFail td
    e <- pathVarOrFail te
    ctrl a b c d e
  handlerToController _ _ = invalidArgs
instance (Parseable a, Typeable a, Parseable b, Typeable b, 
          Parseable c, Typeable c, Parseable d, Typeable d, 
          Parseable e, Typeable e, Parseable f, Typeable f,
          Typeable s, WebMonad m, Typeable m)
  => RequestHandler (a -> b -> c -> d -> e -> f -> Controller s m ()) s m where
  handlerToController [ta, tb, tc, td, te, tf] ctrl = do
    a <- pathVarOrFail ta
    b <- pathVarOrFail tb
    c <- pathVarOrFail tc
    d <- pathVarOrFail td
    e <- pathVarOrFail te
    f <- pathVarOrFail tf
    ctrl a b c d e f
  handlerToController _ _ = invalidArgs

regMethodHandler :: RequestHandler h s m 
                 => Method -> Text -> h -> FrankieConfig s m ()
regMethodHandler method path handler = do
  cfg <- State.get
  segments <- toPathSegments path
  let map0 = cfgDispatchMap cfg
      key0 = (method, segments)
  -- Make sure the controller is not already registered (liquid?)
  when (isJust $ Map.lookup key0 map0) $
    cfgFail $ "Already have handler for: " ++ show (method, segments)
  -- Make sure that the controller and number of vars match (liquid?)
  args <- reqHandlerArgTy handler
  let vars   = filter isVar segments
      nrVars = length vars
      nrArgs = length args
  when (nrVars /= nrArgs) $
    cfgFail $ "Unexpected number of variables (" ++ show nrVars ++ 
              ") for handler (expected " ++ show nrArgs ++ ")"
  -- Update the variable sin the segments with types
  let key1 = (method, fixSegments segments args)
  State.put $ cfg { cfgDispatchMap = 
    Map.insert key1 (handlerToController vars handler) map0 }

-- | Given path segments broken down by 'toPathSegments' and a list of variable
-- types, rename the variables in the segments to add the type info. Assuming
-- types are sorted according to the variables the represent.  Called by
-- regMethodHandler. Usage outside this is discouraged.
fixSegments :: [PathSegment]
            -> [TypeRep]
            -> [PathSegment]
fixSegments (Var n0 i0 : ss) (ty : ts) = 
  let n0' = n0 `Text.append` (Text.pack $ '@' : show ty)
  in (Var n0' i0) : fixSegments ss ts
fixSegments ((Dir d) : ss) ts = (Dir d) : fixSegments ss ts
fixSegments [] [] = []
fixSegments _ _ = error "BUG: fixSegments called incorrectly"

-- | Convert a path to its corresponding segments
toPathSegments :: Monad m => Text -> m [PathSegment]
toPathSegments path = do
  -- TODO: decodePathSegments assumes valid path. We should make sure that
  -- the path is to spec <https://tools.ietf.org/html/rfc3986#section-3.3>
  let segments = decodePathSegments (Text.encodeUtf8 path)
  return . snd $ foldr (\seg (idx, ps) ->
    (idx - 1, toPathSegment seg idx : ps)) (length segments - 1,[]) segments
  where toPathSegment seg idx = if ":" `Text.isPrefixOf` seg
                                  then Var seg idx else Dir seg

-- | A path segment is a simple directory or a variable. Variables are always
-- prefixed by @:@ and considered the same (equal).
data PathSegment = Dir Text | Var Text Int

isVar :: PathSegment -> Bool
isVar (Var _ _) = True
isVar _         = False

instance Show PathSegment where
  show (Dir s)   = Text.unpack s
  show (Var s _) = Text.unpack s -- ++ "@" ++ show i

instance {-# INCOHERENT #-} Show [PathSegment] where
  show ps = "/" ++ (intercalate "/" $ map show ps) ++ "/"

instance Eq PathSegment where
  (Dir x) == (Dir y) = x == y
  (Var _ _) == (Var _ _) = True
  _ == _ = False

instance Ord PathSegment where
  compare (Dir x) (Dir y) = compare x y
  compare (Dir _) (Var _ _) = LT
  compare (Var _ _) (Dir _) = GT
  compare (Var _ _) (Var _ _) = EQ

-- | Set the app port.
port :: Port -> FrankieConfig s m ()
port p = do
  cfg <- State.get
  -- XXX can we use liquid types instead?
  when (isJust $ cfgPort cfg) $ cfgFail "port already set"
  State.put $ cfg { cfgPort = Just p }

-- | Set the app host preference.
host :: HostPreference -> FrankieConfig s m ()
host pref = do
  cfg <- State.get
  -- XXX can we use liquid types instead?
  when (isJust $ cfgHostPref cfg) $ cfgFail "host already set"
  State.put $ cfg { cfgHostPref = Just pref }

-- | Set the app host preference.
appState :: s -> FrankieConfig s m ()
appState s = do
  cfg <- State.get
  -- XXX can we use liquid types instead?
  when (isJust $ cfgAppState cfg) $ cfgFail "state already set"
  State.put $ cfg { cfgAppState = Just s }

-- | Type used to encode a Frankie server configuration
newtype FrankieConfig s m a = FrankieConfig {
  unFrankieConfig :: StateT (ServerConfig s m) IO a
} deriving (Functor, Applicative, Monad, MonadState (ServerConfig s m))

-- | Run a config action to produce a server configuration
runFrankieConfig :: FrankieConfig s m () -> IO (ServerConfig s m)
runFrankieConfig (FrankieConfig act) = do
  (_, cfg) <- runStateT act nullServerCfg
  -- XXX can we use liquid types instead?
  when (isNothing $ cfgPort cfg) $ throwIO $ InvalidConfig "missing port"
  when (isNothing $ cfgHostPref cfg) $ throwIO $ InvalidConfig "missing host"
  when (isNothing $ cfgAppState cfg) $ throwIO $ InvalidConfig "missing state"
  return cfg

-- | Run the Frankie server
runFrankieServer :: WebMonad m 
                 => FrankieConfig s m ()
                 -> IO ()
runFrankieServer frankieAct = do
  cfg <- runFrankieConfig frankieAct
  let cPort  = fromJust . cfgPort $ cfg
      cHost  = fromJust . cfgHostPref $ cfg
      cState = fromJust . cfgAppState $ cfg
  server cPort cHost (toApp (mainFrankieController cfg) cState)

-- | The main controller that dispatches requests to corresponding controllers.
mainFrankieController :: WebMonad m => ServerConfig s m -> Controller s m ()
mainFrankieController cfg = do
  req <- request
  let method   = reqMethod req
      pathInfo = reqPathInfo req
  let cs = Map.toList $ 
            Map.filterWithKey (\(m, ps) _ -> m == method && matchPath ps pathInfo) $
            cfgDispatchMap cfg
  controller <- return $ case cs of
                           [(_, controller)] -> controller
                           _ -> respond notFound
  controller

-- | Match a path segment with the path request info. We only need
-- to make sure that the number of directories are the same and
-- that non-variables match exactly.
matchPath :: [PathSegment] -> [Text] -> Bool
matchPath (Dir p:ps)   (t:ts) = p == t && matchPath ps ts
matchPath (Var _ _:ps) (_:ts) = matchPath ps ts
matchPath []           []     = True
matchPath _            _      = False


-- | A server configuration containts the port and host to run the server on.
-- It also contains the dispatch table.
-- TODO: add support for error handlers, loggers, dev vs. prod, etc.
data ServerConfig s m = ServerConfig {
  cfgPort        :: Maybe Port,
  cfgHostPref    :: Maybe HostPreference,
  cfgAppState    :: Maybe s,
  cfgDispatchMap :: Map (Method, [PathSegment]) (Controller s m ())
}

instance Show s  => Show (ServerConfig s m) where
  show cfg =
    "ServerConfig {"
    ++ "cfgPort = " ++ (show . cfgPort $ cfg)
    ++ ",cfgHostPref = " ++ (show . cfgHostPref $ cfg)
    ++ ",cfgAppState = " ++ (show . cfgAppState $ cfg)
    ++ ",cfgDispatchMap = " ++ (show . Map.keys . cfgDispatchMap $ cfg)
    ++ "}"

-- | Configuration errors
data InvalidConfigException = InvalidConfig String
  deriving (Show, Typeable)
instance Exception InvalidConfigException

-- | Throw 'InvalidConfigException' error to indicate bad configuration.
cfgFail :: String -> FrankieConfig s m a
cfgFail msg = FrankieConfig $ lift . throwIO $ InvalidConfig msg

nullServerCfg :: ServerConfig s m
nullServerCfg = ServerConfig {
  cfgPort = Nothing,
  cfgHostPref = Nothing,
  cfgAppState =  Nothing,
  cfgDispatchMap = Map.empty
}
