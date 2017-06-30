{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module LIO.HTTP.Server.Frankie (
  -- * Top-level interface
  FrankieConfig(..), FrankieConfigDispatch(..),
  runFrankieServer,
  -- ** Configuration modes
  mode, port, host, appState, logger,
  -- ** Dispatch-table
  dispatch,
  get, post, put, patch, delete,
  head, trace, connect, options,
  fallback,
  -- ** Error-handling related
  onError, Exception(..),
  -- * Internal
  -- ** Configuration related
  runFrankieConfig,
  ServerConfig(..), nullServerCfg,
  InvalidConfigException(..),
  FrankieConfigMonad(..),
  -- ** Configuration mode related
  ModeConfig(..), Mode, nullModeCfg,
  -- ** Request handler related
  RequestHandler(..), regMethodHandler,
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
import Control.Monad.Reader
import qualified Control.Monad.State as State

import Data.Dynamic
import Data.Maybe
import Data.Map (Map)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Map as Map

--
-- Configure dispatch table
--

dispatch :: FrankieConfigDispatch s m () -> FrankieConfig s m ()
dispatch (FrankieConfigDispatch act) = act


-- | Matches the GET method on the given URL pattern
get :: RequestHandler h s m => Text -> h -> FrankieConfigDispatch s m ()
get path handler = FrankieConfigDispatch $ regMethodHandler methodGet path handler

-- | Matches the POST method on the given URL pattern
post :: RequestHandler h s m => Text -> h -> FrankieConfigDispatch s m ()
post path handler = FrankieConfigDispatch $ regMethodHandler methodPost path handler

-- | Matches the PUT method on the given URL pattern
put :: RequestHandler h s m => Text -> h -> FrankieConfigDispatch s m ()
put path handler = FrankieConfigDispatch $ regMethodHandler methodPut path handler

-- | Matches the PATCH method on the given URL pattern
patch :: RequestHandler h s m => Text -> h -> FrankieConfigDispatch s m ()
patch path handler = FrankieConfigDispatch $ regMethodHandler methodPatch path handler

-- | Matches the PATCH method on the given URL pattern
delete :: RequestHandler h s m => Text -> h -> FrankieConfigDispatch s m ()
delete path handler = FrankieConfigDispatch $ regMethodHandler methodDelete path handler

-- | Matches the HEAD method on the given URL pattern
head :: RequestHandler h s m => Text -> h -> FrankieConfigDispatch s m ()
head path handler = FrankieConfigDispatch $ regMethodHandler methodHead path handler

-- | Matches the TRACE method on the given URL pattern
trace :: RequestHandler h s m => Text -> h -> FrankieConfigDispatch s m ()
trace path handler = FrankieConfigDispatch $ regMethodHandler methodTrace path handler

-- | Matches the CONNECT method on the given URL pattern
connect :: RequestHandler h s m => Text -> h -> FrankieConfigDispatch s m ()
connect path handler = FrankieConfigDispatch $ regMethodHandler methodConnect path handler

-- | Matches the OPTIONS method on the given URL pattern
options :: RequestHandler h s m => Text -> h -> FrankieConfigDispatch s m ()
options path handler = FrankieConfigDispatch $ regMethodHandler methodOptions path handler

-- | Fallback controller called if nothing else matched
fallback :: Monad m => Controller s m () -> FrankieConfigDispatch s m ()
fallback controller = FrankieConfigDispatch $ do
  cfg <- State.get
  -- XXX liquid types
  when (isJust $ cfgDispatchFallback cfg) $
    cfgFail "fallback handler already set"
  State.put $ cfg { cfgDispatchFallback = Just controller }


--
-- Underlying implementation of the methods
--

-- | On parse failure, respond with 400, bad request.
parseFailed :: Monad m => Controller s m a
parseFailed = respond badRequest

-- | This action produces a server error response. This response indicates a
-- bug in this server implementation.
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

instance (Parseable a, Typeable a, RequestHandler c s m, WebMonad m)
  => RequestHandler (a -> c) s m where
  handlerToController (ta:ts) ctrl = do
    a <- pathVarOrFail ta
    handlerToController ts (ctrl a)
  handlerToController _ _ = invalidArgs



-- | Register a handler for the particular method and path.
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

--
-- Top-level configuration options
--

-- | Set the app port.
port :: Port -> FrankieConfigMode s m ()
port p = do
  cfg <- getModeConfig
  when (isJust $ cfgPort cfg) $ cfgFail "port already set"
  setModeConfig $ cfg {cfgPort = Just p }

-- | Set the app host preference.
host :: HostPreference -> FrankieConfigMode s m ()
host pref = do
  cfg <- getModeConfig
  -- XXX can we use liquid types instead?
  when (isJust $ cfgHostPref cfg) $ cfgFail "host already set"
  setModeConfig $ cfg { cfgHostPref = Just pref }

-- | Set the app host preference.
appState :: s -> FrankieConfigMode s m ()
appState s = do
  cfg <- getModeConfig
  -- XXX can we use liquid types instead?
  when (isJust $ cfgAppState cfg) $ cfgFail "state already set"
  setModeConfig $ cfg { cfgAppState = Just s }

-- | Register a logger with the app.
logger :: Monad m => LogLevel -> Logger m -> FrankieConfigMode s m ()
logger level (Logger lgr0) = do
  cfg <- getModeConfig
  -- create logger that only logs things as sever as level
  let lgr1 l s = when (l <= level) $ lgr0 l s 
      newLogger = case cfgLogger cfg of
                    Just (Logger lgrC) -> \l s -> lgrC l s >> lgr1 l s
                    Nothing -> lgr1
  setModeConfig $ cfg { cfgLogger = Just (Logger newLogger)}


-- | Helper function for getting the mode configuration corresponding to the
-- current mode
getModeConfig :: FrankieConfigMode s m (ModeConfig s m)
getModeConfig = do
  mode0 <- ask
  cfg <- State.get
  let modeMap = cfgModes cfg
  -- XXX liquid types instead of these checks?
  case Map.lookup mode0 modeMap of
    Nothing      -> cfgFail "BUG: should have mode"
    Just modeCfg -> return modeCfg

-- | Helper function for updating the mode configuration corresponding to the
-- current mode
setModeConfig :: ModeConfig s m -> FrankieConfigMode s m ()
setModeConfig modeCfg = do
  mode0 <- ask
  cfg <- State.get
  let modeMap = cfgModes cfg
  State.put $ cfg { cfgModes = Map.insert mode0 modeCfg modeMap }

-- | Helper function for creating a new config mode.
newModeConfig :: FrankieConfigMode s m ()
newModeConfig = do
  mode0 <- ask
  cfg <- State.get
  let modeMap = cfgModes cfg
  -- XXX liquid types instead of these checks?
  case Map.lookup mode0 modeMap of
    Just _       -> cfgFail "mode already defined"
    _            -> State.put $ cfg { cfgModes = Map.insert mode0 nullModeCfg modeMap }


--
--
--

-- | Type used to encode a Frankie server configuration
newtype FrankieConfig s m a = FrankieConfig {
  unFrankieConfig :: StateT (ServerConfig s m) IO a
} deriving (Functor, Applicative, Monad, MonadState (ServerConfig s m))

class FrankieConfigMonad k where
  liftFrankie :: forall s m a. FrankieConfig s m a -> k s m a

instance FrankieConfigMonad FrankieConfig where
  liftFrankie = id

-- | Simple wrapper around 'FrankieConfig' to separate the dispatch-table
-- portions of the configuration from the rest.
newtype FrankieConfigDispatch s m a = FrankieConfigDispatch (FrankieConfig s m a)
  deriving (Functor, Applicative, Monad )

instance FrankieConfigMonad FrankieConfigDispatch where
  liftFrankie = FrankieConfigDispatch

-- | Simple wrapper around 'FrankieConfig' to separate the different mode
-- portions of the configuration from the rest of the configurations.
newtype FrankieConfigMode s m a = FrankieConfigMode (ReaderT Mode (FrankieConfig s m) a)
  deriving (Functor, Applicative, Monad, MonadReader Mode, MonadState (ServerConfig s m))

instance FrankieConfigMonad FrankieConfigMode where
  liftFrankie act = FrankieConfigMode $ lift act

-- | Configure a particular mode.
mode :: Mode -> FrankieConfigMode s m a -> FrankieConfig s m a
mode modeName act =
  let (FrankieConfigMode rModeCfg) = newModeConfig >> act
  in runReaderT rModeCfg modeName

-- | Register a controller to be called when other normal controllers raise
-- exceptions. If this controller throws an exception, a default error response
-- is produced.
onError :: (SomeException -> Controller s m ()) -> FrankieConfig s m ()
onError handler = do
  cfg <- State.get
  when (isJust $ cfgOnErrorHandler cfg) $
    cfgFail "onError handler already registered"
  State.put $ cfg { cfgOnErrorHandler = Just handler }


-- | Run a config action to produce a server configuration
runFrankieConfig :: FrankieConfig s m () -> IO (ServerConfig s m)
runFrankieConfig (FrankieConfig act) = do
  (_, cfg) <- runStateT act nullServerCfg
  -- XXX can we use liquid types instead?
  return cfg

-- | Run the Frankie server
runFrankieServer :: WebMonad m
                 => Mode
                 -> FrankieConfig s m ()
                 -> IO ()
runFrankieServer mode0 frankieAct = do
  cfg <- runFrankieConfig frankieAct
  case (Map.lookup mode0 $ cfgModes cfg) of
    Nothing -> throwIO $ InvalidConfig "invalid mode "
    Just modeCfg -> do
      when (isNothing $ cfgPort modeCfg) $ throwIO $ InvalidConfig "missing port"
      when (isNothing $ cfgHostPref modeCfg) $ throwIO $ InvalidConfig "missing host"
      when (isNothing $ cfgAppState modeCfg) $ throwIO $ InvalidConfig "missing state"
      let cPort  = fromJust . cfgPort $ modeCfg
          cHost  = fromJust . cfgHostPref $ modeCfg
          cState = fromJust . cfgAppState $ modeCfg
          -- if not logger define, just provide a nop
          lgr = case cfgLogger modeCfg of
                  Just l -> l
                  _      -> Logger $ \_ _ -> return ()

      server cPort cHost (toApp (mainFrankieController cfg) cState lgr)

-- | The main controller that dispatches requests to corresponding controllers.
mainFrankieController :: WebMonad m => ServerConfig s m -> Controller s m ()
mainFrankieController cfg = do
  req <- request
  let method   = reqMethod req
      pathInfo = reqPathInfo req
  let cs = Map.toList $
            Map.filterWithKey (\(m, ps) _ -> m == method && matchPath ps pathInfo) $
            cfgDispatchMap cfg
  -- create controller action to execute
  controller <- return $ case cs of
    [(_, controller)] -> controller
    -- didn't match anything in the dispatch table, fallback?
    _ | (isJust $ cfgDispatchFallback cfg) -> fromJust $ cfgDispatchFallback cfg
    -- nope, just respond with 404
    _ -> respond notFound
  -- execute the controller action
  er <- tryController controller
  case er of
    Right r  -> return r
    -- controller raised exception
    Left err ->
      case cfgOnErrorHandler cfg of
        -- have user-define handler
        Just handler -> do
          -- execute user-defined handler
          er' <- tryController $ handler err
          case er' of
            -- handler produced response
            Right r -> return r
            -- handler throw exception
            _       -> respond $ serverError "Something went wrong"
        -- no user-defined handler
        _            -> respond $ serverError "Something went wrong"

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
  cfgModes       :: Map Mode (ModeConfig s m),
  -- ^ Configuration modes
  cfgDispatchMap :: Map (Method, [PathSegment]) (Controller s m ()),
  -- ^ Dispatch table
  cfgDispatchFallback :: Maybe (Controller s m ()),
  -- ^ Dispatch fallback handler
  cfgOnErrorHandler  :: Maybe (SomeException -> Controller s m ())
  -- ^ Exception handler
}

instance Show s  => Show (ServerConfig s m) where
  show cfg =
    "ServerConfig {"
    ++ "cfgModes = " ++ (show . cfgModes $ cfg)
    ++ ",cfgDispatchMap = " ++ (show . Map.keys . cfgDispatchMap $ cfg)
    ++ "}"

-- | Configuration errors
data InvalidConfigException = InvalidConfig String
  deriving (Show, Typeable)
instance Exception InvalidConfigException

-- | Throw 'InvalidConfigException' error to indicate bad configuration.
cfgFail :: FrankieConfigMonad k => String -> k s m a
cfgFail msg = liftFrankie . FrankieConfig $ lift . throwIO $ InvalidConfig msg

-- | Initial emtpy server configuration
nullServerCfg :: ServerConfig s m
nullServerCfg = ServerConfig {
  cfgModes            = Map.empty,
  cfgDispatchMap      = Map.empty,
  cfgDispatchFallback = Nothing,
  cfgOnErrorHandler   = Nothing
}

-- | Modes are described by strings.
type Mode = String

-- | Mode configuration. For example, production or development.
data ModeConfig s m = ModeConfig {
  cfgPort        :: Maybe Port,
  cfgHostPref    :: Maybe HostPreference,
  cfgAppState    :: Maybe s,
  cfgLogger      :: Maybe (Logger m)
  }

instance Show (ModeConfig s m) where
  show cfg = (show . cfgHostPref $ cfg) ++ ":" ++
             (show . cfgHostPref $ cfg)


-- | Empty mode configuration.
nullModeCfg :: ModeConfig s m
nullModeCfg =  ModeConfig {
  cfgPort        = Nothing,
  cfgHostPref    = Nothing,
  cfgAppState    = Nothing,
  cfgLogger      = Nothing
}
