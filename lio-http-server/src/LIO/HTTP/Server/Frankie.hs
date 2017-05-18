{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LIO.HTTP.Server.Frankie where
import LIO.HTTP.Server
import LIO.HTTP.Server.Controller

import Control.Monad.Identity
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


class Typeable h => RequestHandler h where
  toDynController :: h -> DynController
  toDynController = undefined

instance (Typeable s, Typeable m, Typeable a)
  => RequestHandler (Controller s m a)

instance (Parseable a, Typeable a, Typeable s, Typeable m, Typeable b)
  => RequestHandler (a -> Controller s m b)
-- instance (Parseable a, Typeable a,
--           Parseable b, Typeable b)
--   => RequestHandler (a -> b -> Controller s m ())
-- instance (Parseable a, Typeable a, Typeable c,
--           Parseable b, Typeable b, Typeable c)
--   => RequestHandler (a -> b -> c -> Controller s m ())
-- instance (Parseable a, Typeable a, Typeable c, Typeable d,
--           Parseable b, Typeable b, Typeable c, Typeable d)
--   => RequestHandler (a -> b -> c -> d -> Controller s m ())

get :: RequestHandler h => Text -> h -> FrankieConfig ()
get path handler = regMethodHandler methodGet path handler

regMethodHandler :: RequestHandler h => Method -> Text -> h -> FrankieConfig ()
regMethodHandler method path0 handler = do
  cfg <- State.get
  segments <- toPathSegments path0
  let map0 = cfgDispatchMap cfg
      key = (method, segments)
      dynHandler = toDynController handler
  when (isJust $ Map.lookup key map0) $
    fail $ "Already have handler for: " ++ show (method, segments)
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

type FrankieConfig a = StateT ServerConfig Identity a

data ServerConfig = ServerConfig {
  cfgPort        :: Port,
  cfgHostPref    :: HostPreference,
  cfgDispatchMap :: Map (Method, [PathSegment]) DynController
}

type DynController = IO ()
