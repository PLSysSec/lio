{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import LIO.HTTP.Server

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Text (Text)
import Data.Dynamic

import Control.Monad.State.Strict

class Typeable a => Parsable a where
  parseRequest :: Text -> Maybe a
    

-- class Typeable h => RequestHandler h where
-- instance RequestHandler Controller
-- instance Parsable a 
--   => RequestHandler (a -> Controller)
-- instance (Parsable a, Parsable b) 
--   => RequestHandler (a -> b -> Controller)
-- instance (Parsable a, Parsable b, Parsable c)
--   => RequestHandler (a -> b -> c -> Controller)
-- instance (Parsable a, Parsable b, Parsable c, Parsable d)
--   => RequestHandler (a -> b -> c -> d -> Controller)

-- get :: (WebMonad m, RequestHandler h) => Text -> h -> Frankie m ()
-- get path handler = registerByMethodAndPath path handler

-- registerHandler :: (Monad m, RequestHandler h) => h -> (Request m -> 
-- -> Frankie m ()
-- registerHandler ::Requ

type Frankie m a = StateT ServerConfig m a


data ServerConfig = ServerConfig {
  cfgPort        :: Port,
  cfgHostPref    :: HostPreference,
  cfgDispatchMap :: [DynController]
}


type DynController = ()

-- FIXME
newtype Controller m a = Controller { 
  runController :: StateT (Request m) m a
} deriving (Functor, Applicative, Monad)

