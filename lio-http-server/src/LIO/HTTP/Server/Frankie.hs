import LIO.HTTP.Server

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Dynamic

import Control.Monad.State.Strict

get :: (Monad m, Typeable a) => String -> a -> Frankie m ()
get path handler = registerHandler path handler


type Frankie m a = StateT ServerConfig m a


data ServerConfig = ServerConfig {
  cfgDispatchMap :: Map Method Controller
}


-- FIXME
type Controller = IO ()

