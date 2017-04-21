{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.String
import Data.Dynamic
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Control.Monad (unless, forM)


-- get :: Typeable a => [String] -> a -> Controller
get pathInfo dynHandler queryMap parseMap = do
  let pathVars   = filter (\pp -> head pp == ':') pathInfo
      tyHandler  = typeOf dynHandler
      tyArgs     = init' $ typeRepArgs tyHandler
      tyPathVars = zip pathVars tyArgs
  unless (length pathVars == length tyArgs) $ fail "Number of path variables and type arguments do not match"
  forM tyPathVars $ \(var, ty) -> do
    str    <- Map.lookup var queryMap
    parser <- Map.lookup ty parseMap
    parser str

  -- foldM doit 
  -- forM_zip pathVars tyArgs
  -- case Map.
  -- return (pathVars, tyArgs)
    where init' xs = if null xs then [] else init xs




-- strUid <- getQueryParam "uid"
-- userHandler (parseStrUid)


userHandler :: Int -> Controller
userHandler uid = do
  putStrLn $ show uid
  

type Controller = IO ()
