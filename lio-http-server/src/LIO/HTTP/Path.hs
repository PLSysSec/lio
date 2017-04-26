{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
import Data.String
import Data.Dynamic
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Maybe (maybe, listToMaybe)
import Control.Monad (unless, forM, foldM)
-- import Control.Monad.Trans.State.Lazy


-- get :: Typeable a => [String] -> a -> Controller
get :: Typeable a => [String] -> a -> Map String String -> Map TypeRep ReadDynS -> Controller
get pathInfo handler queryMap parseMap = do
  let pathVars   = filter (\pp -> head pp == ':') pathInfo
      tyArgs     = getTypeOfArgs handler
      tyPathVars = zip pathVars tyArgs
  putStrLn $ show $ tyArgs
  unless (length pathVars == length tyArgs) $ 
    fail $ "Number of path variables (" ++ show (length pathVars) ++ ") and type arguments (" ++ show (length tyArgs) ++ ") do not match"
  parsedArgs <- forM tyPathVars $ \(var, ty) -> do
        str     <- lookup var queryMap $ "Could not find query variable " ++ show var
        parser  <- lookup ty parseMap  $ "Could not find parser for " ++ show ty
        case parser str of
          Just x -> return (var, x)
          _      -> fail $ "failed to parse " ++ show ty
  let doit partialHandler (var, arg) = case dynApply partialHandler arg of
                                         Just x -> return x
                                         _      -> fail $ "failed when applying " ++ show var
  dynAct <- foldM doit (toDyn handler) parsedArgs
  fromDyn dynAct (fail "failed to convert from dynamic")

  -- foldM doit (Just dynHandler) tyPathVars
    where init' xs = if null xs then [] else init xs
          lookup name map err = maybe (fail err) return (Map.lookup name map)


class Typeable a => Parsable a where
  parseFunc :: a -> (TypeRep, ReadDynS)

instance (Typeable a, Read a) => Parsable a where
  parseFunc a = (typeOf a, \str -> toDyn `fmap` (maybeRead' a str))
    where maybeRead' :: Read a => a -> String -> Maybe a
          maybeRead' _ = fmap fst . listToMaybe . reads


newtype PostId = PostId { postId :: Int }
  deriving (Show, Eq, Read, Typeable)

app = do
   let qMap = Map.fromList [(":uid", "123"), (":pid", (show $ PostId 1337))]
       parseMap = Map.fromList [parseFunc (undefined :: Int), parseFunc (undefined :: PostId)]
   -- get ["users", ":uid"] userHandler qMap parseMap
   get ["users", ":uid", "posts", ":pid"] userPostHandler qMap parseMap
--

type ReadDynS = String -> Maybe Dynamic -- should probably be Either, not Maybe


--



parseMap = undefined


userHandler :: Int -> Controller
userHandler uid = do
  putStrLn $ "userHandler: " ++ show uid
  
userPostHandler :: Int -> PostId -> Controller
userPostHandler uid pid = do
  putStrLn $ "userPostHandler: " ++ show uid ++ " @ " ++ show pid

type Controller = IO ()


getTypeOfArgs :: Typeable a => a -> [TypeRep]
getTypeOfArgs f = getArgs' $ typeRepArgs $ typeOf f
  where getArgs' :: [TypeRep] -> [TypeRep]
        getArgs' [a, b] = a : (getArgs' $ typeRepArgs b)
        getArgs' _      = []
