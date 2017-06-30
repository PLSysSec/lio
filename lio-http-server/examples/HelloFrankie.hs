{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (log)
import LIO.HTTP.Server.Frankie
import LIO.HTTP.Server.Frankie.Loggers

main :: IO ()
main = runFrankieServer "prod" $ do
  mode "prod" $ do
    host "*"
    port 3030
    appState ()
    logger INFO colorStdErrLogger
    openFileLogger "/tmp/wtf.log.0" >>= logger DEBUG
      
  mode "dev" $ do
    host "127.0.0.1"
    port 3000
    appState ()
  --
  dispatch $ do
    get "/" top
    get "/a/b" top
    get "/fail" doFail
    get "/users/:uid" showUser
    get "/users/:uid/posts/:pid" showUserPost
    fallback $ do
      req <- request
      log INFO $ "Not sure how to handle: " ++ show req
      respond $ notFound
  --
  onError $ \err -> do
    log ERROR $ "Controller failed with " ++ displayException err
    respond $ serverError "bad bad nab"

top :: DCController s
top = respond $ okHtml "Woot"

showUser :: Int -> DCController ()
showUser uid = do
  log INFO $ "uid = " ++ show uid
  respond $ okHtml "showUser done!"

newtype PostId = PostId Int
   deriving Eq

instance Show PostId where
   show (PostId i) = show i

instance Parseable PostId where
  parseText t = case parseText t of
                  Just i | i > 0 -> Just (PostId i)
                  _ -> Nothing

showUserPost :: Int -> PostId -> DCController ()
showUserPost uid pid = do
  log INFO $ "uid = " ++ show uid
  log INFO $ "pid = " ++ show pid
  respond $ okHtml "showUserPost done!"

doFail :: DCController ()
doFail = do
  log WARNING $ "about to throw an exception"
  fail "w00t"
