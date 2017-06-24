{-# LANGUAGE OverloadedStrings #-}
import LIO.HTTP.Server.Frankie
import LIO.TCB (ioTCB)
import Control.Monad.Trans.Class (lift)

main :: IO ()
main = runFrankieServer "prod" $ do
  mode "production" $ do
    host "*"
    port 3030
    appState ()
  mode "dev" $ do
    host "127.0.0.1"
    port 3000
    appState ()
  --
  dispatch $ do
    get "/" top
    get "/a/b" top
    get "/users/:uid" showUser
    get "/users/:uid/posts/:pid" showUserPost

  -- TODO:
  -- onError onErr
  -- onDispatchError onDispatchErr

top :: DCController s
top = respond $ okHtml "Woot"

showUser :: Int -> DCController ()
showUser uid = do
  dcPutStrLn $ "uid = " ++ show uid
  respond $ okHtml "showUser done!"

-- newtype PostId = PostId Int
--   deriving (Eq)
-- instance Show PostId where
--   show (PostId i) = show i
-- instance Read PostId where
--  read s = PostId $ read s

showUserPost :: Int -> String -> DCController ()
showUserPost uid pid = do
  dcPutStrLn $ "uid = " ++ show uid
  dcPutStrLn $ "pid = " ++ show pid
  respond $ okHtml "showUserPost done!"

dcPutStrLn :: String -> DCController ()
dcPutStrLn str = lift . ioTCB $ putStrLn str
