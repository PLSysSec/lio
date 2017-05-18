{-# LANGUAGE OverloadedStrings #-}
import LIO.HTTP.Server
import LIO.HTTP.Server.Frankie

main :: IO ()
main = server 3000 "127.0.0.1" app

--app :: DCFrankieApp
app = do
  -- production $ do
  --   port 3000
  --   host "127.0.0.1"
  get "/" top
  get "/users/:uid/" showUser
  -- get "/users/:uid/posts/:pid" showUserPost
  -- onError onErr
  -- onDispatchError onDispatchErr

top :: DCController s
top = respond $ okHtml "Woot"

showUser :: Int -> DCController s
showUser uid = do
  dcPutStrLn $ "uid = " ++ show uid
  respond $ okHtml "showUser done!"


dcPutStrLn :: String -> DCController s
dcPutStrLn str = lift . ioTCB $ putStrLn str
