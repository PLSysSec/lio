{-# LANGUAGE OverloadedStrings #-}
import LIO.HTTP.Server
import LIO.HTTP.Server.Frankie

main :: IO ()
main = server 3000 "127.0.0.1" app

app :: ServerConfig
app = do
  -- production $ do
  --   port 3000
  --   host "127.0.0.1"
  dispatchTable $ do
    get "/users/:uid/" showUser
    get "/users/:uid/posts/:pid" showUserPost
  -- onError $ \req -> ...
