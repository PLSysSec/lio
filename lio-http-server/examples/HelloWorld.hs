{-# LANGUAGE OverloadedStrings #-}
import LIO.HTTP.Server

main :: IO ()
main = server 3000 "127.0.0.1" app

app :: DCApplication
app _ = do
  return $ Response status200 [] "Hello World!"
