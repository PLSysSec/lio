{-# LANGUAGE OverloadedStrings #-}
import LIO.HTTP.Server
import LIO.HTTP.Server.Responses

main :: IO ()
main = server 3000 "127.0.0.1" app

app :: DCApplication
app _ = return (okHtml "Hello World!")
