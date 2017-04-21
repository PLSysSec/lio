{-# LANGUAGE OverloadedStrings #-}
import LIO.TCB (ioTCB)
import LIO.HTTP.Server
import LIO.HTTP.Server.Responses
import LIO.HTTP.Server.Controller
import Control.Monad.Trans.Class

main :: IO ()
main = run 1

run :: Int -> IO ()
run nr = server 3000 "127.0.0.1" $ case nr of
          5 -> app5
          4 -> app4
          3 -> app3
          2 -> app2
          _ -> app1

dcPutStrLn :: String -> DCController s
dcPutStrLn str = lift . ioTCB $ putStrLn str

app5 :: DCApplication
app5 = flip toApp () $ do
  foos <- queryParams "foo"
  case foos :: [Int] of
   [x] -> do dcPutStrLn $ show x
             respond $ ok "text/plain" "yo"
   _   -> respond notFound

app4 :: DCApplication
app4 = flip toApp () $ do
  request >>= dcPutStrLn . show
  host <- requestHeader "hOsT"
  dcPutStrLn $ show host
  woo <- requestHeader "woo"
  dcPutStrLn $ show $ woo == Nothing
  respond $ okJson "{}"
  dcPutStrLn $ "you wont see this"


app3 :: DCApplication
app3 = flip toApp () $ do
  req <- request 
  dcPutStrLn $ show req
  respond $ okHtml "Yo controller!"

app2 :: DCApplication
app2 _ = return (okHtml "Yo okHtml!")

app1 :: DCApplication
app1 _ = return $ Response status200 [] "Hello World!"
