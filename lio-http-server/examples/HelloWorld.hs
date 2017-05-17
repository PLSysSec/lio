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
          3 -> toApp controller3 ()
          2 -> app2
          _ -> app1

dcPutStrLn :: String -> DCController ()
dcPutStrLn str = lift . ioTCB $ putStrLn str

controller3 :: DCController ()
controller3 = do
  req <- request 
  dcPutStrLn $ show req
  respond $ okHtml "Yo controller!"

app2 :: DCApplication
app2 _ = return (okHtml "Yo okHtml!")

app1 :: DCApplication
app1 _ = return $ Response status200 [] "Hello World!"
