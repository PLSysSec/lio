{-# LANGUAGE OverloadedStrings #-}
module Main where

import Application
import Control.Monad
import Network.Wai.Middleware.RequestLogger
import System.Environment
import Web.Simple

import LIO
import LIO.DCLabel
import LIO.Web.Simple
import LIO.Web.Simple.Auth
import LIO.Web.Simple.TCB (run)




main :: IO ()
main = do
    -- Run app
    env <- getEnvironment
    let port = maybe 3000 read $ lookup "PORT" env
    evalDC $ app $ run port logStdout . authMiddleware
                                      . setClearanceMiddleware

authMiddleware :: SimpleMiddleware DC
authMiddleware = basicAuth "memblog-2" (\_ _ -> return True)

setClearanceMiddleware :: SimpleMiddleware DC
setClearanceMiddleware app req = do
  case lookup "X-User" $ requestHeaders req of
    Just user -> setClearance $ principalBS user %% cTrue
    _         -> setClearance dcPublic
  app req
