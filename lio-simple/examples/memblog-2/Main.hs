{-# LANGUAGE OverloadedStrings #-}
module Main where

import Application
import Data.Monoid
import Control.Monad
import Network.Wai.Middleware.RequestLogger
import System.Environment
import Web.Simple

import LIO
import LIO.Run (evalLIO, privInit)
import LIO.TCB (ioTCB)
import LIO.DCLabel
import LIO.Web.Simple
import LIO.Web.Simple.DCLabel
import LIO.Web.Simple.Auth
import LIO.Web.Simple.TCB (runP)



main :: IO ()
main = do
    -- Run app
    env <- getEnvironment
    let port = maybe 3000 read $ lookup "PORT" env
    evalDC $ app $ runP port logStdout mempty . authMiddleware
                                              . setClearanceMiddleware
                                              . setUserPrivsTCB

authMiddleware :: SimpleDCMiddleware
authMiddleware app priv = basicAuth "memblog-2" (\_ _ -> return True) (app priv)

setClearanceMiddleware :: SimpleDCMiddleware
setClearanceMiddleware app priv req = do
  case lookup "X-User" $ requestHeaders req of
    Just user -> setClearance $ principalBS user %% cTrue
    _         -> setClearance dcPublic
  app priv req

setUserPrivsTCB :: SimpleDCMiddleware
setUserPrivsTCB app priv req = do
  case lookup "X-User" $ requestHeaders req of
    Nothing   -> app priv req
    Just user -> do
       upriv <- ioTCB . privInit $ toCNF $ principalBS user
       app (priv `mappend` upriv) req
