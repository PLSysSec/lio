{-# LANGUAGE OverloadedStrings #-}
module Main where

import Application
import Control.Monad
import Network.Wai.Middleware.RequestLogger
import System.Environment
import Web.Simple

import Data.Monoid

import LIO
import LIO.DCLabel
import LIO.DCLabel
import LIO.Run (evalLIO, privInit)
import LIO.TCB (ioTCB, getLIOStateTCB, putLIOStateTCB)
import LIO.Web.Simple
import LIO.Web.Simple.Auth
import LIO.Web.Simple.DCLabel
import LIO.Web.Simple.TCB

import qualified Data.ByteString.Char8 as S8


main :: IO ()
main = do
  -- run app with user privs
  env <- getEnvironment
  let port = maybe 3000 read $ lookup "PORT" env
  evalDC $ app $ runP port logStdout mempty . authMiddleware
                                            . setClearanceMiddleware
                                            . setUserPrivsTCB


authMiddleware :: SimpleDCMiddleware
authMiddleware app priv req = do
 basicAuth "chattr" (\_ _ -> return True) (app priv) req

setClearanceMiddleware :: SimpleDCMiddleware
setClearanceMiddleware app priv req = do
  case lookup "X-User" $ requestHeaders req of
    Nothing   -> return ()
    Just user -> setClearance $ principalBS user %% cTrue
  app priv req


setUserPrivsTCB :: SimpleDCMiddleware
setUserPrivsTCB app priv req = do
  case lookup "X-User" $ requestHeaders req of
    Nothing   -> app priv req
    Just user -> do
       upriv <- ioTCB . privInit $ toCNF $ principalBS user
       app (priv `mappend` upriv) req
