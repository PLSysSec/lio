{-# LANGUAGE OverloadedStrings #-}
module Main where

import Application
import Control.Monad
import Network.Wai.Middleware.RequestLogger
import System.Environment
import Web.Simple

import LIO.DCLabel
import LIO.FS.Simple
import LIO.FS.Simple.DCLabel
import LIO.Web.Simple.TCB (run)

import System.Directory
import System.FilePath


main :: IO ()
main = do
  fsRoot <- (\d -> d </> "liofs") `liftM` getCurrentDirectory
  withDCFS fsRoot $ do
    -- label views and layouts public
    labelDirectoryRecursively dcPublic $ fsRoot </> "views"
    labelDirectoryRecursively dcPublic $ fsRoot </> "layouts"

    -- run app
    env <- getEnvironment
    let port = maybe 3000 read $ lookup "PORT" env
    evalDC $ app $ run port logStdout
