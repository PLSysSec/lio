{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Control.Exception
import Control.Monad
import LIO.HTTP.Server.Frankie
import qualified Data.Map as Map

import Data.Text (Text)
import Data.Dynamic

main = defaultMain tests

tests = [ 
  testGroup "Frankie:toPathSegment " [
    testCase "produces expected list" test_toPathSegments1,
    testCase "paths equal up to alpha" test_toPathSegments2,
    testCase "paths not equal" test_toPathSegments3
  ],
  testGroup "Frankie:runFrankieConfig" [
    testCase "port and host set" test_runFrankieConfig1,
    testCase "cannot set port 2x" test_runFrankieConfig2,
    testCase "cannot set host 2x" test_runFrankieConfig3,
    testCase "get adds route" test_runFrankieConfig4,
    testCase "cannot get add same route" test_runFrankieConfig5,
    testCase "get and put on same path" test_runFrankieConfig6,
    testCase "get on diff path ok" test_runFrankieConfig7
  ],
  testGroup "Frankie:controller-related" [
    testCase "number of controller args too few" test_mismatchRouteAndController1,
    testCase "number of controller args too many" test_mismatchRouteAndController2
  ]
  ]


test_toPathSegments1 = do
  segs <- toPathSegments "/a/:b/c/:d"
  segs @?= [Dir "a", Var ":b" 1, Dir "c", Var ":d" 3]

test_toPathSegments2 = do
  seg1 <- toPathSegments "/a/:b/c/:d" 
  seg2 <- toPathSegments "/a/:boo/c/:dood"
  seg1 @?= seg2

test_toPathSegments3 = do
  seg1 <- toPathSegments "/a/:b/c/:d" 
  seg2 <- toPathSegments "/a/:b/x/:d"
  seg1 == seg2 @?= False

test_runFrankieConfig1 = do
  cfg <- runFrankieConfig $ do
          port 3030
          host "*"
  cfgPort cfg @?= Just 3030
  cfgHostPref cfg @?= Just "*"

test_runFrankieConfig2 = do
  (void . runFrankieConfig $ do
    port 3030
    host "*"
    port 3030)
   `catch` (\(e :: InvalidConfigException) -> return ())

test_runFrankieConfig3 = do
  (void . runFrankieConfig $ do
    port 3030
    host "*"
    host "*")
   `catch` (\(e :: InvalidConfigException) -> return ())

test_runFrankieConfig4 = do
  cfg <- runFrankieConfig $ do
    host "127.0.0.1" ; port 3030
    get "/x/:y" nullCtrl1
  let map = cfgDispatchMap cfg
  segs <- toPathSegments "/x/:yo"
  Map.keys map @?= [(methodGet, segs)]

test_runFrankieConfig5 = do
  (void . runFrankieConfig $ do
    port 3030
    host "*"
    get "/x/:y" nullCtrl1
    get "/x/:yoyo" nullCtrl1
    )
   `catch` (\(e :: InvalidConfigException) -> return ())

test_runFrankieConfig6 = do
  cfg <- runFrankieConfig $ do
    host "127.0.0.1" ; port 3030
    get "/x/:y" nullCtrl1
    put "/x/:y" nullCtrl1
  let map = cfgDispatchMap cfg
  segs <- toPathSegments "/x/:yo"
  Map.keys map @?= [(methodGet, segs), (methodPut, segs)]

test_runFrankieConfig7 = do
  cfg <- runFrankieConfig $ do
    host "127.0.0.1" ; port 3030
    get "/x/:y" nullCtrl1
    get "/y/:x" nullCtrl1
  let map = cfgDispatchMap cfg
  segs1 <- toPathSegments "/x/:yo"
  segs2 <- toPathSegments "/y/:yo"
  Map.keys map @?= [(methodGet, segs1), (methodGet, segs2)]

test_mismatchRouteAndController1 = do
  (void . runFrankieConfig $ do
    port 3030
    host "*"
    get "/x/:y" nullCtrl0
    )
   `catch` (\(e :: InvalidConfigException) -> return ())

test_mismatchRouteAndController2 = do
  (void . runFrankieConfig $ do
    port 3030
    host "*"
    get "/x/:y" nullCtrl2
    )
   `catch` (\(e :: InvalidConfigException) -> return ())

nullCtrl0 :: DCController ()
nullCtrl0 = return ()

nullCtrl1 :: Int -> DCController ()
nullCtrl1 _ = return ()

nullCtrl2 :: Int -> String -> DCController ()
nullCtrl2 _ _ = return ()
