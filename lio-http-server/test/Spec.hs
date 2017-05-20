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
    testCase "cannot set state 2x" test_runFrankieConfig2a,
    testCase "get adds route" test_runFrankieConfig4,
    testCase "cannot get add same route" test_runFrankieConfig5,
    testCase "get and put on same path" test_runFrankieConfig6,
    testCase "get on diff path ok" test_runFrankieConfig7
  ],
  testGroup "Frankie:controller-related" [
    testCase "number of controller args too few" test_mismatchRouteAndController1,
    testCase "number of controller args too many" test_mismatchRouteAndController2
  ],
  testGroup "Frankie:match-path" [
    testCase "match path no vars" test_matchPath1,
    testCase "match path with vars" test_matchPath2,
    testCase "dont match path no vars" test_matchPath3,
    testCase "trailing / should not matter" test_matchPath4,
    testCase "match empty" test_matchPath5,
    testCase "dont match nonempty route with empty req" test_matchPath6,
    testCase "dont match empty route with nonempty req" test_matchPath7
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

test_toPathSegments4 = do
  seg1 <- toPathSegments "/a/:b/c/:d"
  seg2 <- toPathSegments "/a/:b/c/:d/"
  seg1 == seg2 @?= True

test_runFrankieConfig1 = do
  cfg <- runFrankieConfig $ do
          port 3030
          host "*"
          appState ()
  cfgPort cfg @?= Just 3030
  cfgHostPref cfg @?= Just "*"
  cfgAppState cfg @?= Just ()

test_runFrankieConfig2 = do
  (void . runFrankieConfig $ do
    port 3030
    host "*"
    appState ()
    port 3030)
   `catch` (\(e :: InvalidConfigException) -> return ())

test_runFrankieConfig2a = do
  (void . runFrankieConfig $ do
    port 3030
    host "*"
    appState ()
    appState ()
    )
   `catch` (\(e :: InvalidConfigException) -> return ())

test_runFrankieConfig3 = do
  (void . runFrankieConfig $ do
    port 3030
    host "*"
    appState ()
    host "*")
   `catch` (\(e :: InvalidConfigException) -> return ())

test_runFrankieConfig4 = do
  cfg <- runFrankieConfig $ do
    host "127.0.0.1" ; port 3030 ; appState ()
    get "/x/:y" nullCtrl1
  let map = cfgDispatchMap cfg
  segs <- toPathSegments "/x/:yo"
  Map.keys map @?= [(methodGet, segs)]

test_runFrankieConfig5 = do
  (void . runFrankieConfig $ do
    host "127.0.0.1" ; port 3030 ; appState ()
    get "/x/:y" nullCtrl1
    get "/x/:yoyo" nullCtrl1
    )
   `catch` (\(e :: InvalidConfigException) -> return ())

test_runFrankieConfig6 = do
  cfg <- runFrankieConfig $ do
    host "127.0.0.1" ; port 3030 ; appState ()
    get "/x/:y" nullCtrl1
    put "/x/:y" nullCtrl1
  let map = cfgDispatchMap cfg
  segs <- toPathSegments "/x/:yo"
  Map.keys map @?= [(methodGet, segs), (methodPut, segs)]

test_runFrankieConfig7 = do
  cfg <- runFrankieConfig $ do
    host "127.0.0.1" ; port 3030 ; appState ()
    get "/x/:y" nullCtrl1
    get "/y/:x" nullCtrl1
  let map = cfgDispatchMap cfg
  segs1 <- toPathSegments "/x/:yo"
  segs2 <- toPathSegments "/y/:yo"
  Map.keys map @?= [(methodGet, segs1), (methodGet, segs2)]

test_mismatchRouteAndController1 = do
  (void . runFrankieConfig $ do
    host "127.0.0.1" ; port 3030 ; appState ()
    get "/x/:y" nullCtrl0
    )
   `catch` (\(e :: InvalidConfigException) -> return ())

test_mismatchRouteAndController2 = do
  (void . runFrankieConfig $ do
    host "127.0.0.1" ; port 3030 ; appState ()
    get "/x/:y" nullCtrl2
    )
   `catch` (\(e :: InvalidConfigException) -> return ())


test_matchPath1 = do
  matchPath [Dir "a", Dir "b", Dir "c"] ["a", "b", "c"] @?= True

test_matchPath2 = do
  matchPath [Dir "a", Var ":b" 1, Dir "c"] ["a", "x", "c"] @?= True

test_matchPath3 = do
  matchPath [Dir "a", Dir "b", Dir "c"] ["a", "x", "c"] @?= False

test_matchPath4 = do
  matchPath [Dir "a", Var ":b" 1, Dir "c"] ["a", ":b", "x"] @?= False

test_matchPath5 = do
  matchPath [] [] @?= True

test_matchPath6 = do
  matchPath [Var ":x" 0] [] @?= False

test_matchPath7 = do
  matchPath [] ["a"] @?= False

nullCtrl0 :: DCController ()
nullCtrl0 = return ()

nullCtrl1 :: Int -> DCController ()
nullCtrl1 _ = return ()

nullCtrl2 :: Int -> String -> DCController ()
nullCtrl2 _ _ = return ()
