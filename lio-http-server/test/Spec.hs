{-# LANGUAGE OverloadedStrings #-}
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import LIO.HTTP.Server.Frankie

main = defaultMain tests

tests = [ 
  testGroup "Frankie.toPathSegments " [
    testCase "produces expected list" test_toPathSegments1,
    testCase "paths equal up to alpha" test_toPathSegments2,
    testCase "paths not equal" test_toPathSegments3
  ]
  ]


test_toPathSegments1 = do
  segs <- toPathSegments "/a/:b/c/:d"
  segs @?= [Dir "a", Var ":b", Dir "c", Var ":d"]

test_toPathSegments2 = do
  seg1 <- toPathSegments "/a/:b/c/:d" 
  seg2 <- toPathSegments "/a/:boo/c/:dood"
  seg1 @?= seg2

test_toPathSegments3 = do
  seg1 <- toPathSegments "/a/:b/c/:d" 
  seg2 <- toPathSegments "/a/:b/x/:d"
  seg1 == seg2 @?= False
