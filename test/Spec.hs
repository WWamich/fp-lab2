module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Prelude hiding (lookup)
import Data.OAHashMap.Internal

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Data.OAHashMap Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "lookup on an empty dictionary returns Nothing" $
      lookup "a" (empty :: OADict String Int) @?= Nothing
  ]