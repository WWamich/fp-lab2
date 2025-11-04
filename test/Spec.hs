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
  , testCase "lookup after insert finds the element" $
      let dict = insert "a" 100 empty
      in lookup "a" dict @?= Just 100

  , testCase "inserting an existing key updates the value" $
      let dict1 = insert "a" 100 empty
          dict2 = insert "a" 200 dict1
      in lookup "a" dict2 @?= Just 200

  , testCase "size is updated correctly after inserts" $
      let dict = insert "c" 3 (insert "b" 2 (insert "a" 1 empty))
      in dictSize dict @?= 3

  , testCase "size doesn't change when updating a key" $
      let dict = insert "a" 2 (insert "a" 1 empty)
      in dictSize dict @?= 1

  , testCase "collision is handled correctly" $
      let pairs = [(show x, x) | x <- [1..20]]
          dict = foldr (\(k, v) acc -> insert k v acc) empty pairs
      in do
        dictSize dict @?= 20
        lookup "15" dict @?= Just 15
        lookup "7" dict @?= Just 7
  ]