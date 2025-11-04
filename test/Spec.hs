{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE StandaloneDeriving  #-} 

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Prelude hiding (lookup)
import Data.OAHashMap.Internal
import Data.Hashable (Hashable)


main :: IO ()
main = defaultMain tests 


tests :: TestTree
tests = testGroup "Data.OAHashMap Tests" [unitTests, properties]

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
      , testCase "delete removes an existing key" $
      let dict = insert "a" 1 empty
          dict' = delete "a" dict
      in do
        lookup "a" dict' @?= Nothing
        dictSize dict' @?= 0

  , testCase "delete does nothing for a non-existent key" $
      let dict = insert "a" 1 empty
          dict' = delete "b" dict
      in do
        lookup "a" dict' @?= Just 1
        dictSize dict' @?= 1

  , testCase "delete correctly handles collisions" $
      let dict :: OADict Int Int
          dict = insert 17 170 (insert 1 10 empty)
          dict' = delete 1 dict
      in do
        lookup 1 dict' @?= Nothing
        lookup 17 dict' @?= Just 170
        dictSize dict' @?= 1
    , testCase "mapDict applies a function to all values" $
      let dict = fromList [("a", 1), ("b", 2)]
          dict' = mapDict (*10) dict
      in do
        lookup "a" dict' @?= Just 10
        lookup "b" dict' @?= Just 20
        dictSize dict' @?= 2

  , testCase "filterDict keeps elements that satisfy the predicate" $
      let dict = fromList [("a", 10), ("b", 5), ("c", 20)]
          dict' = filterDict (> 7) dict
      in do
        lookup "a" dict' @?= Just 10
        lookup "b" dict' @?= Nothing
        lookup "c" dict' @?= Just 20
        dictSize dict' @?= 2
  , testCase "mappend (<>) combines two dictionaries" $
      let dict1 = fromList [("a", 1), ("b", 2)]
          dict2 = fromList [("b", 3), ("c", 4)]
          dict' = dict1 <> dict2
      in do
        lookup "a" dict' @?= Just 1
        lookup "b" dict' @?= Just 3
        lookup "c" dict' @?= Just 4
        dictSize dict' @?= 3

  ]

properties :: TestTree
properties = testGroup "Property tests"
  [ qc_prop_insert_lookup
  ]

instance (Eq k, Hashable k, Arbitrary k, Arbitrary v) => Arbitrary (OADict k v) where
  arbitrary = fromList <$> arbitrary 

qc_prop_insert_lookup :: TestTree
qc_prop_insert_lookup = testProperty "lookup k (insert k v dict) == Just v" $
  \(key :: String) (value :: Int) (dict :: OADict String Int) ->
    lookup key (insert key value dict) == Just value

