module Task6_Split where

import Data.List.NonEmpty
import Test.QuickCheck
import Test.Tasty.HUnit

splitFold :: (Eq a) => a -> a -> NonEmpty [a] -> NonEmpty [a]
splitFold sep cur (x :| xs) = if (sep == cur) 
    then [] :| (x : xs)
    else (cur : x) :| xs

splitOn :: (Eq a) => a -> [a] -> NonEmpty [a]
splitOn sep list = foldr (splitFold sep) (pure []) list

joinFold :: (Eq a) => a -> [a] -> [a] -> [a]
joinFold sep cur [] = cur
joinFold sep cur list = cur ++ (sep : list)

joinWith :: (Eq a) => a -> NonEmpty [a] -> [a]
joinWith sep list = foldr (joinFold sep) [] list

ptf = (fromList ["path", "to", "file"])
pt = (fromList ["path", "to"])

unit_splitOn_1 :: Assertion
unit_splitOn_1 = (splitOn '/' "path/to/file") @?= ptf

unit_splitOn_2 :: Assertion
unit_splitOn_2 = (splitOn ' ' "path to") @?= pt

unit_joinWith_1 :: Assertion
unit_joinWith_1 = (joinWith '/' ptf) @?= "path/to/file"

unit_joinWith_2 :: Assertion
unit_joinWith_2 = (joinWith ' ' pt) @?= "path to"

prop_splitOn_joinWith :: Char -> String -> Property
prop_splitOn_joinWith c s = joinWith c (splitOn c s) === s