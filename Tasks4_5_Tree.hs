module Tasks4_5_Tree where

import Data.Foldable
import Data.List (sort)
import Test.QuickCheck
import Test.Tasty.HUnit

data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving (Show)
    
treeIsEmpty :: Tree a -> Bool
treeIsEmpty Leaf = True
treeIsEmpty (Node _ _ _) = False

treeSize :: Tree a -> Int
treeSize Leaf = 0 :: Int
treeSize (Node a l r) = 1 + treeSize l + treeSize r

treeFind :: (Ord a) => a -> Tree a -> Bool
treeFind key Leaf = False
treeFind key (Node a l r) = if key == a
    then True
    else (if key >= a 
        then treeFind key r
        else treeFind key l)
        
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert key Leaf = Node key Leaf Leaf
treeInsert key (Node a l r) = if key >= a 
    then Node a l (treeInsert key r)
    else Node a (treeInsert key l) r
    
fromList :: (Ord a) => [a] -> Tree a
fromList [] = Leaf
fromList (x:xs) = treeInsert x (fromList xs)


-- https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Foldable.html
instance Foldable Tree where
    foldMap f Leaf = mempty
    foldMap f (Node a l r) = foldMap f l `mappend` f a `mappend` foldMap f r

unit_treeIsEmpty_Leaf :: Assertion
unit_treeIsEmpty_Leaf = treeIsEmpty Leaf @?= True

unit_treeIsEmpty_Node :: Assertion
unit_treeIsEmpty_Node = treeIsEmpty (fromList [1, 0, 3, 2, 5, 8]) @?= False

unit_treeFind_True :: Assertion
unit_treeFind_True = treeFind 5 (fromList [1, 0, 3, 2, 5, 8]) @?= True

unit_treeFind_False :: Assertion
unit_treeFind_False = treeFind 9 (fromList [1, 0, 3, 2, 5, 8]) @?= False

unit_treeInsert_Find :: Assertion
unit_treeInsert_Find = treeFind 9 
    (treeInsert 9
        (fromList [1, 0, 3, 2, 5, 8])
    )
    @?= True
    
unit_treeSize_0v1 :: Assertion
unit_treeSize_0v1 = treeSize Leaf @?= 0

unit_treeSize_1 :: Assertion
unit_treeSize_1 = treeSize (fromList [1]) @?= 1
    
unit_treeSize_7 :: Assertion
unit_treeSize_7 = treeSize
    (treeInsert 9
        (fromList [1, 0, 3, 2, 5, 8])
    )
    @?= 7
    
unit_treeSize_7_dups :: Assertion
unit_treeSize_7_dups = treeSize
    (treeInsert 1
        (fromList [1, 0, 3, 2, 5, 8])
    )
    @?= 7

unit_treeFind_empty :: Assertion
unit_treeFind_empty = treeFind 5 Leaf @?= False