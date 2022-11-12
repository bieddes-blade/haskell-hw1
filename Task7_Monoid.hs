{-# LANGUAGE InstanceSigs #-}

module Task7_Monoid where

import Test.QuickCheck
import Test.Tasty.HUnit

associativity :: (Eq a, Semigroup a, Show a) => a -> a -> a -> Property
associativity a b c = (a <> b) <> c === a <> (b <> c)

listConcat :: Maybe [a] -> [a] -> [a]
listConcat Nothing list = list
listConcat (Just list1) list2 = list1 ++ list2

maybeConcat :: [Maybe [a]] -> [a]
maybeConcat list = foldr listConcat [] list

monoidConcat :: (Monoid a, Monoid b) => Either a b -> (a, b) -> (a, b)
monoidConcat (Left item) (l, r) = (item <> l, r)
monoidConcat (Right item) (l, r) = (l, item <> r)

eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat list = foldr monoidConcat (mempty, mempty) list

data NonEmpty a = a :| [a]
    deriving (Eq, Show)

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary

instance Semigroup (NonEmpty a) where
    (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
    (<>) (x :| xs) (y :| ys) = x :| (xs ++ (y : ys))

data ThisOrThat a b = This a | That b | Both a b
    deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (ThisOrThat a b) where
    arbitrary =
        oneof
            [This <$> arbitrary, That <$> arbitrary, Both <$> arbitrary <*> arbitrary]

instance (Ord a, Ord b) => Semigroup (ThisOrThat a b) where
    (<>) :: ThisOrThat a b -> ThisOrThat a b -> ThisOrThat a b
    This a1 <> This a2 = This (min a1 a2)
    This a1 <> That b1 = Both a1 b1
    That b1 <> This a1 = Both a1 b1
    That b1 <> That b2 = That (min b1 b2)
    This a1 <> Both a2 b1 = Both (min a1 a2) b1
    That b1 <> Both a1 b2 = Both a1 (min b1 b2)
    Both a1 b1 <> This a2 = Both (min a1 a2) b1
    Both a1 b1 <> That b2 = Both a1 (min b1 b2)
    Both a1 b1 <> Both a2 b2 = Both (min a1 a2) (min b1 b2)

prop_NonEmpty_associativity
    :: NonEmpty Integer -> NonEmpty Integer -> NonEmpty Integer -> Property
prop_NonEmpty_associativity = associativity

prop_ThisOrThat_associativity
    :: ThisOrThat Integer Char
    -> ThisOrThat Integer Char
    -> ThisOrThat Integer Char
    -> Property
prop_ThisOrThat_associativity = associativity