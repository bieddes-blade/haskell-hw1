module Task3_Nat where

import Test.Tasty.HUnit
import Test.QuickCheck

data Nat = Z | S Nat deriving Show

instance Arbitrary Nat where
    arbitrary = intToNat . getNonNegative <$> arbitrary

intToNat :: Integer -> Nat
intToNat i = if i <= 0 
    then Z
    else S ( intToNat ( i - 1 ) )

natToInt :: Nat -> Integer
natToInt Z = 0
natToInt (S a) = (natToInt a) + 1

prop_nat_int_roundtrip1 :: Nat -> Property
prop_nat_int_roundtrip1 n = intToNat (natToInt n) === n

prop_nat_int_roundtrip2 :: Integer -> Property
prop_nat_int_roundtrip2 i = i >= 0 ==> natToInt (intToNat i) === i

instance Num Nat where
   (+) Z n = n
   (+) n Z = n
   (+) (S a) n = S (a + n)

   (*) Z n = Z
   (*) n Z = Z
   (*) (S a) n = a * n + n

   (-) Z n = Z
   (-) n Z = n
   (-) (S a) (S b) = a - b
   
instance Eq Nat where
   (==) Z Z = True
   (==) Z (S _) = False
   (==) (S _) Z = False
   (==) (S a) (S b) = a == b
   
instance Ord Nat where
   compare Z Z = EQ
   compare Z (S _) = LT
   compare (S _) Z = GT
   compare (S a) (S b) = compare a b
   
isEven :: Nat -> Bool
isEven Z = True
isEven (S a) = not (isEven a)

divideNat :: Nat -> Nat -> Nat
divideNat a b = if (a < b) 
    then Z
    else S ( divideNat (a - b) b )

modNat :: Nat -> Nat -> Nat
modNat a b = if (a < b) 
    then a
    else modNat (a - b) b 

unit_nat_summ_2 :: Assertion
unit_nat_summ_2 = natToInt ( (intToNat 1) + (intToNat 1) ) @?= 2

unit_nat_summ_100 :: Assertion
unit_nat_summ_100 = natToInt ( (intToNat 39) + (intToNat 61) ) @?= 100

unit_nat_subt_0 :: Assertion
unit_nat_subt_0 = natToInt ( (intToNat 41) - (intToNat 41) ) @?= 0

unit_nat_subt_2 :: Assertion
unit_nat_subt_2 = natToInt ( (intToNat 41) - (intToNat 39) ) @?= 2

unit_nat_mult_10 :: Assertion
unit_nat_mult_10 = natToInt ( (intToNat 2) * (intToNat 5) ) @?= 10

unit_nat_eq :: Assertion
unit_nat_eq = (intToNat 3) == ( intToNat (2 + 1) ) @?= True

unit_nat_neq :: Assertion
unit_nat_neq = (intToNat 3) == (intToNat 2) @?= False

unit_nat_even :: Assertion
unit_nat_even = isEven ( intToNat 4 ) @?= True

unit_nat_odd :: Assertion
unit_nat_odd = isEven ( intToNat 3 ) @?= False

unit_nat_divide_10 :: Assertion
unit_nat_divide_10 = natToInt ( divideNat (intToNat 32) (intToNat 3) ) @?= 10

unit_nat_mod_2 :: Assertion
unit_nat_mod_2 = natToInt ( modNat (intToNat 32) (intToNat 3) ) @?= 2