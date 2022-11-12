--https://stackoverflow.com/questions/4787421/how-do-i-use-fix-and-how-does-it-work

module Task8_Fix where

import Data.Function
import Test.QuickCheck
import Test.Tasty.HUnit

iterateElement :: a -> [a]
iterateElement = fix (\f -> \x -> x : f(x))

prop_iterateElement :: Integer -> Property
prop_iterateElement x = (take 100 (iterateElement x)) === (take 100 [x, x..])

fibonacci :: Integer -> Integer
fibonacci = fix (\f -> \n -> 
    if n < 2 
        then 1 
        else f (n - 1) + f (n - 2) 
    )

factorial :: Integer -> Integer
factorial = fix (\f -> \n -> 
    if n == 0
        then 1 
        else f (n - 1) * n
    )

mapFix :: (a -> b) -> [a] -> [b]
mapFix = fix (\f -> \g -> \list ->
    if null list
        then [] 
        else (g (head list)) : (f g (tail list))
    )

prop_mapFix_eq_map :: Blind (Integer -> Char) -> [Integer] -> Property
prop_mapFix_eq_map (Blind f) xs = mapFix f xs === map f xs

unit_fibonacci_5 :: Assertion
unit_fibonacci_5 = fibonacci 5 @?= 8

unit_fibonacci_15 :: Assertion
unit_fibonacci_15 = fibonacci 15 @?= 987

unit_factorial_5 :: Assertion
unit_factorial_5 = factorial 5 @?= 120

unit_factorial_10 :: Assertion
unit_factorial_10 = factorial 10 @?= 3628800

unit_maxFix_5 :: Assertion
unit_maxFix_5 = mapFix (*5) [-5..5] @?= map (*5) [-5..5]

unit_maxFix_sign :: Assertion
unit_maxFix_sign = mapFix signum [-5..5] @?= map signum [-5..5]