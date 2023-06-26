module Primes
( isPrime
, primeFactors
, primes ) where

import           Data.Function.Memoize
import           Data.List             (find, unfoldr)


primes :: [Integer]
primes = 2 : 3 : filter (\n -> length (primeFactors n) == 1) [5, 7 ..]

-- One simple improvement to make this code more efficient is to use memoization
-- to store previously calculated prime numbers, so that we don't have to recalculate
-- them every time we call isPrime.

primes' :: [Integer]
primes' = memoize primes

isPrime :: Integer -> Bool
isPrime n = n `elem` takeWhile (<= n) primes'


primeFactors :: Integer -> [Integer]
primeFactors num = unfoldr f (testFactors num, num) where
    f (_, 1) = Nothing
    f (ps, n) = case find (\p -> (n `rem` p) == 0) ps of
                        Nothing -> Just (n, ([], 1)) -- prime
                        Just fact -> Just (fact, (dropWhile (< fact) ps, n `div` fact))


testFactors :: Integer -> [Integer]
testFactors n = takeWhile ((<=n) . square) primes

-- -- avoiding some num conversion ugliness with (^2)
square :: Integer -> Integer
square x = x * x
