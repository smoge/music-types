{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-identities #-}
module Barlow
  ( primeFactors,
    indispensability
  )
where

import           Data.List
-- import           Math.Primes


-- primeFactors :: Integer -> [Integer]
-- primeFactors num = unfoldr f (testFactors num, num) where
--     f (_, 1) = Nothing
--     f (ps, n) = case find (\p -> (n `rem` p) == 0) ps of
--                         Nothing -> Just (n, ([], 1)) -- prime
--                         Just fact -> Just (fact, (dropWhile (< fact) ps, n `div` fact))

primeFactors :: Integer -> [Integer]
primeFactors n = go n 2 []
  where
    go n i factors
      | i * i > n = if n > 1 then factors ++ [n] else factors
      | n `mod` i == 0 = go (n `div` i) i (factors ++ [i])
      | otherwise = go n (i + 1) factors

wFunc :: Integer -> Integer
wFunc 0 = 0
wFunc _ = 1

basicIndispensability :: Integer -> Integer -> Integer
basicIndispensability pulse prime
  | prime <= 3 = (prime + pulse - 2) `mod` prime
  | otherwise =
      (q + wFunc (q `div` (prime `div` 4))) * wFunc (prime - pulse - 1)
      + (prime `div` 4) * (1 - wFunc (prime - pulse - 1))
  where
    newPulse = pulse - 1 + wFunc (prime - pulse)
    factors = reverse $ primeFactors (prime - 1)
    q = indispensability newPulse factors

indispensability :: Integer -> [Integer] -> Integer
indispensability pulse primes =
  sum
    [ mult * basic
    | r <- [0 .. z - 1]
    , let bot = product $ take (r + 1) (drop (z + 1 - r) extendedPrimes)
    , let mult = product $ take (z - r) extendedPrimes
    , let modulo = extendedPrimes !! (z - r)
    , let temp = fromIntegral ((pulse - 2) `mod` top)
    , let temp' = 1 + floor (temp / fromIntegral bot)
    , let temp'' = 1 + fromIntegral (temp' `mod` modulo)
    , let basic = basicIndispensability temp'' modulo
    ]
  where
    extendedPrimes = [1] ++ primes ++ [1]
    z = length primes
    top = product $ take (z + 1) extendedPrimes

main :: IO ()
main = do
  print $ indispensability 1 [2, 3, 2, 2, 2]
  print $ indispensability 2 [2, 3, 2, 2, 2]
  print $ indispensability 3 [2, 3, 2, 2, 2]
  print $ indispensability 4 [2, 3, 2, 2, 2]
  print $ indispensability 5 [2, 3, 2, 2, 2]
  print $ indispensability 6 [2, 3, 2, 2, 2]
  print $ indispensability 0 [2, 3, 2, 2, 2]

-- 47
-- 0
-- 24
-- 12
-- 36
-- 6
-- 23
