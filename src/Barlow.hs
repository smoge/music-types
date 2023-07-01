module Barlow
  ( primeFactors,
    wFunc,
    basicIndispensability,
    indispensability,
    main,
  )
where

import Data.List

-- Function to calculate prime factors of a number
primeFactors :: Integer -> [Integer]
primeFactors n = go n 2 []
  where
    go n i factors
      | i * i > n = if n > 1 then factors ++ [n] else factors
      | n `mod` i == 0 = go (n `div` i) i (factors ++ [i])
      | otherwise = go n (i + 1) factors

-- Function to calculate wFunc
wFunc :: Integer -> Integer
wFunc 0 = 0
wFunc _ = 1

-- Function to calculate basic indispensability
basicIndispensability :: Integer -> Integer -> Integer
basicIndispensability pulse prime
  | prime <= 3 = mod (prime + pulse - 2) prime
  | otherwise =
    (q + wFunc (div q (div prime 4))) * wFunc (prime - pulse - 1)
      + div prime 4 * (1 - wFunc (prime - pulse - 1))
  where
    newPulse = pulse - 1 + wFunc (prime - pulse)
    factors = reverse $ primeFactors (prime - 1)
    q = indispensability newPulse factors

-- Function to calculate indispensability
indispensability :: Integer -> [Integer] -> Integer
indispensability pulse primes =
  sum
    [ mult * basic
      | r <- [0 .. z - 1],
        let bot = product $ take (r + 1) (drop (z + 1 - r) extendedPrimes),
        let mult = product $ take (z - r) extendedPrimes,
        let modulo = extendedPrimes !! (z - r),
        let temp = fromIntegral ((pulse - 2) `mod` top),
        let temp' = 1 + floor (temp / fromIntegral bot),
        let temp'' = fromIntegral (mod temp' modulo),
        let temp''' = 1 + temp'',
        let basic = basicIndispensability temp''' modulo
    ]
  where
    extendedPrimes = [1] ++ primes ++ [1]
    z = length extendedPrimes - 2
    top = product $ take (z + 1) extendedPrimes

main :: IO ()
main = do
  -- Test cases
  print $ indispensability 1 [2, 3, 2, 2, 2]
  print $ indispensability 2 [2, 3, 2, 2, 2]
  print $ indispensability 3 [2, 3, 2, 2, 2]
  print $ indispensability 4 [2, 3, 2, 2, 2]
  print $ indispensability 5 [2, 3, 2, 2, 2]
  print $ indispensability 6 [2, 3, 2, 2, 2]
  print $ indispensability 0 [2, 3, 2, 2, 2]