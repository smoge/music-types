module Binary
  ( decToBin,
    isAssignableInteger,
    fromDotCount,
  )
where

import Data.List (isInfixOf)
import Data.Ratio

fromDotCount :: Int -> Rational
fromDotCount dotCount = n % d
  where
    n = 2 ^ (dotCount + 1) - 1
    d = 2 ^ dotCount

isAssignableInteger :: Integer -> Bool
isAssignableInteger x = x > 0 && not ("01" `isInfixOf` decToBin x)

impliedProlation :: Integer -> Rational
impliedProlation d =
  let n = greatestPowerOfTwoLessEqual d
   in n % d

greatestPowerOfTwoLessEqual :: Integer -> Integer
greatestPowerOfTwoLessEqual denominator = go 1
  where
    go n
      | n * 2 > denominator = n
      | otherwise = go (n * 2)

printImpliedProlation :: IO ()
printImpliedProlation = mapM_ putStrLn $ zipWith format [1 .. 16] impliedProlations
  where
    format n ip = showRational n ++ "\t" ++ showRational ip
    showRational r = if denominator r == 1 then show (numerator r) else show (numerator r) ++ "/" ++ show (denominator r)
    impliedProlations = [impliedProlation d | d <- [1 .. 16]]

{-
> printImpliedProlation
    1       1
    2       1
    3       2/3
    4       1
    5       4/5
    6       2/3
    7       4/7
    8       1
    9       8/9
    10      4/5
    11      8/11
    12      2/3
    13      8/13
    14      4/7
    15      8/15
    16      1
-}

prolationString :: Rational -> String
prolationString duration = show (denominator duration) ++ ":" ++ show (numerator duration)

prolationString' :: Integer -> Integer -> String
prolationString' numerator denominator = show denominator ++ ":" ++ show numerator

withDenominator :: Rational -> Integer -> (Integer, Integer)
withDenominator dur d =
  let n = numerator dur
      currentDenominator = denominator dur
      multiplier = d % currentDenominator
      newN = numerator (multiplier * fromIntegral n)
   in (newN, d)

{-
> withDenominator (1%2) 4
(2,4)
-}

decToBin :: Integer -> String
decToBin n
  | n < 0 = error "Input must be non-negative"
  | n == 0 = "0"
  | otherwise = reverse $ decToBin' n

decToBin' :: Integer -> String
decToBin' n
  | n == 0 = ""
  | otherwise = show (n `mod` 2) ++ decToBin' (n `div` 2)

main :: IO ()
main = mapM_ printAssignability [0 .. 16]
  where
    printAssignability n = putStrLn $ show n ++ "\t" ++ show (isAssignableInteger n)

{-
0 False
1 True
2 True
3 True
4 True
5 False
6 True
7 True
8 True
9 False
10 False
11 False
12 True
13 False
14 True
15 True
16 True
-}