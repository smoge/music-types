import Data.Ratio
import Data.Bits 

isPowerOfTwo :: Int -> Bool
isPowerOfTwo n = popCount n == 1

largestPowerOfTwo :: Int -> Int
largestPowerOfTwo n = 2 ^ (floor . logBase 2 . fromIntegral) n

toLogicalTies :: Ratio Int -> [Ratio Int]
toLogicalTies x 
    | isPowerOfTwo (numerator x) = [x]
    | otherwise = 
        let 
            largestPower = largestPowerOfTwo (numerator x)
            firstDuration = largestPower % (2 * denominator x)
            secondDuration = x - firstDuration
        in 
            [firstDuration, secondDuration]

{- 
ghci> toLogicalTies (5%16)
[1 % 8,3 % 16]
ghci> toLogicalTies (11%16)
[1 % 4,7 % 16]
 -}