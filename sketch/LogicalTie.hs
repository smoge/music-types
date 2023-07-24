import Data.Ratio
import Data.Bits 

-- Function to check if a number is a power of 2
isPowerOfTwo :: Int -> Bool
isPowerOfTwo n = popCount n == 1

-- Function to find the largest power of 2 below a number
largestPowerOfTwo :: Int -> Int
largestPowerOfTwo n = 2 ^ ((floor . logBase 2 . fromIntegral) n)

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
