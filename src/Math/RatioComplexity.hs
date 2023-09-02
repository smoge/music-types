module Math.RatioComplexity (
    getIntegerPart,
    getFractionalPart,
    continuedFraction,
    reconstruct,
    calculateComplexity,
    ComplexityFactors(..),
    calculateComplexities,
) where

import Data.Ratio ( (%), denominator, numerator )
import Data.List ( group, sortBy )
import Data.Ord ( comparing )
import Data.Maybe ( fromMaybe, isJust, fromJust )


-- | Calculates the integer part of a rational number.
getIntegerPart :: Rational -> Integer
getIntegerPart = truncate -- floor

-- | Calculates the fractional part of a rational number.
getFractionalPart :: Rational -> Rational
getFractionalPart = snd . properFraction

-- | Calculates the continued fraction representation of a rational number.
continuedFraction :: Rational -> Integer -> Maybe [Integer]
continuedFraction _ 0 = Nothing
continuedFraction x maxDepth = helper x maxDepth 0
  where
    helper _ 0 _ = Just []
    helper x' maxDepth' depth
      | depth >= maxDepth' = Just []
      | r == 0             = Just [n]
      | otherwise          = (n :) <$> helper (1 / r) maxDepth' (depth + 1)
      where
        n = getIntegerPart x'
        r = getFractionalPart x'

-- | Reconstructs a rational number from its continued fraction representation.
reconstruct :: [Integer] -> Rational
reconstruct [] = 0 % 1
reconstruct xs = foldl (flip (\current val -> val + 1 / current)) (1 % last xs) (map (% 1) (init xs))

-- | Calculates the complexity of a list of integers.
calculateComplexity :: [Integer] -> Float
calculateComplexity [] = 0
calculateComplexity xs = numTermsFactor + sizeFactor + maxTermFactor
  where
    numTerms = length xs
    sizes = map ((\n -> numerator n + denominator n) . (% 1)) xs
    totalSize = sum sizes
    maxTerm = maximum sizes
    numTermsFactor = fromIntegral numTerms
    sizeFactor = fromIntegral totalSize / fromIntegral numTerms
    maxTermFactor = fromIntegral maxTerm / fromIntegral numTerms



-- Define a type to represent the complexity factors
data ComplexityFactors = ComplexityFactors
    { numTerms :: Int
    , averageCoefficientSize :: Double
    , repeatingPattern :: Bool
    } deriving (Show, Eq)


-- | An instance of Ord for ComplexityFactors
instance Ord ComplexityFactors where
    compare = comparing numTerms <> comparing averageCoefficientSize <> comparing repeatingPattern


-- | Sorts a list of rationals by complexity.
compareByComplexity :: Rational -> Rational -> Ordering
compareByComplexity ratio1 ratio2 =
    compare (calculateComplexityFactors ratio1) (calculateComplexityFactors ratio2)



-- Function to calculate the complexity factors for a list of rationals
calculateComplexities :: [Rational] -> [ComplexityFactors]
calculateComplexities = map calculateComplexityFactors


-- | Sorts a list of rationals by complexity.
compareByComplexity' :: Rational -> Rational -> Ordering
compareByComplexity' ratio1 ratio2 =
    compare (calculateComplexityFactors ratio1) (calculateComplexityFactors ratio2)



{- 
>>> calculateComplexityFactors (355 % 113)
ComplexityFactors {numTerms = 3, averageCoefficientSize = 8.666666666666666, repeatingPattern = False}

>>> calculateComplexityFactors (35 % 13) 
ComplexityFactors {numTerms = 4, averageCoefficientSize = 2.25, repeatingPattern = False}

-}

-- -- Function to sort a list of rationals by complexity
-- sortRationalsByComplexity' :: [Rational] -> [Rational]
-- sortRationalsByComplexity'  = sortedRatios 

-- | Sorts a list of rationals by complexity
sortRationalsByComplexity :: [Rational] -> [Rational]
sortRationalsByComplexity  = sortBy compareByComplexity 

{-
>>> example3 =  [ num %  den | den <- [2..11], num <- [1..11], gcd num den == 1]   :: [Rational]

>>>  sortRationalsByComplexity example3
[1 % 2,3 % 2,1 % 3,4 % 3,1 % 4,5 % 2,7 % 2,7 % 3,5 % 4,1 % 5,9 % 2,9 % 4,6 % 5,1 % 6,10 % 3,11 % 2,11 % 5,7 % 6,1 % 7,8 % 7,1 % 8,9 % 8,1 % 9,10 % 9,1 % 10,11 % 10,1 % 11,2 % 3,3 % 4,5 % 3,2 % 5,8 % 3,4 % 5,2 % 7,3 % 7,7 % 4,7 % 5,11 % 3,11 % 4,5 % 6,9 % 7,10 % 7,2 % 9,4 % 9,9 % 5,3 % 10,6 % 7,11 % 9,2 % 11,5 % 11,11 % 6,7 % 8,8 % 9,9 % 10,10 % 11,3 % 5,3 % 8,8 % 5,4 % 7,5 % 7,11 % 8,7 % 9,7 % 10,3 % 11,4 % 11,11 % 7,5 % 9,9 % 11,6 % 11,5 % 8,8 % 11,7 % 11]

-- Calculate complexities for the example rationals
>>> exampleComplexities = calculateComplexities example3  :: [ComplexityFactors]
 -}



{- 
>>> calculateComplexityFactors (355 % 113)
ComplexityFactors {numTerms = 3, averageCoefficientSize = 8.666666666666666, repeatingPattern = False}

>>> calculateComplexityFactors (35 % 13)
ComplexityFactors {numTerms = 4, averageCoefficientSize = 2.25, repeatingPattern = False}
 -}

 -- | Calculates the complexity factors for a rational number
calculateComplexityFactors :: Rational -> ComplexityFactors
calculateComplexityFactors ratio =
    case continuedFraction ratio 10 of
        Just cf -> let numTerms = length cf
                       totalSize = sum cf
                   in ComplexityFactors
                        { numTerms = numTerms
                        , averageCoefficientSize = fromIntegral totalSize / fromIntegral numTerms
                        , repeatingPattern = hasRepeatingPattern cf
                        }
        Nothing -> ComplexityFactors
            { numTerms = 0
            , averageCoefficientSize = 0
            , repeatingPattern = False
            }




-- hasRepeatingPattern :: [Integer] -> Bool
-- hasRepeatingPattern cf = any (\(_, groupSize) -> groupSize > 1) consecutiveGroups
--   where
--     consecutiveGroups = map (\group -> (head group, length group)) (group cf)

-- Check if a continued fraction has a repeating pattern
hasRepeatingPattern :: [Integer] -> Bool
hasRepeatingPattern cf = any (\(_, groupSize) -> groupSize > 1) consecutiveGroups
  where
    consecutiveGroups = map (\group -> (head group, length group)) (group cf)



-- | Sorts a list of rationals by complexity
sortedRatios :: [Rational] -> [Rational]
sortedRatios = sortBy compareByComplexity 


{- 
>>> sortedRatios [3 % 2, 13 % 17, 5 % 7, 23 % 29, 17%13]
[3 % 2,17 % 13,5 % 7,13 % 17,23 % 29]


 -}

{- 
>>> rationals = [3%2, 13 % 17, 5 % 7, 21 % 29, 17%13]
>>> complexities = map (calculateComplexity . fromJust . (`continuedFraction` 20)) rationals
>>> complexities
[6.0,8.25,7.0,9.571428,8.333334]
-}


-- | Example demonstrating the calculation of complexity factors.
example1 :: ComplexityFactors
example1 = calculateComplexityFactors (355 % 113)

-- | Example demonstrating sorting rationals by complexity.
example2 :: [Rational]
example2 = sortRationalsByComplexity [3 % 2, 13 % 17, 5 % 7, 23 % 29, 17 % 13]

-- | Example demonstrating the calculation of complexities for a list of rationals.
example3 :: [ComplexityFactors]
example3 = calculateComplexities [3 % 2, 13 % 17, 5 % 7, 23 % 29, 17 % 13]


