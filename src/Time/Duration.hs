{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-} 
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-orphans #-}


module Time.Duration
  ( Duration (..),
    Rq,
    Dots,
    Division,
    HasDuration (..),
    dotMultiplier,
    durationToRq,
    rqToDuration,
  )
where

import Control.Lens ( (^.), view, makeLenses )
import Data.Ord ( comparing )
import qualified Data.Set as Set
import Data.List ( minimumBy, sortOn, nub ) 
import Data.Bits ( Bits((.&.)) )
import Data.Maybe ( listToMaybe ) 
import Math.Primes ( primeFactors )
import Data.Ratio ( (%), denominator, numerator )
import Test.QuickCheck
    ( Arbitrary(arbitrary),
      Property,
      elements,
      quickCheck,
      stdArgs,
      verboseCheckWith,
      Testable(property),
      Args(maxSuccess) )


type Division = Integer

type Dots = Int

type Rq = Rational

data Duration = Duration
  { _division :: Division,
    _dots :: Dots,
    _multiplier :: Rq
  }
  deriving (Eq, Show)

makeLenses ''Duration

class HasDuration a where
  _duration :: a -> Duration
  _rq :: a -> Rq
  _rq a = durationToRq (_duration a)
  _duration _ = Duration 1 0 1 -- default implementation


dotMultiplier :: Dots -> Rq
dotMultiplier d = 1 + (2 ^ d - 1) / 2 ^ d

generateDivisions :: Int -> [Division]
generateDivisions n = map (2^) [0..n]

isPowerOfTwo :: Int -> Bool
isPowerOfTwo n = n > 0 && (n Data.Bits..&. (n - 1)) == 0

-- | Generates a list of rational numbers based limits
generateRationalNumbers :: Int -> [Rq]
generateRationalNumbers n =
    Set.toList $ Set.fromList
        [fromIntegral num % fromIntegral den |
         den <- [2..n],
         num <- [1..den], 
         gcd num den == 1,
         not (isPowerOfTwo num && num /= 2),
         not (num == 1 && isPowerOfTwo den)]


-- ratioComplexity :: Rq -> Int
-- ratioComplexity r = 
--     length (primeFactors (numerator r)) + (2 * length (primeFactors (denominator r))) 


orderDurationBySimplicity :: [Duration] -> [Duration]
orderDurationBySimplicity =
    sortOn musicalSimplicity
  where
    musicalSimplicity :: Duration -> (Int, Int, Int, Int)
    musicalSimplicity d = (fromInteger (denominator (durationToRq d)),  fromInteger (numerator (durationToRq d)), ratioComplexity (d ^. multiplier),d ^. dots)



ratioComplexity :: Rq -> Int
ratioComplexity r = 
    length (primeFactors (numerator r)) + length (primeFactors (denominator r))




divisions :: [Division]
divisions = generateDivisions 12

dotsList :: [Dots]
dotsList = [0 .. 4]

rationalNumbers :: [Rq]
rationalNumbers = 1 : orderByMusicalSimplicity (generateRationalNumbers 13)

reciprocalRational :: Rational -> Rational
reciprocalRational r = denominator r % numerator r

rationalNumbersRec :: [Rq]
rationalNumbersRec = orderByMusicalSimplicity $ 1 : map reciprocalRational (generateRationalNumbers 13)

-- combinedRationals :: [Rational]
-- combinedRationals =  orderByMusicalSimplicity $ nub (generateRationalNumbersCombined 13)

generateRationalNumbersCombined :: Int -> [Rq]
generateRationalNumbersCombined n =
    orderByMusicalSimplicity $ 1 : (generateRationalNumbers n ++ map reciprocalRational (generateRationalNumbers n))

-- combinedRationals :: [Rational]
-- combinedRationals =  orderByMusicalSimplicity $ nub (generateRationalNumbersCombined 13)

-- generateRationalNumbersCombined :: Int -> [Rq]
-- generateRationalNumbersCombined n =
--     orderByMusicalSimplicity $ 1 : (generateRationalNumbers n ++ map reciprocalRational (generateRationalNumbers n))


combinedRationals :: [Rational]
combinedRationals =  orderByMusicalSimplicity $ rationalNumbers ++ rationalNumbersRec

divisions' :: [Division]
divisions' = generateDivisions 5

dotsList' :: [Dots]
dotsList' = [0 .. 2]

rationalNumbers' :: [Rq]
rationalNumbers' = 1 : orderByMusicalSimplicity (generateRationalNumbers 5)


orderByMusicalSimplicity' :: [Rq] -> [Rq]
orderByMusicalSimplicity' = orderByMusicalSimplicity



kolmogorovComplexityRational :: Rational -> Int
kolmogorovComplexityRational r = length (primeFactors (numerator r)) + length (primeFactors (denominator r))


orderByMusicalSimplicity :: [Rq] -> [Rq]
orderByMusicalSimplicity = sortOn musicalOrderHelper

musicalOrderHelper :: Rq -> (Int, Int, Int, Int, Int, Int)
musicalOrderHelper r =
  (     
    complexity,
    denom,
    numerLessThanDenom,
    absDiff,
    numer,
    powerOfTwo
    )
  where
    numer = negate $ fromIntegral (numerator r)
    denom = fromIntegral (denominator r)
    complexity = kolmogorovComplexityRational r
    absDiff = numer - denom
    numerLessThanDenom = if absDiff < 0 then 1 else 0
    powerOfTwo = if isPowerOfTwo denom then 1 else 0


orderDurationByComplexity :: [Duration] -> [Duration]
orderDurationByComplexity =
    sortOn (ratioComplexity . _multiplier)

multipliersSet :: Set.Set Rq
multipliersSet = Set.fromDistinctAscList rationalNumbers

-- multipliersSet :: Set.Set Rq
-- multipliersSet = Set.fromList rationalNumbers

compareDurations :: Duration -> Duration -> Ordering
compareDurations = Data.Ord.comparing durationToRq

-- | Compares two durations by their complexity
compareDurationsSimplicity :: Duration -> Duration -> Ordering
compareDurationsSimplicity d1 d2 =
    compare (ratioComplexity (d1 ^. multiplier)) (ratioComplexity (d2 ^. multiplier))

addDurations :: Duration -> Duration -> [Duration]
addDurations d1 d2 =
    let rqSum = durationToRq d1 + durationToRq d2
    in rqToDuration rqSum

scaleDuration :: RealFrac a => a -> Duration -> Maybe Duration
scaleDuration factor d =
    let scalar = abs $ realToFrac factor
        minClamp = 1/10 :: Rq
        maxClamp = 19/20 :: Rq
        rqScaled = scalar * durationToRq d
        clampedRq =  max minClamp (min maxClamp rqScaled) -- Clamp the result to the valid range [0, 1]
        x =  orderDurationBySimplicity $ rqToDuration clampedRq
    in case x of
        (firstDuration:_) -> Just firstDuration
        _ -> Nothing
        
scaleDuration' :: RealFrac a => a -> Duration -> Maybe [Duration]
scaleDuration' factor d =
    let scalar = abs $ realToFrac factor
        rqScaled = scalar * durationToRq d
        clampedRq =  max (1/10 :: Rq) (min (19/20 :: Rq) rqScaled) -- Clamp the result to the valid range [0, 1]
        x =  orderDurationBySimplicity $ rqToDuration clampedRq
    in case x of
        [] -> Nothing
        _ -> Just x

scaleDuration'' :: RealFrac a => a -> Duration -> Maybe [Duration]
scaleDuration'' factor d =
    let scalar = abs $ realToFrac factor
        rqScaled = scalar * durationToRq d
        clampedRq =  max (1/10 :: Rq) (min (19/20 :: Rq) rqScaled) -- Clamp the result to the valid range [0, 1]
        x =  orderDurationBySimplicity $ rqToDuration clampedRq
    in case x of
        [] -> Nothing
        (firstDuration:_) -> Just [firstDuration]

multiplyDuration :: Duration -> Scalar -> Maybe Duration
multiplyDuration d scalar =
    let scalar' = abs scalar
        rqScaled = scalar' * durationToRq d
        potentialResults = rqToDuration rqScaled
        sameMultiplierResults = filter (\result -> _multiplier result == _multiplier d) potentialResults
    in case sameMultiplierResults of
        (firstWithSameMultiplier:_) -> Just firstWithSameMultiplier
        _ -> case potentialResults of
                (bestResult:_) -> Just bestResult
                _ -> Nothing

multiplyDuration' :: Duration -> Scalar -> Maybe [Duration]
multiplyDuration' d scalar =
    let scalar' = abs scalar
        rqScaled = scalar' * durationToRq d
        potentialResults = rqToDuration rqScaled
        ordered = orderDurationBySimplicity potentialResults
    in case ordered of
        [] -> Nothing
        _ -> Just ordered

           

type Scalar = Rq

instance Ord Duration where
  compare :: Duration -> Duration -> Ordering
  compare = compareDurations'


instance Num Duration where
    (+) :: Duration -> Duration -> Duration
    d1 + d2 = case addDurationsWithSameMultiplier d1 d2 of
                Just d  -> d
                Nothing -> error "Failed to add durations with differing multipliers."

    (*) :: Duration -> Duration -> Duration
    d1 * d2 = case d1 .* durationToRq d2 of
                Just d  -> d
                Nothing -> error "Failed to multiply durations"


    -- Absolute value can be itself, assuming durations can't be negative.
    abs :: Duration -> Duration
    abs d = d

    -- Signum will return a duration with value 1 for positive durations, and throw an error otherwise.
    signum :: Duration -> Duration
    signum d = if d > 0 then d else error "Negative or zero duration not supported."

    -- Converting an Integer to a Duration is not straightforward, so we'll throw an error.
    fromInteger :: Integer -> Duration
    fromInteger _ = error "Conversion from Integer to Duration not defined."

    -- Providing definition for subtraction; if this isn't correct, it can be modified accordingly.
    (-) :: Duration -> Duration -> Duration
    d1 - d2 = case subtractDurationsWithSameMultiplier d1 d2 of
                Just d  -> d
                Nothing -> error "Failed to subtract durations with differing multipliers."

    -- Negate does not semantically make sense for duration. So, throw an error.
    negate :: Duration -> Duration
    negate _ = error "Negation not defined for Duration."


class Scalable a b where
    (.*) :: a -> b -> Maybe a

instance Scalable Duration Scalar where
    (.*) :: Duration -> Scalar -> Maybe Duration
    d .* s = multiplyDuration d s


subtractDurationsWithSameMultiplier :: Duration -> Duration -> Maybe Duration
subtractDurationsWithSameMultiplier d1 d2
  | d1 ^. multiplier /= d2 ^. multiplier = Nothing
  | otherwise =
    let rqDifference = durationToRq d1 - durationToRq d2
    in if rqDifference < 0
       then Nothing -- This ensures durations remain non-negative. Adjust as necessary.
       else
           let possibleDurations = rqToDuration rqDifference
               matchedDuration = filter ((== (d1 ^. multiplier)) . (^. multiplier)) possibleDurations
           in case matchedDuration of
               (d:_) -> Just d
               []    -> Nothing

-- durationToRq :: Duration -> Rq
-- durationToRq (Duration div_ dots_ mult) =
--   1 %  div_ * dotMultiplier dots_ * mult

-- Simplified definition using RecordWildCards
durationToRq :: Duration -> Rq
durationToRq Duration{..} = (1 % _division) * dotMultiplier _dots * _multiplier


isMultiplierIdentity :: Duration -> Bool
isMultiplierIdentity = (1 ==) . view multiplier

allMultipliersIdentity :: [Duration] -> Bool
allMultipliersIdentity = all isMultiplierIdentity

areMultiplierEqual :: Duration -> Duration -> Bool
areMultiplierEqual d1 d2 = d1 ^. multiplier == d2 ^. multiplier


allMultipliersEqual :: [Duration] -> Bool
allMultipliersEqual [] = True
allMultipliersEqual (d:ds) = all (areMultiplierEqual d) ds



       
-- rqToDuration :: Rq -> [Duration]
-- rqToDuration rq =
--       let multipliers =  rationalNumbers
--           validDurations = [Duration d dt m | d <- divisions, dt <- dotsList, m <- multipliers]
--           matchingDurations = filter (\d -> durationToRq d == rq) validDurations
--        in matchingDurations

-- rqToDuration :: Rq -> [Duration]
-- rqToDuration rq
--   | rq <= 0 = []
--   | otherwise =
--       let multipliers =  rationalNumbers
--           validDurations = [Duration d dt m | d <- divisions, dt <- dotsList, m <- multipliers]
--           matchingDurations = filter (\d -> durationToRq d == rq) validDurations
--        in matchingDurations

-- rqToDuration :: Rq -> [Duration]
-- rqToDuration rq = case rq of
--   rq' | rq' <= 0 -> []
--       | otherwise -> [Duration d dt m | d <- divisions, dt <- dotsList, m <- rationalNumbers, durationToRq (Duration d dt m) == rq']

-- rqToDuration :: Rq -> [Duration]
-- rqToDuration rq
--   | rq <= 0 = []
--   | otherwise = filter (\d -> durationToRq d == rq) validDurations
--   where
--     validDurations = [Duration d dt m | d <- divisions, dt <- dotsList, m <- rationalNumbers]


-- Cleaned up the rqToDuration function
rqToDuration :: Rq -> [Duration]
rqToDuration rq
  | rq <= 0 = []
  | otherwise = filter (\d -> durationToRq d == rq) validDurations
  where
    validDurations = [Duration d dt m | d <- divisions, dt <- dotsList, m <- rationalNumbers]


-- addDurationsWithSameMultiplier :: Duration -> Duration -> Maybe Duration
-- addDurationsWithSameMultiplier d1 d2
--   | d1 ^. multiplier /= d2 ^. multiplier = Nothing
--   | otherwise =
--     let rqSum = durationToRq d1 + durationToRq d2
--         possibleDurations = rqToDuration rqSum
--         matchedDuration = filter ((== (d1 ^. multiplier)) . (^. multiplier)) possibleDurations
--     in case matchedDuration of
--         (d:_) -> Just d
--         []    -> Nothing


-- Streamlined the addDurationsWithSameMultiplier function
addDurationsWithSameMultiplier :: Duration -> Duration -> Maybe Duration
addDurationsWithSameMultiplier d1 d2
  | d1 ^. multiplier /= d2 ^. multiplier = Nothing
  | otherwise = 
      let rqSum = durationToRq d1 + durationToRq d2
          matchedDuration = filter ((== (d1 ^. multiplier)) . (^. multiplier)) (rqToDuration rqSum)
      in listToMaybe matchedDuration

bestDuration :: [Duration] -> Duration
bestDuration = minimumBy compareDurations

compareDurations' :: Duration -> Duration -> Ordering
compareDurations' d1 d2 =
    comparing _division d1 d2
    <> comparing _dots d1 d2
    <> comparing (abs . (1 -) . _multiplier) d1 d2

filterSameMultiplier :: Duration -> Duration -> [Duration] -> [Duration]
filterSameMultiplier d1 d2 = filter (\d -> _multiplier d == _multiplier d1 || _multiplier d == _multiplier d2)




-- !QuickCheck

instance Test.QuickCheck.Arbitrary Duration where
  arbitrary = do
    div_ <- Test.QuickCheck.elements divisions'
    dt <-  Test.QuickCheck.elements  dotsList'
    mult <-  Test.QuickCheck.elements  rationalNumbers'
    return Duration {_division = div_, _dots = dt, _multiplier = mult}


specificRationals :: [Rq]
specificRationals = [2%3, 3%2, 5%4, 4%5]

prop_rqToDuration :: Duration -> Bool
prop_rqToDuration d =
  let rq = durationToRq d
      durations = rqToDuration rq
   in d `elem` durations


propRqToDuration' :: Rq -> Property
propRqToDuration' rq =
    let durations = rqToDuration (abs rq)
    in property $ all (\d -> durationToRq d == abs rq) durations


reverseScalar :: Rq -> Rq
reverseScalar r = denominator r % numerator r



main :: IO ()
main = do
  x <- Test.QuickCheck.quickCheck prop_rqToDuration
  print x
  y <- Test.QuickCheck.quickCheck propRqToDuration' 
  print y



runTests :: IO ()
runTests = Test.QuickCheck.verboseCheckWith (Test.QuickCheck.stdArgs {maxSuccess = 100}) prop_rqToDuration

runTests' :: IO ()
runTests' = Test.QuickCheck.verboseCheckWith (Test.QuickCheck.stdArgs {maxSuccess = 100}) propRqToDuration' 


--

allDurations :: [Duration]
allDurations =
  [ Duration d dt m
    | d <- [1, 2, 4, 8, 16, 32, 64, 128, 256, 512],
      dt <- dotsList,
      m <- rationalNumbers
  ]

allDurations' :: Set.Set Duration
allDurations' =
  Set.fromList
    [ Duration d dt m
      | d <- [1, 2, 4, 8, 16, 32, 64, 128, 256, 512],
        dt <- dotsList,
        m <- rationalNumbers
    ]

allRqs :: [Rq]
allRqs = map durationToRq (Set.toList allDurations')

roundTo4 :: RealFrac a => a -> Int
roundTo4 x = round (x * 10000)


-- | All possible RQs as Integers 0 - 100
allRqsAsInt :: [(Int, Rq)]
allRqsAsInt = map (\x -> (roundTo4 x, x)) allRqs

-- | All possible RQs as Integers 0 - 100
-- allRqsComplexity :: [Int]
-- allRqsComplexity = map ratioComplexity allRqs

-- Order allRqs byt ratioComplexity\
orderRqsByComplexity :: [Rq] -> [Rq]
orderRqsByComplexity = sortOn ratioComplexity
-- orderRqsByComplexity :: [Rq] -> [Rq]
-- orderRqsByComplexity = sortOn ratioComplexity 


rqByComplexity :: [Rq]
rqByComplexity = orderRqsByComplexity allRqs

allRqsAsPair :: [(Int, Rq)]
allRqsAsPair = map (\x -> (ratioComplexity x, x)) rqByComplexity

rqByComplexityasInt :: [Int]
rqByComplexityasInt =  map (\x -> round ((realToFrac x) * 10000)) y
    where
        y = orderRqsByComplexity allRqs


durationsForRq :: Rq -> [Duration]
durationsForRq rq = filter (\d -> durationToRq d == rq) allDurations

countDurationsForRq :: Rq -> Int
countDurationsForRq rq = length $ durationsForRq rq

-- countDurationsForRq :: Rq -> Int
-- countDurationsForRq rq = foldl (\acc d -> if durationToRq d == rq then acc + 1 else acc) 0 allDurations

