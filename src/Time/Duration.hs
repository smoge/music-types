
-- | Module defines a `Duration` data type and provides functions for converting Durations to Rq and vice versa.
--
-- The `Duration` data type represents a duration, consisting of three fields: `_division`, `_dots`, and `_multiplier`.
--
-- The module includes the following functions:
--   - `durationToRq`: Converts a duration to a rational number.
--   - `rqToDuration`: Converts a rational number to a list of durations.
--
-- Example usage:
--   >>> duration = Duration 8 1 (4 % 5)
--   >>> durationToRq duration
--   -- Output: 3 % 20
--
--   >>> rq = 7 % 12
--   >>> rqToDuration rq
--   -- Output: [Duration {_division = 2, _dots = 1, _multiplier = 7 % 9},Duration {_division = 2, _dots = 2, _multiplier = 2 % 3}]
--
-- The module also includes QuickCheck tests and helper functions for generating and manipulating durations and rational numbers.

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BangPatterns #-}

module Time.Duration
  ( Duration (..),
    Rq,
    durationToRq,
    rqToDuration,
  )
where

import Control.Lens (makeLenses)
import Data.Ord (comparing)
import Data.Ratio ((%))
import qualified Data.Set as Set
import Test.QuickCheck

type Division = Int

type Dots = Int

type Rq = Rational

data Duration = Duration
  { _division :: !Division,
    _dots :: !Dots,
    _multiplier :: !Rational
  }
  deriving (Eq, Show)

makeLenses ''Duration

class HasDuration a where
  _duration :: a -> Duration
  _rq :: a -> Rq
  _rq a = durationToRq (_duration a)
  _duration _ = Duration 1 0 1 -- default implementation
 
dotMultiplier :: Dots -> Rational
dotMultiplier d = dotArray !! d
  where
    dotArray = [1, 3 / 2, 7 / 4, 15 / 8, 31 / 16, 63 / 32, 127 / 64]




generateDivisions :: Int -> [Division]
generateDivisions n = [2^x | x <- [0..n]]

divisions :: [Division]
divisions = generateDivisions 9



rationalNumbers :: [Rational]
rationalNumbers = [2 % 3, 3 % 4, 3 % 5, 4 % 5, 4 % 7, 4 % 9, 5 % 6, 5 % 7, 5 % 8, 5 % 9, 6 % 7, 6 % 11, 7 % 8, 7 % 9, 7 % 10, 7 % 11, 8 % 9, 8 % 11, 9 % 10, 9 % 11, 10 % 11]

dotsSet :: Set.Set Dots
dotsSet = Set.fromList dotsList

multipliersSet :: Set.Set Rq
multipliersSet = Set.fromList rationalNumbers

-- rationalNumbers :: [Rational]
-- rationalNumbers = [ x % y | x <- [2..10], y <- [3..11], x < y ]

dotsList :: [Dots]
dotsList = [0 .. 6]

compareDurations :: Duration -> Duration -> Ordering
compareDurations = Data.Ord.comparing durationToRq

instance Ord Duration where
  compare :: Duration -> Duration -> Ordering
  compare = compareDurations



durationToRq :: Duration -> Rational
durationToRq (Duration div dots mult) =
  (1 % fromIntegral div) * dotMultiplier dots * mult

rqToDuration :: Rq -> [Duration]
rqToDuration rq
  | rq <= 0 = []
  | otherwise = foldMap potentialDurations divisions
  where
    potentialDurations :: Division -> [Duration]
    potentialDurations d = foldMap (potentialDurationsForDot d) dotsList

    potentialDurationsForDot :: Division -> Dots -> [Duration]
    potentialDurationsForDot d dt =
      let !reciprocalDiv = 1 % fromIntegral d
          !dotMult = dotMultiplier dt
          !divisor = reciprocalDiv * dotMult
          potentialMultiplier = rq / divisor
       in [Duration d dt potentialMultiplier | Set.member potentialMultiplier multipliersSet]


-- rqToDuration :: Rq -> [Duration]
-- rqToDuration rq
--   | rq <= 0 = []
--   | otherwise = concatMap potentialDurations divisions
--   where
--     multipliersSet = Set.fromList rationalNumbers

--     potentialDurations :: Division -> [Duration]
--     potentialDurations d = concatMap (potentialDurationsForDot d) dotsList

--     potentialDurationsForDot :: Division -> Dots -> [Duration]
--     potentialDurationsForDot d dt =
--       let potentialMultiplier = rq / ((1 % d) * dotMultiplier dt)
--        in [Duration d dt potentialMultiplier | Set.member potentialMultiplier multipliersSet]



-- !QuickCheck

instance Arbitrary Duration where
  arbitrary = do
    div <- elements divisions
    dt <- choose (0, 6)
    mult <- elements rationalNumbers
    return Duration {_division = div, _dots = dt, _multiplier = mult}

prop_rqToDuration :: Duration -> Bool
prop_rqToDuration d =
  let rq = durationToRq d
      durations = rqToDuration rq
   in d `elem` durations

runTests :: IO ()
runTests = verboseCheckWith (stdArgs {maxSuccess = 10000}) prop_rqToDuration


--

allDurations :: [Duration]
allDurations =
  [ Duration d dt m
    | d <- [1, 2, 4, 8, 16, 32, 64, 128, 256, 512],
      dt <- dotsList,
      m <- rationalNumbers
  ]

allRqs :: [Rq]
allRqs = map durationToRq allDurations

durationsForRq :: Rq -> [Duration]
durationsForRq rq = filter (\d -> durationToRq d == rq) allDurations

countDurationsForRq :: Rq -> Int
countDurationsForRq rq = length $ durationsForRq rq
