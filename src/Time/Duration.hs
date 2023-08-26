{-# LANGUAGE TemplateHaskell #-}

module Time.Duration
  ( Duration (..),
    Rq,
    Division,
    Dots,
    durationToRq,
    rqToDuration,
  )
where

import Control.Lens (makeLenses, view)
import Data.Ord (comparing)
import Data.Ratio ((%))
import qualified Data.Set as Set
import Test.QuickCheck

type Division = Integer

type Dots = Int

type Rq = Rational

data Duration = Duration
  { _division :: Division,
    _dots :: Dots,
    _multiplier :: Rational
  }
  deriving (Eq, Show)

makeLenses ''Duration

class HasDuration a where
  _duration :: a -> Duration
  _rq :: a -> Rq
  _rq a = durationToRq (_duration a)

dotMultiplier :: Dots -> Rational
dotMultiplier 0 = 1
dotMultiplier 1 = 3 / 2
dotMultiplier 2 = 7 / 4
dotMultiplier 3 = 15 / 8
dotMultiplier 4 = 31 / 16
dotMultiplier 5 = 63 / 32
dotMultiplier 6 = 127 / 64
dotMultiplier _ = error "Invalid number of dots"

generateDivisions :: Int -> [Int]
generateDivisions n = [2^x | x <- [0..n]]

divisions :: [Division]
divisions = map fromIntegral (generateDivisions 9)

durationToRq :: Duration -> Rational
durationToRq (Duration div dots mult) =
  (1 % div) * dotMultiplier dots * mult

rationalNumbers :: [Rational]
rationalNumbers =
  [ 2 % 3,
    3 % 4,
    3 % 5,
    4 % 5,
    4 % 7,
    4 % 9,
    5 % 6,
    5 % 7,
    5 % 8,
    5 % 9,
    6 % 7,
    6 % 11,
    7 % 8,
    7 % 9,
    7 % 10,
    7 % 11,
    8 % 9,
    8 % 11,
    9 % 10,
    9 % 11,
    10 % 11
  ]

dotsList :: [Dots]
dotsList = [0 .. 6]

compareDurations :: Duration -> Duration -> Ordering
compareDurations = comparing durationToRq

instance Ord Duration where
  compare = compareDurations

isMultiplierIdentity :: Duration -> Bool
isMultiplierIdentity = (1 ==) . view multiplier

rqToDuration :: Rq -> [Duration]
rqToDuration rq
  | rq <= 0 = []
  | otherwise = concatMap potentialDurations divisions
  where
    multipliersSet = Set.fromList rationalNumbers

    potentialDurations :: Division -> [Duration]
    potentialDurations d = concatMap (potentialDurationsForDot d) dotsList

    potentialDurationsForDot :: Division -> Dots -> [Duration]
    potentialDurationsForDot d dt =
      let potentialMultiplier = rq / ((1 % d) * dotMultiplier dt)
       in [Duration d dt potentialMultiplier | Set.member potentialMultiplier multipliersSet]

-- QuickCheck

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

