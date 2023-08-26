{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Time.Duration where

import Control.Lens
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Ratio ((%))
import Test.QuickCheck

type Division = Integer
type Dots = Int
type Rq = Rational

data Duration = Duration
  { _division :: Division
  , _dots :: Dots
  , _multiplier :: Rational
  }
  deriving (Eq, Show)

makeLenses ''Duration

class HasDuration a where
  _duration :: a -> Duration
  _rq :: a -> Rq
  _rq a = durationToRq (_duration a)

dotMultiplier :: Dots -> Rational
dotMultiplier dotCount = n % d
  where
    n = 2 ^ (dotCount + 1) - 1
    d = 2 ^ dotCount

durationToRq :: Duration -> Rq
durationToRq d = (1 % (d ^. division)) * dotMultiplier (d ^. dots) * (d ^. multiplier)

durationToRq' :: Duration -> Rational
durationToRq' d = a * b * c
  where
    a = 1 % (view division d) :: Rational 
    b = dotMultiplier (view dots d) :: Rational
    c = view multiplier d :: Rational

      

compareDurations :: Duration -> Duration -> Ordering
compareDurations = comparing durationToRq

instance Ord Duration where
  compare = compareDurations

isMultiplierIdentity :: Duration -> Bool
isMultiplierIdentity = (1 ==) . view multiplier

-- no multiplier
rqToDuration :: Rq -> Maybe Duration
rqToDuration rq
  | rq <= 0 = Nothing
  | otherwise =
      let divisions = [1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192]
          dots = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
          validDurations = [Duration d dt 1 | d <- divisions, dt <- dots]
          closestDuration = minimumBy (comparing $ \d -> abs (durationToRq d - rq)) validDurations
       in if durationToRq closestDuration == rq then Just closestDuration else Nothing

rqToDuration' :: Rq -> Maybe Duration
rqToDuration' rq
  | rq <= 0 = Nothing
  | otherwise =
      let divisions = [1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192]
          dots = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
          multipliers = [1, 2 / 3, 4 / 5]
          validDurations = [Duration d dt m | d <- divisions, dt <- dots, m <- multipliers]
          closestDuration = minimumBy (comparing $ \d -> abs (durationToRq d - rq)) validDurations
       in if durationToRq closestDuration == rq then Just closestDuration else Nothing

rqToDuration'' :: Rq -> [Duration]
rqToDuration'' rq
  | rq <= 0 = []
  | otherwise =
      let divisions = [1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192]
          dots = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
          multipliers = [1, 2 / 3, 4 / 5, 3/5, 2/5, 5/6, 6/7, 5/7, 4/7, 3/7, 2/7]
          validDurations = [Duration d dt m | d <- divisions, dt <- dots, m <- multipliers]
          matchingDurations = filter (\d -> durationToRq d == rq) validDurations
       in matchingDurations


durationToLilypondType :: Duration -> String
durationToLilypondType dur =
  let divStr =
        case view division dur of
          0 -> "\\breve"
          n -> show n
   in divStr ++ replicate (view dots dur) '.'

accessDivision :: Duration -> Division
accessDivision = view division

---------------------------------------------------------------------------- !!
-- ~ QuickCheck 
---------------------------------------------------------------------------- !!


-- instance Arbitrary Duration where
--   arbitrary = do
--     div <- Test.QuickCheck.elements [1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192]
--     dt <- choose (0, 12)
--     return Duration {_division = div, _dots = dt, _multiplier = (1 % 1)}

-- instance Arbitrary Duration where
--   arbitrary = do
--     div <- Test.QuickCheck.elements [1, 2, 4, 8, 16, 32, 64, 128, 256, 512]
--     dt <- choose (0, 2)
--     mult <- Test.QuickCheck.elements [1, 2 / 3, 4 / 5]
--     return Duration {_division = div, _dots = dt, _multiplier = mult}

-- -- prop_durationToRq :: Duration -> Bool
-- -- prop_durationToRq d = (rqToDuration . durationToRq) d == Just d

-- prop_rqToDuration' :: Duration -> Bool
-- prop_rqToDuration' d = (rqToDuration' . durationToRq) d == Just d

-- runTests :: IO ()
-- runTests = verboseCheck prop_rqToDuration'


instance Arbitrary Duration where
  arbitrary = do
    div <- Test.QuickCheck.elements [1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024]
    dt <- choose (0, 3)
    mult <- Test.QuickCheck.elements [1, 2 / 3, 4 / 5, 3/5, 5/6]
    return Duration {_division = div, _dots = dt, _multiplier = mult}

prop_rqToDuration' :: Duration -> Bool
prop_rqToDuration' d =
  let rq = durationToRq d
      durations = rqToDuration'' rq
   in d `elem` durations

runTests :: IO ()
runTests = verboseCheck prop_rqToDuration'

---------------------------------------------------------------------------- !!
-- ~ End QuickCheck 
---------------------------------------------------------------------------- !!

{-
d4 = Duration {_division = 8, _dots = 1, _multiplier = 1}
durationToRq d4
r1 = durationToRq d4
rqToDuration r1

d1 = Duration {_division = 4, _dots = 1, _multiplier = 0.75}
d2 = Duration {_division = 8, _dots = 0, _multiplier = 1}
d3 = Duration {_division = 8, _dots = 0, _multiplier = 4/5}
d4 = Duration {_division = 8, _dots = 1, _multiplier = 1}
d5 = Duration {_division = 8, _dots = 2, _multiplier = 1}

durationToRq d3

durationToRq d4
durationToRq d5

 -}


{-
d :: Duration
d1 = Duration {_division = 4, _dots = 1, _multiplier = 0.75}
d2 = Duration {_division = 8, _dots = 0, _multiplier = 1}
d3 = Duration {_division = 8, _dots = 0, _multiplier = 4/5}
d4 = Duration {_division = 8, _dots = 1, _multiplier = 1}
d5 = Duration {_division = 8, _dots = 2, _multiplier = 1}

 -}
{-
ghci> d3= Duration {_division = 8, _dots = 0, _multiplier = 4/5}
ghci> durationToRq d3
10 % 1
 -}

{-
ghci > d1 = Duration 4 3 1
ghci> d1
Duration {division = 4, dots = 1, multiplier = 1 % 1}
ghci> durationToLilypondType d1
-}