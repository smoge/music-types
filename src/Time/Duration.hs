{-# LANGUAGE TemplateHaskell #-}

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
dotMultiplier dotCount = n % d
  where
    n = 2 ^ (dotCount + 1) - 1
    d = 2 ^ dotCount

durationToRq :: Duration -> Rq
durationToRq d = (1 % (d ^. division)) * dotMultiplier (d ^. dots) * (d ^. multiplier)

compareDurations :: Duration -> Duration -> Ordering
compareDurations = comparing durationToRq

instance Ord Duration where
  compare = compareDurations

isMultiplierIdentity :: Duration -> Bool
isMultiplierIdentity = (1 ==) . view multiplier

rqToDuration :: Rq -> Maybe Duration
rqToDuration rq
  | rq <= 0 = Nothing
  | otherwise =
      let divisions = [1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192]
          dots = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
          validDurations = [Duration d dt 1 | d <- divisions, dt <- dots]
          closestDuration = minimumBy (comparing $ \d -> abs (durationToRq d - rq)) validDurations
       in if durationToRq closestDuration == rq then Just closestDuration else Nothing

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


instance Arbitrary Duration where
  arbitrary = do
    div <- Test.QuickCheck.elements [1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192]
    dt <- choose (0, 12)
    return Duration {_division = div, _dots = dt, _multiplier = (1 % 1)}

prop_durationToRq :: Duration -> Bool
prop_durationToRq d = (rqToDuration . durationToRq) d == Just d

runTests :: IO ()
runTests = quickCheckWith (stdArgs {maxSuccess = 1000}) prop_durationToRq

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

--! test
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
"4."
 -}
