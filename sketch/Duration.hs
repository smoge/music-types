{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

import Control.Lens 
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Ratio

type Division = Integer
type Dots = Int
type Rq = Rational

data Duration where
  Duration :: {  _division :: Division,
                 _dots :: Dots,
                 _multiplier :: Rational}
                -> Duration
  deriving (Eq, Show)


makeLenses ''Duration


dotMultiplier :: Dots -> Rational
dotMultiplier dotCount = n % d
  where
    n = 2 ^ (dotCount + 1) - 1
    d = 2 ^ dotCount


durationToRq :: Duration -> Rq
durationToRq d = (1 % (d ^. division)) * dotMultiplier (d ^. dots) * (d ^. multiplier)

{- 

d1 = Duration {_division = 4, _dots = 1, _multiplier = 0.75}
d2 = Duration {_division = 8, _dots = 0, _multiplier = 1}
d3 = Duration {_division = 8, _dots = 0, _multiplier = 4/5}
d4 = Duration {_division = 8, _dots = 1, _multiplier = 1}
d5 = Duration {_division = 8, _dots = 2, _multiplier = 1}

durationToRq d3

durationToRq d4
durationToRq d5

 -}

-- Function to compare the multipliers of two durations
areMultipliersEqual :: Duration -> Duration -> Bool
areMultipliersEqual dur1 dur2 = dur1 ^. multiplier == dur2 ^. multiplier

-- Function to check if the multiplier is the identity (ie. 1)
isMultiplierIdentity :: Duration -> Bool
isMultiplierIdentity = (== 1) . view multiplier

-- Function to compare two durations with equal multipliers.
compareDurationsWithEqualMultipliers :: Duration -> Duration -> Maybe Ordering
compareDurationsWithEqualMultipliers dur1 dur2
  | dur1 == dur2 = Just EQ
  | dur1 ^. multiplier /= dur2 ^. multiplier = Nothing
  | otherwise = Just $ compare (dur1 ^. division, dur1 ^. dots) (dur2 ^. division, dur2 ^. dots)


-- Example: map durationToLilypondType [Duration 2 0 1, Duration 4 1 1] == ["2","4."]
durationToLilypondType :: Duration -> String
durationToLilypondType dur =
    let divStr = if dur ^. division == 0 then "\\breve" else show (dur ^. division)
    in divStr ++ replicate (dur ^. dots) '.'
    

-- Accessing the division 
accessDivision :: Duration -> Division
accessDivision  = view division 

multiplyDuration :: Duration -> Rational -> Duration
multiplyDuration duration factor = over multiplier (* factor) duration


hasDots :: Duration -> Bool
hasDots = has (dots . filtered (/= 0))

-- Combining lenses with folds and traversals
sumDivisions :: [Duration] -> Division
sumDivisions = sumOf (folded . division)  -- Calculate the sum of divisions in a list of durations




-- test
{- 
d :: Duration
d1 = Duration {_division = 4, _dots = 1, _multiplier = 0.75}
d2 = Duration {_division = 8, _dots = 0, _multiplier = 1}
d3 = Duration {_division = 8, _dots = 0, _multiplier = 4/5}
d4 = Duration {_division = 8, _dots = 1, _multiplier = 1}
d5 = Duration {_division = 8, _dots = 2, _multiplier = 1}

-- todo
rqToClosestDuration $ durationToRq d2

rqToClosestDuration $ durationToRq d3

-- Chaining lens operations
modifyDuration :: Duration -> Duration
modifyDuration duration = duration & division %~ (+ 2)    -- Increment the division by 2
                                  & dots %~ (* 2)        -- Double the dots value
                                  & multiplier %~ recip  -- Take the reciprocal of the multiplier


 -}
{- 
ghci> d3= Duration {_division = 8, _dots = 0, _multiplier = 4/5}
ghci> durationToRq d3
10 % 1
 -}

{- 
ghci> d1 = Duration 4 1 1
ghci> d1
Duration {division = 4, dots = 1, multiplier = 1 % 1}
ghci> durationToLilypondType d1
"4."
 -}

