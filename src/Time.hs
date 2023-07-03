{-# LANGUAGE GADTs #-}

module Time (
    TimeSignature(..),
    measureDuration
) where

import           Data.Ratio

data TimeSignature where
    TS :: { tsNum :: Integer, tsDenom :: Integer } -> TimeSignature
    deriving (Eq, Ord, Show)

measureDuration :: TimeSignature -> Rational
measureDuration (TS num denom) = num % denom
