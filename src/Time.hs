{-# LANGUAGE GADTs #-}

module Time (
    TimeSignature(..),
    Duration,
    measureDuration,
    Tempo(..),
    toSeconds
) where

import           Data.Ratio

type Duration = Rational

newtype Tempo where
  BPM :: Int -> Tempo

data TimeSignature where
    TS :: { tsNum :: Integer, tsDenom :: Integer } -> TimeSignature
    deriving (Eq, Ord, Show)

measureDuration :: TimeSignature -> Duration
measureDuration (TS num denom) = num % denom

toSeconds :: Tempo -> Duration -> Double
toSeconds (BPM bpm) duration = fromRational (duration / (toRational bpm / 60))
