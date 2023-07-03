{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "[]" #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Time (
    TimeSignature(..),
    Duration,
    measureDuration,
    Tempo(..),
    toSeconds
) where

type Duration = Rational

data Tempo = BPM Int
  deriving (Eq, Ord, Show)

data TimeSignature = TS { numerator :: Integer, denominator :: Integer }
  deriving (Eq, Ord, Show)

measureDuration :: TimeSignature -> Duration
measureDuration (TS num denom) = num % denom

toSeconds :: Duration -> Tempo -> Double
toSeconds duration (BPM bpm) =  fromRational duration / (fromIntegral bpm / 60)


{- test -}
d :: Duration
d = 1 % 2

t :: Tempo
t = BPM 110

result :: Double
result = toSeconds d t
