{-# LANGUAGE GADTs #-}

module Time (
    TimeSignature(..),
    Duration,
    measureDuration,
    Tempo(..),
    toSeconds
) where
import           Data.Ratio ((%))

type Duration = Rational

data Tempo = BPM Int
  deriving (Eq, Ord, Show)

data TimeSignature where
  TS :: {numerator :: Integer, denominator :: Integer}
          -> TimeSignature
  deriving (Eq, Ord, Show)

measureDuration :: TimeSignature -> Duration
measureDuration (TS num denom) = num Data.Ratio.% denom

toSeconds :: Duration -> Tempo -> Double
toSeconds duration (BPM bpm) =  fromRational duration / (fromIntegral bpm / 60)


{- test -}
d :: Duration
d = 1 Data.Ratio.% 2

t :: Tempo
t = BPM 120

result :: Double
result = toSeconds d t
