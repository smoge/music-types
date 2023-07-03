{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}


module Time (
    TimeSignature(..),
    Duration,
    measureDuration,
    Tempo(..),
    toSeconds,
    formatTime,
    isValidRMeasure
) where
import           Data.Ratio  ((%))
import           Text.Printf


type Duration = Rational

data Tempo where
  BPM :: Int -> Tempo
  deriving (Eq, Ord, Show)

data TimeSignature where
  TS :: {numerator :: Integer, denominator :: Integer}
          -> TimeSignature
  deriving (Eq, Ord, Show)

measureDuration :: TimeSignature -> Duration
measureDuration (TS num denom) = num Data.Ratio.% denom

toSeconds :: Duration -> Tempo -> Double
toSeconds duration (BPM bpm) =  fromRational duration / (fromIntegral bpm / 60)


formatTime :: Double -> String
formatTime seconds =
  let totalMilliseconds = round (seconds * 1000)
      milliseconds = totalMilliseconds `mod` 1000
      totalSeconds = totalMilliseconds `div` 1000
      minutes = totalSeconds `div` 60
      remainingSeconds = totalSeconds `mod` 60
  in printf "%02d:%02d:%03d" (minutes :: Int) (remainingSeconds :: Int) (milliseconds :: Int)

data RMeasure where
  RMeasure :: {timeSignature :: TimeSignature,
                 durations :: [Duration]}
                -> RMeasure
  deriving (Eq, Show)

isValidRMeasure :: RMeasure -> Bool
isValidRMeasure (RMeasure ts ds) = sum ds == measureDuration ts


{- test -}

d :: Duration
d = 1 % 2

-- t :: Tempo
-- t = BPM 120

result :: Double
result = toSeconds d (BPM 120)

rm :: RMeasure
rm = RMeasure (TS 4 4) [d, d, d, d]

isValid :: Bool
isValid = isValidRMeasure rm

main :: IO ()
main = do
  let formatted = formatTime result
  putStrLn formatted

-- "00:00:250"
