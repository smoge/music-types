{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Time (
    TimeSignature(..),
    Duration,
    measureDuration,
    Tempo(..),
    toSeconds,
    formatTime,
    isValidRMeasure,
    Tuplet(..),
    calculateTotalDuration,
    createTuplet
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



totalDuration :: [Duration] -> Duration
totalDuration = sum

durationRatio :: Duration -> Duration -> Rational
durationRatio d1 d2 = d1 / d2

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

-- data RMeasureComponent = DurationInRMeasure Duration | TupletInRMeasure Tuplet
--   deriving (Eq, Show)

data RMeasureComponent where
    DurationInRMeasure :: Duration -> RMeasureComponent
    TupletInRMeasure :: Tuplet -> RMeasureComponent
    deriving (Eq, Show)

data RMeasure where
    RMeasure :: TimeSignature -> [RMeasureComponent] -> RMeasure
    deriving (Eq, Show)

isValidRMeasure :: RMeasure -> Bool
isValidRMeasure (RMeasure ts components) = totalDur == measureDuration ts
    where
        totalDur = sum $ map componentDuration components
        componentDuration (DurationInRMeasure d) = d
        componentDuration (TupletInRMeasure (Tuplet _ _ _ multipliedDur)) = multipliedDur


data TupletComponent = DurationComponent Duration | NestedTupletComponent Tuplet
  deriving (Eq, Show)

data Tuplet where
  Tuplet :: {tupletMultiplier :: Rational,
               tupletComponents :: [TupletComponent],
               tupletTotalDuration :: Duration,
               tupletMultipliedDuration :: Duration}
              -> Tuplet
  deriving (Eq, Show)

getTupletMultipliedDuration :: Tuplet -> Duration
getTupletMultipliedDuration = tupletMultipliedDuration

getTupletMultiplier :: Tuplet -> Rational
getTupletMultiplier = tupletMultiplier

getTupletTotalDuration :: Tuplet -> Duration
getTupletTotalDuration = tupletTotalDuration

calculateTotalDuration :: [TupletComponent] -> Duration
calculateTotalDuration components =
  sum $ map calculateDuration components
  where
    calculateDuration (DurationComponent duration) = duration
    calculateDuration (NestedTupletComponent tuplet) = tupletTotalDuration tuplet * tupletMultiplier tuplet

formatRMeasure :: RMeasure -> String
formatRMeasure (RMeasure (TS numerator denominator) components) =
    "RMeasure (timeSignature: " ++ show numerator ++ " / " ++ show denominator ++ ")\n"
    ++ concatMap formatComponent components
  where
    formatComponent :: RMeasureComponent -> String
    formatComponent (DurationInRMeasure duration) =
        "  Duration: " ++ show duration ++ "\n"
    formatComponent (TupletInRMeasure tuplet) =
        "  " ++ formatTuplet tuplet ++ "\n"


createTuplet :: Rational -> [TupletComponent] -> Tuplet
createTuplet multiplier components =
  let totalDur = calculateTotalDuration components
      multipliedDuration = totalDur * multiplier
  in Tuplet multiplier components totalDur multipliedDuration

formatTuplet :: Tuplet -> String
formatTuplet tuplet = formatTupletWithIndentation tuplet 0

formatTupletWithIndentation :: Tuplet -> Int -> String
formatTupletWithIndentation (Tuplet multiplier components _ dur) indentationLevel =
    let indentation = replicate (indentationLevel * 2) ' '
        formattedComponents = concatMap (formatComponent (indentationLevel + 1)) components
    in indentation ++ "Tuplet: (ratio: " ++ show multiplier ++ ", duration: " ++ show dur ++ ")\n" ++ formattedComponents

formatComponent :: Int -> TupletComponent -> String
formatComponent indentationLevel (DurationComponent duration) =
    replicate (indentationLevel * 2) ' ' ++ "Duration: " ++ show duration ++ "\n"
formatComponent indentationLevel (NestedTupletComponent tuplet) =
    formatTupletWithIndentation tuplet indentationLevel

-- -------------------------------------------------------------------------- --
-- TESTS --
-- -------------------------------------------------------------------------- --

d :: Duration
d = 1 % 2

t :: Tempo
t = BPM 120

secs :: Double
secs = toSeconds d (BPM 120)

-- rm :: RMeasure
-- rm = RMeasure (TS 4 4) [d, d, d, d]

-- isValid :: Bool
-- isValid = isValidRMeasure rm

test1 :: IO ()
test1 = do
    let duration1 = 4 % 8
        duration2 = 5 % 8
        ratio = durationRatio duration1 duration2
    putStrLn $ "Duration ratio: " ++ show ratio
-- Duration ratio: 4 % 5

test2 :: IO ()
test2 = do
  let formatted = formatTime secs
  putStrLn formatted
-- "00:00:250"

test3:: IO ()
test3 = do
  let innerTuplet = createTuplet (2 % 3) [DurationComponent (1 % 8), DurationComponent (1 % 8), DurationComponent (1 % 8)]
      components = [DurationComponent (1 % 8), NestedTupletComponent innerTuplet, DurationComponent (1 % 8), DurationComponent (1 % 8)]
      tuplet = createTuplet (4 % 5) components
  print tuplet
-- Tuplet {tupletMultiplier = 4 % 5, tupletComponents = [DurationComponent (1 % 8),NestedTupletComponent (Tuplet {tupletMultiplier = 2 % 3, tupletComponents = [DurationComponent (1 % 8),DurationComponent (1 % 8),DurationComponent (1 % 8)], tupletTotalDuration = 3 % 8, tupletMultipliedDuration = 1 % 4}),DurationComponent (1 % 8),DurationComponent (1 % 8)], tupletTotalDuration = 5 % 8, tupletMultipliedDuration = 1 % 2}

test4:: IO ()
test4 = do
    let innerTuplet = createTuplet (2 % 3) [DurationComponent (1 % 8), DurationComponent (1 % 8), DurationComponent (1 % 8)]
        components = [DurationComponent (1 % 8), NestedTupletComponent innerTuplet, DurationComponent (1 % 8), DurationComponent (1 % 8)]
        tuplet = createTuplet (4 % 5) components
    putStrLn $ formatTuplet tuplet

{-
Tuplet: (ratio: 4 % 5, duration: 1 % 2)
  Duration: 1 % 8
  Tuplet: (ratio: 2 % 3, duration: 1 % 4)
    Duration: 1 % 8
    Duration: 1 % 8
    Duration: 1 % 8
  Duration: 1 % 8
  Duration: 1 % 8
-}

testRMeasure :: IO ()
testRMeasure = do
    let myDuration = 1 % 8 :: Duration
        myTuplet = createTuplet (2 % 3) [DurationComponent (1 % 8), DurationComponent (1 % 8), DurationComponent (1 % 8)]
        myRMeasure = RMeasure (TS 3 8) [DurationInRMeasure myDuration, TupletInRMeasure myTuplet]

    putStrLn $ formatRMeasure myRMeasure

{- FIXME
RMeasure (timeSignature: 3 / 8)
  Duration: 1 % 8
  Tuplet: (ratio: 2 % 3, duration: 1 % 4)
  Duration: 1 % 8
  Duration: 1 % 8
  Duration: 1 % 8
-}
