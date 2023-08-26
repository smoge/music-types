{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedLists   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE TemplateHaskell   #-}

module Pitch.Accidental
  ( Accidental (..),
    Arrow (..),
    AccidentalName (..),
    name,
    arrow,
    semitone,
    abbreviation,
    defaultAccidental,
    accidentals,
    accidentalNameToAbbreviation,
    accidentalNameToSemitones,
    accidentalAbbreviationToSemitones,
    invertMapping,
    accidentalAbbreviationToName,
    accidentalSemitonesToAbbreviation,
    accidentalSemitonesToName,
    quarterSharpen,
    quarterFlatten,
    eighthSharpen,
    eighthFlatten,
    thirdSharpen,
    thirdFlatten,
    sixthSharpen,
    sixthFlatten,
    sharpen,
    flatten,
    sharp,
    flat,
    natural,
    semiSharp,
    semiFlat,
    quarterSharp,
    quarterFlat,
    threeQuartersSharp,
    threeQuartersFlat,
    doubleFlat,
    doubleSharp,
    sixthSharp,
    sixthFlat,
    twelfthSharp,
    twelfthFlat,
    eighthSharp,
    eighthFlat,
    threeEighthsSharp,
    threeEighthsFlat,
    fiveSixthsSharp,
    fiveSixthsFlat,
    elevenTwelfthsSharp,
    elevenTwelfthsFlat,
    twoThirdsSharp,
    twoThirdsFlat,
  )
where

import Control.Lens
import Data.List (maximumBy, minimumBy, sortOn)
import Data.Maybe (fromMaybe, isJust)
import Data.Ord (comparing)
import Data.Ratio 
import Test.QuickCheck 

data Arrow = Up | Down deriving (Show, Eq)

data AccidentalName
  = Sharp
  | Flat
  | Natural
  | QuarterSharp
  | QuarterFlat
  | ThreeQuartersSharp
  | ThreeQuartersFlat
  | DoubleFlat
  | DoubleSharp
  | ThirdSharp
  | ThirdFlat
  | SixthSharp
  | SixthFlat
  | TwelfthSharp
  | TwelfthFlat
  | EighthSharp
  | EighthFlat
  | FiveTwelfthsSharp
  | FiveTwelfthsFlat
  | SevenTwelfthsSharp
  | SevenTwelfthsFlat
  | FiveSixthsSharp
  | FiveSixthsFlat
  | ElevenTwelfthsSharp
  | ElevenTwelfthsFlat
  | TwoThirdsSharp
  | TwoThirdsFlat
  | ThreeEighthsSharp
  | ThreeEighthsFlat
  | CustomAccidental Rational
  deriving (Show, Eq)

data Accidental = Accidental
  { _accName :: AccidentalName,
    _accAbbreviation :: String,
    _accArrow :: Maybe Arrow,
    _accSemitones :: Rational
  }
  deriving (Show, Eq)

instance Ord Accidental where
  compare acc1 acc2 = compare (_accSemitones acc1) (_accSemitones acc2)

defaultAccidental :: Accidental
defaultAccidental =
  Accidental
    { _accName = Natural,
      _accAbbreviation = "",
      _accArrow = Nothing,
      _accSemitones = 0 % 1
    }

-- (@@) :: a -> (a -> b) -> b
-- (@@) = flip ($)

class Initializable a where
  initializeAccidental :: a -> Accidental

instance Initializable AccidentalName where
  initializeAccidental :: AccidentalName -> Accidental
  initializeAccidental accidentalName =
    Accidental
      { _accName = accidentalName,
        _accAbbreviation = fromMaybe "" $ lookup accidentalName accidentalNameToAbbreviation,
        _accSemitones = fromMaybe 0 $ lookup accidentalName accidentalNameToSemitones,
        _accArrow = Nothing
      }

instance Initializable String where
  initializeAccidental :: String -> Accidental
  initializeAccidental a =
    let n = fromMaybe (CustomAccidental 0) $ lookup a accidentalAbbreviationToName
        s = fromMaybe 0 $ lookup a accidentalAbbreviationToSemitones
     in Accidental {_accName = n, _accAbbreviation = a, _accSemitones = s, _accArrow = Nothing}

instance Initializable Rational where
  initializeAccidental :: Rational -> Accidental
  initializeAccidental s =
    let n = fromMaybe (CustomAccidental s) $ lookup s accidentalSemitonesToName
        a = fromMaybe "" $ lookup s accidentalSemitonesToAbbreviation
     in Accidental {_accName = n, _accAbbreviation = a, _accSemitones = s, _accArrow = Nothing}

instance Num Accidental where
  (+) :: Accidental -> Accidental -> Accidental
  (+) a1 a2 =
    let newSemitones = _accSemitones a1 + _accSemitones a2
     in initializeAccidental newSemitones

  (-) :: Accidental -> Accidental -> Accidental
  (-) a1 a2 =
    let newSemitones = _accSemitones a1 - _accSemitones a2
     in initializeAccidental newSemitones

  (*) :: Accidental -> Accidental -> Accidental
  (*) a1 a2 =
    let newSemitones = _accSemitones a1 * _accSemitones a2
     in initializeAccidental newSemitones

  abs :: Accidental -> Accidental
  abs a =
    let absSemitones = abs (_accSemitones a)
     in initializeAccidental absSemitones

instance Fractional Accidental where
  fromRational :: Rational -> Accidental
  fromRational = initializeAccidental

  (/) :: Accidental -> Accidental -> Accidental
  (/) a1 a2 = initializeAccidental $ _accSemitones a1 / _accSemitones a2

data AccidentalData = AccidentalData
  { abbrev :: String,
    semitones :: Rational
  }
  deriving (Show, Eq)

getAccidentalData :: AccidentalName -> Maybe AccidentalData
getAccidentalData name = lookup name accidentalMapping

semitoneToNameMapping :: [(Rational, AccidentalName)]
semitoneToNameMapping = [(semitones d, n) | (n, d) <- accidentalMapping]

normalizeAccidental :: Rational -> Maybe AccidentalName
normalizeAccidental r = lookup r semitoneToNameMapping

accidentalMapping :: [(AccidentalName, AccidentalData)]
accidentalMapping =
  [ (Flat, AccidentalData "f" ((-1) % 1)),
    (Natural, AccidentalData "" (0 % 1)),
    (Sharp, AccidentalData "s" (1 % 1)),
    (QuarterFlat, AccidentalData "qf" ((-1) % 2)),
    (QuarterSharp, AccidentalData "qs" (1 % 2)),
    (ThreeQuartersFlat, AccidentalData "tqf" ((-3) % 2)),
    (ThreeQuartersSharp, AccidentalData "tqs" (3 % 2)),
    (DoubleSharp, AccidentalData "ss" (2 % 1)),
    (DoubleFlat, AccidentalData "ff" ((-2) % 1)),
    (ThirdSharp, AccidentalData "rs" (2 % 3)),
    (ThirdFlat, AccidentalData "rf" ((-2) % 3)),
    (SixthSharp, AccidentalData "xs" (1 % 3)),
    (SixthFlat, AccidentalData "xf" ((-1) % 3)),
    (TwelfthSharp, AccidentalData "ts" (1 % 6)),
    (TwelfthFlat, AccidentalData "tf" ((-1) % 6)),
    (EighthSharp, AccidentalData "es" (1 % 4)),
    (EighthFlat, AccidentalData "ef" ((-1) % 4)),
    (FiveTwelfthsSharp, AccidentalData "fts" (5 % 6)),
    (FiveTwelfthsFlat, AccidentalData "ftf" ((-5) % 6)),
    (SevenTwelfthsSharp, AccidentalData "sts" (7 % 6)),
    (SevenTwelfthsFlat, AccidentalData "stf" ((-7) % 6)),
    (FiveSixthsSharp, AccidentalData "fxs" (5 % 3)),
    (FiveSixthsFlat, AccidentalData "fxf" ((-5) % 3)),
    (ElevenTwelfthsSharp, AccidentalData "ets" (11 % 6)),
    (ElevenTwelfthsFlat, AccidentalData "etf" ((-11) % 6)),
    (TwoThirdsSharp, AccidentalData "tts" (2 % 3)),
    (TwoThirdsFlat, AccidentalData "ttf" ((-2) % 3)),
    (ThreeEighthsSharp, AccidentalData "tes" (3 % 4)),
    (ThreeEighthsFlat, AccidentalData "tef" ((-3) % 4))
  ]

-- Generate the list of Accidental from accidentalMapping
accidentals :: [Accidental]
accidentals = 
  [Accidental n (abbrev d) Nothing (semitones d) | (n, d) <- accidentalMapping]



orderedAccidentals :: [Accidental]
orderedAccidentals = sortOn _accSemitones accidentals

sortAccidentalsBySemitones :: [Accidental] -> [Accidental]
sortAccidentalsBySemitones = sortOn _accSemitones

getAccidentalsAbbreviations :: [Accidental] -> [String]
getAccidentalsAbbreviations = map _accAbbreviation

et12Accidentals :: [Accidental]
et12Accidentals = map initializeAccidental [Flat, Sharp, Natural, DoubleFlat, DoubleSharp]

et24Accidentals :: [Accidental]
et24Accidentals = map initializeAccidental [Flat, Sharp, Natural, DoubleFlat, DoubleSharp, QuarterSharp, QuarterFlat, ThreeQuartersSharp, ThreeQuartersFlat]

etAccidentalMap :: Int -> [(Int, Accidental)]
etAccidentalMap 12 = [(0, natural), (1, sharp), (-1, flat), (2, doubleSharp), (-2, doubleFlat)]
etAccidentalMap 24 = [(0, natural), (2, sharp), (-2, flat), (4, doubleSharp), (-4, doubleFlat), (1, quarterSharp), (-1, quarterFlat), (3, threeQuartersSharp), (-3, threeQuartersFlat)]

getAccidentalBySteps :: Int -> Int -> Maybe Accidental
getAccidentalBySteps divisions steps
  | divisions `elem` ([12, 24] :: [Int]) = lookup steps (etAccidentalMap divisions)
  | otherwise = Nothing

isET12Accidental :: Accidental -> Bool
isET12Accidental acc = _accName acc `elem` map _accName et12Accidentals

isET24Accidental :: Accidental -> Bool
isET24Accidental acc = _accName acc `elem` map _accName et24Accidentals

roundToNearestAccidental :: Rational -> Accidental
roundToNearestAccidental num =
  minimumBy (comparing (abs . subtract num . _accSemitones)) accidentals

roundToET12Accidental :: Accidental -> Accidental
roundToET12Accidental acc =
  let floorAccidental = maximumBy (comparing _accSemitones) $ filter (<= acc) et12Accidentals
      ceilingAccidental = minimumBy (comparing _accSemitones) $ filter (>= acc) et12Accidentals
   in if abs (_accSemitones floorAccidental - _accSemitones acc) <= abs (_accSemitones ceilingAccidental - _accSemitones acc)
        then floorAccidental
        else ceilingAccidental

roundToET24Accidental :: Accidental -> Accidental
roundToET24Accidental acc =
  let floorAccidental = maximumBy (comparing _accSemitones) $ filter (<= acc) et24Accidentals
      ceilingAccidental = minimumBy (comparing _accSemitones) $ filter (>= acc) et24Accidentals
   in if abs (_accSemitones floorAccidental - _accSemitones acc) <= abs (_accSemitones ceilingAccidental - _accSemitones acc)
        then floorAccidental
        else ceilingAccidental

hasArrow :: Accidental -> Bool
hasArrow acc = isJust (_accArrow acc)

arrowUp :: Accidental -> Accidental
arrowUp = arrow ?~ Up

arrowDown :: Accidental -> Accidental
arrowDown = arrow ?~ Down

noArrow :: Accidental -> Accidental
noArrow = arrow .~ Nothing

accidentalNameToAbbreviation :: [(AccidentalName, String)]
accidentalNameToAbbreviation = [(_accName accidental, _accAbbreviation accidental) | accidental <- accidentals]

accidentalNameToSemitones :: [(AccidentalName, Rational)]
accidentalNameToSemitones = [(_accName accidental, _accSemitones accidental) | accidental <- accidentals]

accidentalAbbreviationToSemitones :: [(String, Rational)]
accidentalAbbreviationToSemitones = [(_accAbbreviation accidental, _accSemitones accidental) | accidental <- accidentals]

invertMapping :: [(a, b)] -> [(b, a)]
invertMapping = map (\(a, b) -> (b, a))

accidentalAbbreviationToName :: [(String, AccidentalName)]
accidentalAbbreviationToName = invertMapping accidentalNameToAbbreviation

accidentalSemitonesToAbbreviation :: [(Rational, String)]
accidentalSemitonesToAbbreviation = invertMapping accidentalAbbreviationToSemitones

accidentalSemitonesToName :: [(Rational, AccidentalName)]
accidentalSemitonesToName = invertMapping accidentalNameToSemitones
-- Begin Code Snippet --
name :: Lens' Accidental AccidentalName
name = lens _accName updateAccidentalFields
  where
    updateAccidentalFields acc newName = acc {_accName = newName, _accSemitones = getSemitones newName, _accAbbreviation = getAbbreviation (_accSemitones acc)}

    getSemitones n = fromMaybe (error "Invalid name value") $ lookup n accidentalNameToSemitones

    getAbbreviation :: Rational -> String
    getAbbreviation s = fromMaybe "" $ lookup s accidentalSemitonesToAbbreviation

abbreviation :: Lens' Accidental String
abbreviation = lens _accAbbreviation updateAccidentalFields
  where
    updateAccidentalFields acc newAbbreviation = acc {_accAbbreviation = newAbbreviation, _accName = getName newAbbreviation, _accSemitones = getSemitones (_accName acc)}

    getName :: String -> AccidentalName
    getName a = fromMaybe (CustomAccidental 0) $ lookup a accidentalAbbreviationToName

    getSemitones :: AccidentalName -> Rational
    getSemitones n = fromMaybe 0 $ lookup n accidentalNameToSemitones

arrow :: Lens' Accidental (Maybe Arrow)
arrow = lens _accArrow (\acc newArrow -> acc {_accArrow = newArrow})

instance Semigroup Accidental where
  (<>) a1 a2 =
    let newSemitones = _accSemitones a1 + _accSemitones a2
     in Accidental
          { _accName = getName newSemitones,
            _accAbbreviation = getAbbreviation newSemitones,
            _accArrow = Nothing,
            _accSemitones = newSemitones
          }
    where
      getName s = fromMaybe (CustomAccidental s) $ lookup s accidentalSemitonesToName
      getAbbreviation s = fromMaybe "" $ lookup s accidentalSemitonesToAbbreviation

semitone :: Lens' Accidental Rational
semitone = lens _accSemitones updateAccidentalFields
  where
    updateAccidentalFields acc newSemitones = acc {_accSemitones = newSemitones, _accName = getName newSemitones, _accAbbreviation = getAbbreviation newSemitones}

    getName :: Rational -> AccidentalName
    getName s = fromMaybe (CustomAccidental s) $ lookup s accidentalSemitonesToName

    getAbbreviation :: Rational -> String
    getAbbreviation s = fromMaybe "" $ lookup s accidentalSemitonesToAbbreviation
-- End Code Snippet --

-- Simple Modifiers

sharpen :: Accidental -> Accidental
sharpen = over semitone (+ (1 % 1))

flatten :: Accidental -> Accidental
flatten = over semitone (subtract (1 % 1))

quarterSharpen :: Accidental -> Accidental
quarterSharpen = over semitone (+ (1 % 2))

quarterFlatten :: Accidental -> Accidental
quarterFlatten = over semitone (subtract (1 % 2))

eighthSharpen :: Accidental -> Accidental
eighthSharpen = over semitone (+ (1 % 4))

eighthFlatten :: Accidental -> Accidental
eighthFlatten = over semitone (subtract (1 % 4))

thirdSharpen :: Accidental -> Accidental
thirdSharpen = over semitone (+ (3 % 2))

thirdFlatten :: Accidental -> Accidental
thirdFlatten = over semitone (subtract (3 % 2))

sixthSharpen :: Accidental -> Accidental
sixthSharpen = over semitone (+ (1 % 3))

sixthFlatten :: Accidental -> Accidental
sixthFlatten = over semitone (subtract (1 % 3))

---- Literals

sharp :: Accidental
sharp = initializeAccidental Sharp

flat :: Accidental
flat = initializeAccidental Flat

natural :: Accidental
natural = initializeAccidental Natural

semiSharp :: Accidental
semiSharp = initializeAccidental QuarterSharp

semiFlat :: Accidental
semiFlat = initializeAccidental QuarterFlat

quarterSharp :: Accidental
quarterSharp = initializeAccidental QuarterSharp

quarterFlat :: Accidental
quarterFlat = initializeAccidental QuarterFlat

threeQuartersSharp :: Accidental
threeQuartersSharp = initializeAccidental ThreeQuartersSharp

threeQuartersFlat :: Accidental
threeQuartersFlat = initializeAccidental ThreeQuartersFlat

doubleFlat :: Accidental
doubleFlat = initializeAccidental DoubleFlat

doubleSharp :: Accidental
doubleSharp = initializeAccidental DoubleSharp

sixthSharp :: Accidental
sixthSharp = initializeAccidental SixthSharp

sixthFlat :: Accidental
sixthFlat = initializeAccidental SixthFlat

twelfthSharp :: Accidental
twelfthSharp = initializeAccidental TwelfthSharp

twelfthFlat :: Accidental
twelfthFlat = initializeAccidental TwelfthFlat

eighthSharp :: Accidental
eighthSharp = initializeAccidental EighthSharp

eighthFlat :: Accidental
eighthFlat = initializeAccidental EighthFlat

threeEighthsSharp :: Accidental
threeEighthsSharp = initializeAccidental ThreeEighthsSharp

threeEighthsFlat :: Accidental
threeEighthsFlat = initializeAccidental ThreeEighthsFlat

fiveSixthsSharp :: Accidental
fiveSixthsSharp = initializeAccidental FiveSixthsSharp

fiveSixthsFlat :: Accidental
fiveSixthsFlat = initializeAccidental FiveSixthsFlat

elevenTwelfthsSharp :: Accidental
elevenTwelfthsSharp = initializeAccidental ElevenTwelfthsSharp

elevenTwelfthsFlat :: Accidental
elevenTwelfthsFlat = initializeAccidental ElevenTwelfthsFlat

twoThirdsSharp :: Accidental
twoThirdsSharp = initializeAccidental TwoThirdsSharp

twoThirdsFlat :: Accidental
twoThirdsFlat = initializeAccidental TwoThirdsFlat

{-

ghci > sharp ^. semitone
1 % 1

ghci > sharp ^. name
Sharp

ghci > sharp ^. abbreviation
"s"

ghci > flat & semitone .~ (1 % 1)
Accidental {_accName = Sharp, _accAbbreviation = "s", _accArrow = Nothing, _accSemitones = 1 % 1}

ghci > flat & semitone +~ (1 % 1)
Accidental {_accName = Natural, _accAbbreviation = "", _accArrow = Nothing, _accSemitones = 0 % 1}

ghci> natural & name .~ DoubleSharp
Accidental {_accName = DoubleSharp, _accAbbreviation = "", _accArrow = Nothing, _accSemitones = 2 % 1}

ghci> doubleSharp & abbreviation %~ (++ "x")
Accidental {_accName = CustomAccidental (0 % 1), _accAbbreviation = "ssx", _accArrow = Nothing, _accSemitones = 2 % 1}

ghci> semiFlat ^. semitone + 0.5
0 % 1

ghci> doubleFlat & arrow ?~ Up
Accidental {_accName = DoubleFlat, _accAbbreviation = "ff", _accArrow = Just Up, _accSemitones = (-2) %

>>> semiSharp & semitone -~ (1 % 4)
Accidental {_accName = EighthSharp, _accAbbreviation = "es", _accArrow = Nothing, _accSemitones = 1 % 4}

>>> flat & semitone +~ (1 % 3)
Accidental {_accName = ThirdFlat, _accAbbreviation = "rf", _accArrow = Nothing, _accSemitones = (-2) % 3}

ghci > flat + flat & semitone +~ (1 % 3)
Accidental {_accName = FiveSixthsFlat, _accAbbreviation = "fxf", _accArrow = Nothing, _accSemitones = (-5) % 3}

orderedAccidentals :: [Accidental]
orderedAccidentals = sortBy (comparing _accSemitones) accidentals

positiveSemitones :: [Accidental] -> [Accidental]
positiveSemitones = filter (\acc -> _accSemitones acc > 0)

negativeSemitones :: [Accidental] -> [Accidental]
negativeSemitones = filter (\acc -> _accSemitones acc < 0)

positiveSemitones orderedAccidentals
negativeSemitones orderedAccidentals

-}

{-
ghci > getAccidentalsAbbreviations orderedAccidentals
["ff", "etf", "fxf", "tqf", "stf", "f", "ftf", "tef", "rf", "qf", "xf", "trf", "ef", "tf", "", "ts", "es", "xs", "trs", "qs", "rs", "tes", "fts", "s", "sts", "tqs", "fxs", "ets", "ss"]

ghci> getAccidentalBySteps 12 1
Just (Accidental {_accName = Sharp, _accAbbreviation = "s", _accArrow = Nothing, _accSemitones = 1 % 1})

ghci> getAccidentalBySteps 24 1
Just (Accidental {_accName = QuarterSharp, _accAbbreviation = "qs", _accArrow = Nothing, _accSemitones = 1 % 2})

-}

-- orderedSemitones :: [Rational]
-- orderedSemitones = [(-2) % 1, (-11) % 6, (-5) % 3, (-3) % 2, (-7) % 6, (-1) % 1, (-5) % 6, (-2) % 3, (-1) % 2, (-1) % 3, (-1) % 3, (-1) % 4, (-1) % 6, 0 % 1, 1 % 6, 1 % 4, 1 % 3, 1 % 3, 1 % 2, 2 % 3, 5 % 6, 1 % 1, 7 % 6, 3 % 2, 5 % 3, 11 % 6, 2 % 1]

-- accidental1 = initializeAccidental FiveSixthsSharp

-- rounded2 = roundToET24Accidental accidental1

-- Accidental {_accName = ThreeQuartersSharp, _accAbbreviation = "tqs", _accArrow = Nothing, _accSemitones = 3 % 2}




-- accidentals :: [Accidental]
-- accidentals =
--   [ Accidental Flat "f" Nothing ((-1) % 1),
--     Accidental Natural "" Nothing (0 % 1),
--     Accidental Sharp "s" Nothing (1 % 1),
--     Accidental QuarterFlat "qf" Nothing ((-1) % 2),
--     Accidental QuarterSharp "qs" Nothing (1 % 2),
--     Accidental ThreeQuartersFlat "tqf" Nothing ((-3) % 2),
--     Accidental ThreeQuartersSharp "tqs" Nothing (3 % 2),
--     Accidental DoubleSharp "ss" Nothing (2 % 1),
--     Accidental DoubleFlat "ff" Nothing ((-2) % 1),
--     Accidental ThirdSharp "rs" Nothing (2 % 3),
--     Accidental ThirdFlat "rf" Nothing ((-2) % 3),
--     Accidental SixthSharp "xs" Nothing (1 % 3),
--     Accidental SixthFlat "xf" Nothing ((-1) % 3),
--     Accidental TwelfthSharp "ts" Nothing (1 % 6),
--     Accidental TwelfthFlat "tf" Nothing ((-1) % 6),
--     Accidental EighthSharp "es" Nothing (1 % 4),
--     Accidental EighthFlat "ef" Nothing ((-1) % 4),
--     Accidental FiveTwelfthsSharp "fts" Nothing (5 % 6),
--     Accidental FiveTwelfthsFlat "ftf" Nothing ((-5) % 6),
--     Accidental SevenTwelfthsSharp "sts" Nothing (7 % 6),
--     Accidental SevenTwelfthsFlat "stf" Nothing ((-7) % 6),
--     Accidental FiveSixthsSharp "fxs" Nothing (5 % 3),
--     Accidental FiveSixthsFlat "fxf" Nothing ((-5) % 3),
--     Accidental ElevenTwelfthsSharp "ets" Nothing (11 % 6),
--     Accidental ElevenTwelfthsFlat "etf" Nothing ((-11) % 6),
--     Accidental TwoThirdsFlat "trf" Nothing ((-4) % 3),
--     Accidental TwoThirdsSharp "trf" Nothing (4 % 3),
--     Accidental ThreeEighthsSharp "tes" Nothing (3 % 4),
--     Accidental ThreeEighthsFlat "tef" Nothing ((-3) % 4)
--   ]




-- Property to ensure that the addition of two accidentals results in an Accidental
-- with a sum of the two semitones:
prop_additionCorrectness :: Accidental -> Accidental -> Bool
prop_additionCorrectness a1 a2 = _accSemitones (a1 + a2) == _accSemitones a1 + _accSemitones a2

-- Property to ensure the subtraction of two accidentals results in an Accidental
-- with the difference of the two semitones:
prop_subtractionCorrectness :: Accidental -> Accidental -> Bool
prop_subtractionCorrectness a1 a2 = _accSemitones (a1 - a2) == _accSemitones a1 - _accSemitones a2

-- Property to ensure the multiplication of two accidentals results in an Accidental
-- with a product of the two semitones:
prop_multiplicationCorrectness :: Accidental -> Accidental -> Bool
prop_multiplicationCorrectness a1 a2 = _accSemitones (a1 * a2) == _accSemitones a1 * _accSemitones a2

-- Property to ensure the division of two accidentals results in an Accidental
-- with a quotient of the two semitones, provided second Accidental is not zero:
prop_divisionCorrectness :: Accidental -> Accidental -> Property
prop_divisionCorrectness a1 a2 = (_accSemitones a2 /= 0) ==> _accSemitones (a1 / a2) == _accSemitones a1 / _accSemitones a2

-- A generator to create random Accidental
instance Arbitrary Accidental where
  arbitrary = Test.QuickCheck.elements accidentals



main :: IO ()
main = do
  let args = stdArgs {maxSuccess = 1000}

  putStrLn "Testing addition correctness:"
  quickCheckWith args prop_additionCorrectness

  putStrLn "Testing subtraction correctness:"
  quickCheckWith args prop_subtractionCorrectness

  putStrLn "Testing multiplication correctness:"
  quickCheckWith args prop_multiplicationCorrectness

  putStrLn "Testing division correctness:"
  quickCheckWith args prop_divisionCorrectness
  


{-
>>> roundToNearestAccidental (11 % 15)
Accidental {_accName = ThreeEighthsSharp, _accAbbreviation = "tes", _accArrow = Nothing, _accSemitones = 3 % 4}

>>> roundToNearestAccidental (2 % 7)
Accidental {_accName = EighthSharp, _accAbbreviation = "es", _accArrow = Nothing, _accSemitones = 1 % 4}

>>> roundToNearestAccidental (13 % 21)
Accidental {_accName = ThirdSharp, _accAbbreviation = "rs", _accArrow = Nothing, _accSemitones = 2 % 3}

>>> roundToNearestAccidental (0.83274 :: Rational)
Accidental {_accName = FiveTwelfthsSharp, _accAbbreviation = "fts", _accArrow = Nothing, _accSemitones = 5 % 6}

-}