{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Accidental
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
import Data.Ratio ((%))

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

(@@) :: a -> (a -> b) -> b
(@@) = flip ($)

class Initializable a where
  initializeAccidental :: a -> Accidental

instance Initializable AccidentalName where
  initializeAccidental accidentalName =
    Accidental
      { _accName = accidentalName,
        _accAbbreviation = fromMaybe "" $ lookup accidentalName accidentalNameToAbbreviation,
        _accSemitones = fromMaybe 0 $ lookup accidentalName accidentalNameToSemitones,
        _accArrow = Nothing
      }

newtype AccidentalString = AccidentalString String

instance Initializable AccidentalString where
  initializeAccidental (AccidentalString a) =
    let n = fromMaybe (CustomAccidental 0) $ lookup a accidentalAbbreviationToName
        s = fromMaybe 0 $ lookup a accidentalAbbreviationToSemitones
     in Accidental {_accName = n, _accAbbreviation = a, _accSemitones = s, _accArrow = Nothing}

newtype AccidentalRational = AccidentalRational Rational

instance Initializable AccidentalRational where
  initializeAccidental (AccidentalRational s) =
    let n = fromMaybe (CustomAccidental s) $ lookup s accidentalSemitonesToName
        a = fromMaybe "" $ lookup s accidentalSemitonesToAbbreviation
     in Accidental {_accName = n, _accAbbreviation = a, _accSemitones = s, _accArrow = Nothing}

instance Num Accidental where
  (+) a1 a2 =
    let newSemitones = _accSemitones a1 + _accSemitones a2
     in initializeAccidental (AccidentalRational newSemitones)

  (-) a1 a2 =
    let newSemitones = _accSemitones a1 - _accSemitones a2
     in initializeAccidental (AccidentalRational newSemitones)

  (*) a1 a2 =
    let newSemitones = _accSemitones a1 * _accSemitones a2
     in initializeAccidental (AccidentalRational newSemitones)

  abs a =
    let absSemitones = abs (_accSemitones a)
     in initializeAccidental (AccidentalRational absSemitones)

  signum a =
    case compare (_accSemitones a) 0 of
      LT -> initializeAccidental (AccidentalRational (-1)) -- 'flat'
      EQ -> initializeAccidental (AccidentalRational 0) -- 'natural'
      GT -> initializeAccidental (AccidentalRational 1) -- 'sharp'

  fromInteger i = initializeAccidental (AccidentalRational $ fromInteger i)

{-
ghci> signum flat == flat
True

ghci > accidental = fromInteger 1 :: Accidental

ghci > accidental
Accidental {_accName = Sharp, _accAbbreviation = "s", _accArrow = Nothing, _accSemitones = 1 % 1}
-}

instance Fractional Accidental where
  fromRational r = initializeAccidental (AccidentalRational r)

  (/) a1 a2 =
    let newSemitones = _accSemitones a1 / _accSemitones a2
     in initializeAccidental (AccidentalRational newSemitones)

{-
ghci > acc = fromRational (1.5 :: Rational) :: Accidental

ghci > acc
Accidental {_accName = ThreeQuartersSharp, _accAbbreviation = "tqs", _accArrow = Nothing, _accSemitones = 3 % 2}

ghci > acc2 = -0.5 :: Accidental

ghci > acc + acc2
Accidental {_accName = Sharp, _accAbbreviation = "s", _accArrow = Nothing, _accSemitones = 1 % 1}
-}

accidentals :: [Accidental]
accidentals =
  [ Accidental Flat "f" Nothing ((-1) % 1),
    Accidental Natural "" Nothing (0 % 1),
    Accidental Sharp "s" Nothing (1 % 1),
    Accidental QuarterFlat "qf" Nothing ((-1) % 2),
    Accidental QuarterSharp "qs" Nothing (1 % 2),
    Accidental ThreeQuartersFlat "tqf" Nothing ((-3) % 2),
    Accidental ThreeQuartersSharp "tqs" Nothing (3 % 2),
    Accidental DoubleSharp "ss" Nothing (2 % 1),
    Accidental DoubleFlat "ff" Nothing ((-2) % 1),
    Accidental ThirdSharp "rs" Nothing (2 % 3),
    Accidental ThirdFlat "rf" Nothing ((-2) % 3),
    Accidental SixthSharp "xs" Nothing (1 % 3),
    Accidental SixthFlat "xf" Nothing ((-1) % 3),
    Accidental TwelfthSharp "ts" Nothing (1 % 6),
    Accidental TwelfthFlat "tf" Nothing ((-1) % 6),
    Accidental EighthSharp "es" Nothing (1 % 4),
    Accidental EighthFlat "ef" Nothing ((-1) % 4),
    Accidental FiveTwelfthsSharp "fts" Nothing (5 % 6),
    Accidental FiveTwelfthsFlat "ftf" Nothing ((-5) % 6),
    Accidental SevenTwelfthsSharp "sts" Nothing (7 % 6),
    Accidental SevenTwelfthsFlat "stf" Nothing ((-7) % 6),
    Accidental FiveSixthsSharp "fxs" Nothing (5 % 3),
    Accidental FiveSixthsFlat "fxf" Nothing ((-5) % 3),
    Accidental ElevenTwelfthsSharp "ets" Nothing (11 % 6),
    Accidental ElevenTwelfthsFlat "etf" Nothing ((-11) % 6),
    Accidental TwoThirdsFlat "trf" Nothing ((-4) % 3),
    Accidental TwoThirdsSharp "trf" Nothing (4 % 3),
    Accidental ThreeEighthsSharp "tes" Nothing (3 % 4),
    Accidental ThreeEighthsFlat "tef" Nothing ((-3) % 4)
  ]

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

{-
roundToNearestAccidental (11 % 15)
Accidental {_accName = ThreeEighthsSharp, _accAbbreviation = "tes", _accArrow = Nothing, _accSemitones = 3 % 4}

roundToNearestAccidental (2 % 7)
Accidental {_accName = EighthSharp, _accAbbreviation = "es", _accArrow = Nothing, _accSemitones = 1 % 4}

roundToNearestAccidental (13 % 21)
Accidental {_accName = ThirdSharp, _accAbbreviation = "rs", _accArrow = Nothing, _accSemitones = 2 % 3}

roundToNearestAccidental (0.83274 :: Rational)
Accidental {_accName = FiveTwelfthsSharp, _accAbbreviation = "fts", _accArrow = Nothing, _accSemitones = 5 % 6}

-}

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

{-
 sharp @@ arrowUp
Accidental {_accName = Sharp, _accAbbreviation = "s", _accArrow = Just Up, _accSemitones = 1 % 1}
-}

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

name :: Lens' Accidental AccidentalName
name = lens _accName (\acc newName -> acc {_accName = newName, _accSemitones = getSemitones newName, _accAbbreviation = getAbbreviation (_accSemitones acc)})
  where
    getSemitones n = fromMaybe (error "Invalid name value") $ lookup n accidentalNameToSemitones

    getAbbreviation :: Rational -> String
    getAbbreviation s = fromMaybe "" $ lookup s accidentalSemitonesToAbbreviation

abbreviation :: Lens' Accidental String
abbreviation = lens _accAbbreviation (\acc newAbbreviation -> acc {_accAbbreviation = newAbbreviation, _accName = getName newAbbreviation, _accSemitones = getSemitones (_accName acc)})
  where
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
semitone = lens _accSemitones (\acc newSemitones -> acc {_accSemitones = newSemitones, _accName = getName newSemitones, _accAbbreviation = getAbbreviation newSemitones})
  where
    getName :: Rational -> AccidentalName
    getName s = fromMaybe (CustomAccidental s) $ lookup s accidentalSemitonesToName

    getAbbreviation :: Rational -> String
    getAbbreviation s = fromMaybe "" $ lookup s accidentalSemitonesToAbbreviation

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

ghci > semiSharp & semitone -~ (1 % 4)
Accidental {_accName = CustomAccidental (1 % 4), _accAbbreviation = "", _accArrow = Nothing, _accSemitones = 1 % 4}

ghci > flat & semitone +~ (1 % 3)
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