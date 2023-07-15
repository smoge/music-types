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
    orderedSemitones,
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
import Data.Maybe (fromMaybe)
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
    Accidental TwoThirdsSharp "trs" Nothing (1 % 3),
    Accidental TwoThirdsFlat "trf" Nothing ((-1) % 3),
    Accidental ThreeEighthsSharp "tes" Nothing (3 % 4),
    Accidental ThreeEighthsFlat "tef" Nothing ((-3) % 4)
  ]

orderedSemitones :: [Rational]
orderedSemitones = [(-2) % 1, (-11) % 6, (-5) % 3, (-3) % 2, (-7) % 6, (-1) % 1, (-5) % 6, (-2) % 3, (-1) % 2, (-1) % 3, (-1) % 3, (-1) % 4, (-1) % 6, 0 % 1, 1 % 6, 1 % 4, 1 % 3, 1 % 3, 1 % 2, 2 % 3, 5 % 6, 1 % 1, 7 % 6, 3 % 2, 5 % 3, 11 % 6, 2 % 1]

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

semitone :: Lens' Accidental Rational
semitone = lens _accSemitones (\acc newSemitones -> acc {_accSemitones = newSemitones, _accName = getName newSemitones, _accAbbreviation = getAbbreviation newSemitones})
  where
    getName :: Rational -> AccidentalName
    getName s = fromMaybe (CustomAccidental s) $ lookup s accidentalSemitonesToName

    getAbbreviation :: Rational -> String
    getAbbreviation s = fromMaybe "" $ lookup s accidentalSemitonesToAbbreviation

-- semitonesLens :: Lens' Accidental Rational
-- semitonesLens = lens _accSemitones (\acc newSemitones -> acc {_accSemitones = newSemitones, _accName = getName newSemitones, _accAbbreviation = getAbbreviation newSemitones})
--   where
--     getName :: Rational -> AccidentalName
--     getName s = fromMaybe (CustomAccidental s) $ lookup s accidentalSemitonesToName

--     getAbbreviation :: Rational -> String
--     getAbbreviation s = fromMaybe "" $ lookup s accidentalSemitonesToAbbreviation

----

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

----

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

-}

{-

import Data.Ratio ((%))

-- Accessing Accidental properties using lenses

-- Get the semitones of the 'sharp' Accidental
example1 = sharp ^. semitone -- Result: 1 % 1

-- Get the name of the 'sharp' Accidental
example2 = sharp ^. name -- Result: Sharp

-- Get the abbreviation of the 'sharp' Accidental
example3 = sharp ^. abbreviation -- Result: "s"

-- Modifying Accidental properties using lenses

-- Set the semitones of the 'flat' Accidental to 1 % 1
example4 = flat & semitone .~ (1 % 1)

-- Result: Accidental {_accName = Sharp, _accAbbreviation = "s", _accArrow = Nothing, _accSemitones = 1 % 1}

-- Increase the semitones of the 'flat' Accidental by 1 % 1
example5 = flat & semitone +~ (1 % 1)

-- Result: Accidental {_accName = Natural, _accAbbreviation = "", _accArrow = Nothing, _accSemitones = 0 % 1}

-- Set the name of the 'natural' Accidental to 'DoubleSharp'
example6 = natural & name .~ DoubleSharp

-- Result: Accidental {_accName = DoubleSharp, _accAbbreviation = "", _accArrow = Nothing, _accSemitones = 2 % 1}

-- Append 'x' to the abbreviation of the 'doubleSharp' Accidental
example7 = doubleSharp & abbreviation %~ (++ "x")

-- Result: Accidental {_accName = CustomAccidental (0 % 1), _accAbbreviation = "ssx", _accArrow = Nothing, _accSemitones = 2 % 1}

-- Accessing and modifying Accidental properties using lenses

-- Access the semitones of the 'semiFlat' Accidental and add 0.5 to the value
example8 = semiFlat ^. semitone + 0.5 -- Result: 0 % 1

-- Set the arrow of the 'doubleFlat' Accidental to 'Just Up'
example9 = doubleFlat & arrow ?~ Up

-- Result: Accidental {_accName = DoubleFlat, _accAbbreviation = "ff", _accArrow = Just Up, _accSemitones = -2 % 1}

-- Decrease the semitones of the 'semiSharp' Accidental by 1 % 4
example10 = semiSharp & semitone -~ (1 % 4)

-- Result: Accidental {_accName = CustomAccidental (1 % 4), _accAbbreviation = "", _accArrow = Nothing, _accSemitones = 1 % 4}

-}

{-

57
Wyschnegradsky accidentals (72-EDO)
 U+E420 accidentalWyschnegradsky1TwelfthsSharp 72
 U+E421 accidentalWyschnegradsky2TwelfthsSharp 72
 U+E422 accidentalWyschnegradsky3TwelfthsSharp 72
 U+E423 accidentalWyschnegradsky4TwelfthsSharp 72
 U+E424 accidentalWyschnegradsky5TwelfthsSharp 72
 U+E425 accidentalWyschnegradsky6TwelfthsSharp 72
 U+E426 accidentalWyschnegradsky7TwelfthsSharp 72
 U+E427 accidentalWyschnegradsky8TwelfthsSharp 72
 U+E428 accidentalWyschnegradsky9TwelfthsSharp 72
 U+E429 accidentalWyschnegradsky10TwelfthsSharp 72
 U+E42A accidentalWyschnegradsky11TwelfthsSharp 72
 U+E42B accidentalWyschnegradsky1TwelfthsFlat 72
 U+E42C accidentalWyschnegradsky2TwelfthsFlat 72
 U+E42D accidentalWyschnegradsky3TwelfthsFlat 72
 U+E42E accidentalWyschnegradsky4TwelfthsFlat 72
 U+E42F accidentalWyschnegradsky5TwelfthsFlat 72
 U+E430 accidentalWyschnegradsky6TwelfthsFlat 72
 U+E431 accidentalWyschnegradsky7TwelfthsFlat 72
 U+E432 accidentalWyschnegradsky8TwelfthsFlat 72
 U+E433 accidentalWyschnegradsky9TwelfthsFlat 72
 U+E434 accidentalWyschnegradsky10TwelfthsFlat 72
 U+E435 accidentalWyschnegradsky11TwelfthsFlat 72

-}