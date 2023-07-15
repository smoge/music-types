{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Control.Lens.Operators
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
  | CustomAccidental Rational
  deriving (Show, Eq)

data Accidental = Accidental
  { _accName :: AccidentalName,
    _accAbbreviation :: String,
    _accArrow :: Maybe Arrow,
    _accSemitones :: Rational
  }
  deriving (Show)

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
    Accidental ThirdSharp "ts" Nothing (2 % 3),
    Accidental ThirdFlat "tf" Nothing ((-2) % 3),
    Accidental SixthSharp "sis" Nothing (1 % 3),
    Accidental SixthFlat "sif" Nothing ((-1) % 3),
    Accidental TwelfthSharp "tes" Nothing (1 % 6),
    Accidental TwelfthFlat "tef" Nothing ((-1) % 6),
    Accidental EighthSharp "es" Nothing (1 % 4),
    Accidental EighthFlat "tf" Nothing ((-1) % 4),
    Accidental FiveTwelfthsSharp "fts" Nothing (5 % 6),
    Accidental FiveTwelfthsFlat "ftf" Nothing ((-5) % 6),
    Accidental SevenTwelfthsSharp "sevents" Nothing (7 % 6),
    Accidental SevenTwelfthsFlat "seventf" Nothing ((-7) % 6),
    Accidental FiveSixthsSharp "fsixs" Nothing (5 % 3),
    Accidental FiveSixthsFlat "fsixf" Nothing ((-5) % 3),
    Accidental ElevenTwelfthsSharp "elevents" Nothing (11 % 6),
    Accidental ElevenTwelfthsFlat "elevenf" Nothing ((-11) % 6),
    Accidental TwoThirdsSharp "twothirds" Nothing (1 % 3),
    Accidental TwoThirdsFlat "twothirdf" Nothing ((-1) % 3)
  ]

accidentalNameToAbbreviation :: [(AccidentalName, String)]
accidentalNameToAbbreviation = [(_accName accidental, _accAbbreviation accidental) | accidental <- accidentals]

accidentalNameToSemitones :: [(AccidentalName, Rational)]
accidentalNameToSemitones = [(_accName accidental, _accSemitones accidental) | accidental <- accidentals]

accidentalAbbreviationToSemitones :: [(String, Rational)]
accidentalAbbreviationToSemitones = [(_accAbbreviation accidental, _accSemitones accidental) | accidental <- accidentals]

invertMapping :: (Eq a, Eq b) => [(a, b)] -> [(b, a)]
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

semitone :: Lens' Accidental Rational
semitone = lens _accSemitones (\acc newSemitones -> acc {_accSemitones = newSemitones, _accName = getName newSemitones, _accAbbreviation = getAbbreviation newSemitones})
  where
    getName :: Rational -> AccidentalName
    getName s = fromMaybe (CustomAccidental s) $ lookup s accidentalSemitonesToName

    getAbbreviation :: Rational -> String
    getAbbreviation s = fromMaybe "" $ lookup s accidentalSemitonesToAbbreviation

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
  initializeAccidental (AccidentalString abbreviation) =
    let name = fromMaybe (CustomAccidental 0) $ lookup abbreviation accidentalAbbreviationToName
        semitones = fromMaybe 0 $ lookup abbreviation accidentalAbbreviationToSemitones
     in Accidental {_accName = name, _accAbbreviation = abbreviation, _accSemitones = semitones, _accArrow = Nothing}

newtype AccidentalRational = AccidentalRational Rational

instance Initializable AccidentalRational where
  initializeAccidental (AccidentalRational semitones) =
    let name = fromMaybe (CustomAccidental semitones) $ lookup semitones accidentalSemitonesToName
        abbreviation = fromMaybe "" $ lookup semitones accidentalSemitonesToAbbreviation
     in Accidental {_accName = name, _accAbbreviation = abbreviation, _accSemitones = semitones, _accArrow = Nothing}

----

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

import Control.Lens

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