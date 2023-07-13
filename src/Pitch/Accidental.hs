{-# LANGUAGE InstanceSigs #-}

module Accidental
  ( Accidental (..),
    accidentalNameToAbbreviation,
    accidentalNameToSemitones,
    initializeAccidental,
    accidentalSemitonesToName,
    getAccidentalNameForSemitones,
    setArrow,
    setName,
    setSemitones,
  )
where

import Data.Bifunctor (second)
import Data.Maybe (fromMaybe)
import Data.Ratio
import Text.Regex.TDFA ((=~))

data Accidental = Accidental
  { name :: String,
    arrow :: Maybe Bool,
    semitones :: Rational
  }
  deriving (Show)

accidentalNameToAbbreviation :: [(String, String)]
accidentalNameToAbbreviation =
  [ ("double flat", "ff"),
    ("three-quarters flat", "tqf"),
    ("flat", "f"),
    ("natural", ""),
    ("quarter sharp", "qs"),
    ("sharp", "s"),
    ("three-quarters sharp", "tqs"),
    ("double sharp", "ss")
  ]

accidentalNameToSemitones :: [(String, Rational)]
accidentalNameToSemitones =
  [ ("double flat", -2),
    ("three-quarters flat", -(3 % 2)),
    ("flat", -1),
    ("quarter flat", -(1 % 2)),
    ("natural", 0),
    ("quarter sharp", 1 % 2),
    ("sharp", 1),
    ("three-quarters sharp", 3 % 2),
    ("double sharp", 2)
  ]

accidentalAbbreviationToSemitones :: [(String, Rational)]
accidentalAbbreviationToSemitones =
  [ ("ff", -2),
    ("tqf", -(3 % 2)),
    ("f", -1),
    ("qf", -(1 % 2)),
    ("", 0),
    ("qs", 1 % 2),
    ("s", 1),
    ("tqs", 3 % 2),
    ("ss", 2)
  ]

invertMapping :: (Eq a, Eq b) => [(a, b)] -> [(b, a)]
invertMapping = map (\(a, b) -> (b, a))

accidentalAbreviationToName :: [(String, String)]
accidentalAbreviationToName = invertMapping accidentalNameToAbbreviation

accidentalSemitonesToAbbreviation :: [(Rational, String)]
accidentalSemitonesToAbbreviation = invertMapping accidentalAbbreviationToSemitones

accidentalSemitonesToName :: [(Rational, String)]
accidentalSemitonesToName = invertMapping accidentalNameToSemitones

-- accidentalSemitonesToName :: [(Rational, String)]
-- accidentalSemitonesToName =
--   map (second convertAbbreviationToName) accidentalSemitonesToAbbreviation
--   where
--     convertAbbreviationToName abbrev = lookup abbrev accidentalAbreviationToName

-- accidentalSemitonesToName :: [(Rational, String)]
-- accidentalSemitonesToName =
--   map (second convertAbbreviationToName) accidentalAbbreviationToSemitones
--   where
--     convertAbbreviationToName :: Rational -> String
--     convertAbbreviationToName semitones = fromMaybe (error "Invalid semitones value") $ lookup semitones (map swap accidentalNameToAbbreviation)

-- swap :: (a, b) -> (b, a)
-- swap (x, y) = (y, x)

getAccidentalNameForSemitones :: Rational -> String
getAccidentalNameForSemitones semitones =
  fromMaybe "Invalid semitones value" $ lookup semitones accidentalSemitonesToName

instance Semigroup Accidental where
  (<>) a1 a2 =
    Accidental
      { name = getName (semitones a1 + semitones a2),
        arrow = Nothing,
        semitones = semitones a1 + semitones a2
      }
    where
      getName semitones = case lookup semitones accidentalSemitonesToName of
        Just name -> name
        Nothing -> show (numerator semitones) ++ "/" ++ show (denominator semitones)

initializeAccidental :: String -> Accidental
initializeAccidental str =
  case lookup str accidentalNameToAbbreviation of
    Just abbreviation ->
      let semitones = fromMaybe (error "Invalid accidental abbreviation") $ lookup abbreviation accidentalAbbreviationToSemitones
       in Accidental {name = str, arrow = Nothing, semitones = semitones}
    Nothing ->
      case lookup str accidentalAbbreviationToSemitones of
        Just semitones ->
          let name = fromMaybe (error "Invalid accidental abbreviation") $ lookup str accidentalAbreviationToName
           in Accidental {name = name, arrow = Nothing, semitones = semitones}
        Nothing -> error "Invalid accidental name or abbreviation"

getName :: Accidental -> String
getName = name

getArrow :: Accidental -> Maybe Bool
getArrow = arrow

getSemitones :: Accidental -> Rational
getSemitones = semitones

setArrow :: Maybe Bool -> Accidental -> Accidental
setArrow newArrow accidental = accidental {arrow = newArrow}

setName :: String -> Accidental -> Accidental
setName newName accidental =
  let newSemitones = semitones (initializeAccidental newName)
   in accidental {name = newName, semitones = newSemitones}

setSemitones :: Rational -> Accidental -> Accidental
setSemitones newSemitones accidental =
  let newName = fromMaybe (error "Invalid semitones value") $ lookup newSemitones accidentalSemitonesToName
   in accidental {name = newName, semitones = newSemitones}

{-

> initializeAccidental "f"
Accidental {name = "flat", arrow = Nothing, semitones = (-1) % 1}

> initializeAccidental "s"
Accidental {name = "sharp", arrow = Nothing, semitones = 1 % 1}

> initializeAccidental "quarter sharp"
Accidental {name = "quarter sharp", arrow = Nothing, semitones = 1 % 2}

acc = initializeAccidental "sharp"
acc
getName acc
acc2 = setName "natural" acc
acc2

semitones acc2 == (0%1)

acc3 = setSemitones (1%2) acc
acc3
-- Accidental {name = "quarter sharp", arrow = Nothing, semitones = 1 % 2}

-}
