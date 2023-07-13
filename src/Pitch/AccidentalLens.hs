{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Accidental
  ( Accidental (..),
    nameLens,
    arrowLens,
    semitonesLens,
  )
where

import Control.Lens
import Data.Bifunctor (second)
import Data.Maybe (fromMaybe)
import Data.Ratio
import Text.Regex.TDFA ((=~))

data Accidental = Accidental
  { _name :: String,
    _arrow :: Maybe Bool,
    _semitones :: Rational
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

makeLenses ''Accidental

arrowLens :: Lens' Accidental (Maybe Bool)
arrowLens = arrow

semitonesLens :: Lens' Accidental Rational
semitonesLens = lens getter setter
  where
    getter :: Accidental -> Rational
    getter = _semitones

    setter :: Accidental -> Rational -> Accidental
    setter acc newSemitones = acc {_semitones = newSemitones, _name = getName newSemitones}

    getName :: Rational -> String
    getName semitones = fromMaybe (error "Invalid semitones value") $ lookup semitones accidentalSemitonesToName

nameLens :: Lens' Accidental String
nameLens = lens _name (\acc newName -> acc {_name = newName, _semitones = getSemitones newName})
  where
    getSemitones n = fromMaybe (error "Invalid name value") $ lookup n accidentalNameToSemitones

{-

-- Create an initial Accidental
acc :: Accidental
acc = Accidental {_name = "flat", _arrow = Nothing, _semitones = -1}

-- Get the current name using the lens
currentName :: String
currentName = acc ^. nameLens

-- Output: "flat"

-- Update the name using the lens
updatedAcc :: Accidental
updatedAcc = acc & nameLens .~ "sharp"

-- Accidental {_name = "sharp", _arrow = Nothing, _semitones = 1 % 1}

updatedAcc2 :: Accidental
updatedAcc2 = acc & nameLens .~ "natural"

-- Accidental {_name = "natural", _arrow = Nothing, _semitones = 0 % 1}

-- Update the name and semitones using the lens
updatedAcc2 :: Accidental
updatedAcc2 = acc & nameLens .~ "natural" & semitonesLens .~ 0

-- Output: Accidental {_name = "natural", _arrow = Nothing, _semitones = 0}

-- Create an initial Accidental
acc :: Accidental
acc = Accidental {_name = "flat", _arrow = Nothing, _semitones = -1}

-- Get the current semitones using the lens
currentSemitones :: Rational
currentSemitones = acc ^. semitonesLens

-- Output: (-1) % 1

-- Update the semitones using the lens
updatedAcc :: Accidental
updatedAcc = acc & semitonesLens .~ 1

-- Accidental {_name = "sharp", _arrow = Nothing, _semitones = 1 % 1}

-- Update the semitones and name using the lens
updatedAcc2 :: Accidental
updatedAcc2 = acc & semitonesLens .~ 0 & nameLens .~ "natural"

-- Accidental {_name = "natural", _arrow = Nothing, _semitones = 0 % 1}

-}