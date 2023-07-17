{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Pitch.Pitch
  ( NoteName,
    Pitch (..),
    HasPitch (..),
    (=~),
  )
where

import Control.Lens
import Data.Fixed (mod')
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Pitch.Accidental
import Control.Monad (guard)


data NoteName = C | D | E | F | G | A | B deriving (Eq, Show)

data PitchClass = PitchClass
  { _noteName :: NoteName,
    _accidental :: Accidental
  }
  deriving (Eq, Show)

makeLenses ''PitchClass

createPitchClass :: NoteName -> Accidental -> PitchClass
createPitchClass name acc = PitchClass {_noteName = name, _accidental = acc}

newtype PitchVal = PitchVal Rational
  deriving (Eq, Show, Num)

noteNameToRational :: [(NoteName, Rational)]
noteNameToRational =
  [ (C, 0 % 1),
    (D, 2 % 1),
    (E, 4 % 1),
    (F, 5 % 1),
    (G, 7 % 1),
    (A, 9 % 1),
    (B, 11 % 1)
  ]

pitchClassVal :: PitchClass -> Rational
pitchClassVal pitchClass = base + ac
  where
    base = fromMaybe (0 % 1) $ lookup nn noteNameToRational
    nn = pitchClass ^. noteName
    ac = pitchClass ^. (accidental . semitone)

newtype Octave = Octave Int
  deriving (Eq, Show, Num)

createOctave :: Int -> Maybe Octave
createOctave oct
  | oct >= minOctave && oct <= maxOctave = Just (Octave oct)
  | otherwise = Nothing
  where
    minOctave = -1
    maxOctave = 10

data Pitch = Pitch
  { _pitchClass :: PitchClass,
    _octave :: Octave
  }
  deriving (Eq, Show)

makeLenses ''Pitch

class HasPitch a where
  pitch :: a -> PitchVal

instance HasPitch Pitch where
  pitch = pitchVal

(=~) :: (HasPitch a) => a -> a -> Bool
a =~ b = pitch a == pitch b

-- createPitch :: PitchClass -> Int -> Maybe Pitch
-- createPitch pc oct = do
--   guard (isValidPitchClass pc && isValidOctave oct)
--   return $ Pitch pc (Octave oct)

createPitch :: PitchClass -> Int -> Pitch
createPitch pc oct
  | isValidPitchClass pc && isValidOctave oct = Pitch pc (Octave oct)
  | otherwise = error "Invalid pitch"


isValidPitchClass :: PitchClass -> Bool
isValidPitchClass pc = pc ^. noteName `elem` [C, D, E, F, G, A, B]

isValidOctave :: Int -> Bool
isValidOctave oct = oct >= -1 && oct <= 10

octaveVal :: Octave -> Int
octaveVal (Octave oct) = (oct - 4) * 12

pitchVal :: Pitch -> PitchVal
pitchVal pitch = PitchVal (pcVal + fromIntegral octVal)
  where
    pcVal = pitchClassVal (pitch ^. pitchClass)
    octVal = octaveVal (pitch ^. octave)

createPitch' :: NoteName -> Accidental -> Int -> Pitch
createPitch' pc acc oct = createPitch (createPitchClass pc acc) oct




-- changePitchSemitones :: Rational -> Pitch -> Pitch
-- changePitchSemitones semis pitch =
--   let pc = pitch ^. pitchClass
--       oct = pitch ^. octave
--       newPC = changePitchClassSemitones semis pc
--       newOct = changeOctaveSemitones semis oct
--    in createPitch (fromMaybe pc newPC) (fromMaybe oct newOct)

-- changePitchClassSemitones :: Rational -> PitchClass -> Maybe PitchClass
-- changePitchClassSemitones semis pc = do
--   let currentSemitones = pc ^. accidental . semitone
--       newSemitones = currentSemitones + semis
--       acc = pc ^. accidental
--       newAcc = acc & semitone .~ newSemitones
--    in Just $ pc & accidental .~ newAcc

-- changeOctaveSemitones :: Rational -> Octave -> Maybe Octave
-- changeOctaveSemitones semis oct = do
--   let currentOctave = oct
--       newOctave = currentOctave + fromIntegral (truncate semis `div` 12)
--    in createOctave newOctave

--

{-
createPitch :: PitchClass -> Int -> Maybe Pitch
createPitch pc o = do
  oct <- createOctave o
  return $ Pitch pc oct

 -}

{-

c = createPitchClass C natural

pitchClassVal c
-- 0 % 1

fs = createPitchClass F sharp

pitchClassVal fs
-- 6 % 1

fqs = createPitchClass F quarterSharp

pitchClassVal fqs
-- 11 % 2

middleC = createPitch c 4 
fSharp5 = createPitch fs 5
-- Pitch {_pitchClass = PitchClass {_noteName = F, _accidental = Accidental {_accName = Sharp, _accAbbreviation = "s", _accArrow = Nothing, _accSemitones = 1 % 1}}, _octave = Octave 5}

pitchVal middleC
-- 0 % 1

pitchVal fSharp5
-- 18 % 1
 -}



{- 
pitch middleC

c = createPitchClass C natural

fSharp = createPitchClass F sharp

middleC = createPitch' C natural 4

middleC ^.  semitone

updatedPitchClass :: PitchClass
updatedPitchClass = c & semitone .~ 4.5

updatedPitchClass :: PitchClass
updatedPitchClass = c & accidental . semitone .~ 4.5


updatedPitch :: Pitch 
updatedPitch = middleC @ semitone .~ 4.5

-- Accessing fields using lenses
cName = c ^. noteName 
-- C

cAcc = c ^. accidental
-- Accidental {_accName = Natural, _accAbbreviation = "", _accArrow = Nothing, _accSemitones = 0 % 1}

fSharpAccSemitones = fSharp ^. accidental ^. semitone
-- 1 % 1

middleCOctave = middleC ^. octave
-- Octave 4

-- Modifying fields using lenses
modifiedC = c & noteName .~ D
-- PitchClass {_noteName = D, _accidental = Accidental {_accName = Natural, _accAbbreviation = "", _accArrow = Nothing, _accSemitones = 0 % 1}}

modifiedC ^. noteName
-- D

modifiedFS = fSharp & accidental . semitone .~ 2
-- PitchClass {_noteName = F, _accidental = Accidental {_accName = DoubleSharp, _accAbbreviation = "ss", _accArrow = Nothing, _accSemitones = 2 % 1}}

modifiedMiddleC = middleC & octave %~ (+ 1)
-- Pitch {_pitchClass = PitchClass {_noteName = C, _accidental = Accidental {_accName = Natural, _accAbbreviation = "", _accArrow = Nothing, _accSemitones = 0 % 1}}, _octave = Octave 5}

-- Composing lenses for nested access
modifiedMiddleCName = middleC & pitchClass . noteName .~ D
-- Pitch {_pitchClass = PitchClass {_noteName = D, _accidental = Accidental {_accName = Natural, _accAbbreviation = "", _accArrow = Nothing, _accSemitones = 0 % 1}}, _octave = Octave 4}

modifiedMiddleCAccSemitones = middleC & pitchClass . accidental . semitone %~ (+ 1.5)
-- Pitch {_pitchClass = PitchClass {_noteName = C, _accidental = Accidental {_accName = ThreeQuartersSharp, _accAbbreviation = "tqs", _accArrow = Nothing, _accSemitones = 3 % 2}}, _octave = Octave 4}

-- Chaining lenses with the (.~) operator
modifiedMiddleCNameAcc = middleC & pitchClass . noteName .~ D & pitchClass . accidental .~ flat
-- Pitch {_pitchClass = PitchClass {_noteName = D, _accidental = Accidental {_accName = Flat, _accAbbreviation = "f", _accArrow = Nothing, _accSemitones = (-1) % 1}}, _octave = Octave 4}

 -}






{- class HasPitch a where
  pitchSemitones :: Lens' a Rational

instance HasPitch Pitch where
  pitchSemitones = lens getter setter
    where
      getter pitch = pitch ^. pitchClass . accidental . semitone
      setter pitch newSemitones = pitch & pitchClass . accidental %~ updateAccidental newSemitones
        where
          updateAccidental semis acc = acc & semitones .~ semis

instance HasPitch PitchClass where
  pitchSemitones = accidental . semitone
 -}


-- instance HasPitch Pitch where
--   pitchSemitones = lens pitchVal (\pitch semis -> pitch {_pitchClass = (_pitchClass pitch) {_accidental = (_accidental (_pitchClass pitch)) {_accSemitones = semis}}})

-- instance HasPitch PitchClass where
--   pitchSemitones = accidental . accSemitones

