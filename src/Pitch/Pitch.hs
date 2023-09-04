{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Pitch.Pitch
  ( NoteName (..),
    Pitch (..),
    HasPitch (..),
    (=~),
    Accidental (..),
    AccidentalName (..),
    defaultAccidental,
    createPitch,
    PitchClass (..),
    createPitchClass,
    accidentals,
    noteNameToRational,
    pitchClassVal,
    pitchVal,
    Alterable (..),
    Octave (..),
    fromOctave,
    ToLilyString (..),
  )
where

import Control.Lens
-- import Control.Lens.TH
-- import Data.Fixed (mod')

import Data.Kind (Type)

import Data.List ( minimumBy )
import qualified Data.Map as Map
import qualified Data.Map.Strict as MapS
import Data.Maybe ( fromMaybe )
import Data.Ord ()
import Data.Ratio
import Pitch.Accidental
--import Test.QuickCheck

data NoteName = C | D | E | F | G | A | B deriving (Eq, Enum, Ord, Show)

type PitchVal = Rational

data PitchClass = PitchClass
  { _noteName :: NoteName,
    _accidental :: Pitch.Accidental.Accidental
  }
  deriving (Show)


makeLenses ''PitchClass

instance Ord PitchClass where
  compare :: PitchClass -> PitchClass -> Ordering
  compare pc1 pc2 = compare (pitchClassVal pc1) (pitchClassVal pc2)

instance Eq PitchClass where
  pc1 == pc2 = pitchClassVal pc1 == pitchClassVal pc2

pitchClassVal :: PitchClass -> Pitch.Accidental.Rq
pitchClassVal pc = base + acVal
  where
    base = fromMaybe (0 % 1) (lookup (_noteName pc) noteNameToRational) :: Pitch.Accidental.Rq
    acVal = _accSemitones (_accidental pc) :: Pitch.Accidental.Rq

noteNameToRational :: [(NoteName, Pitch.Accidental.Rq)]
noteNameToRational =
  [ (C, 0 % 1),
    (D, 2 % 1),
    (E, 4 % 1),
    (F, 5 % 1),
    (G, 7 % 1),
    (A, 9 % 1),
    (B, 11 % 1)
  ]

class Pitchable a where
  makePitch :: NoteName -> Pitch.Accidental.Accidental -> Octave -> a

instance Pitchable Pitch where
  makePitch n acc oct = Pitch {_pitchClass = PitchClass {_noteName = n, _accidental = acc}, _octave = oct} -- Using a default octave, modify as necessary

class PitchClassable a where
  createPitchClass :: NoteName -> Pitch.Accidental.Accidental -> a

instance PitchClassable PitchClass where
  createPitchClass n acc = PitchClass {_noteName = n, _accidental = acc}

instance PitchClassable Pitch where
  createPitchClass n acc = Pitch {_pitchClass = PitchClass {_noteName = n, _accidental = acc}, _octave = 4} -- Using a default octave, modify as necessary

class Alterable a where
  sharpen :: a -> a
  flatten :: a -> a
  quarterSharpen :: a -> a
  quarterFlatten :: a -> a

instance Alterable Accidental where
  sharpen = over semitone (+ (1 % 1))
  flatten = over semitone (subtract (1 % 1))
  quarterSharpen = over semitone (+ (1 % 2))
  quarterFlatten = over semitone (subtract (1 % 2))

instance Alterable PitchClass where
  sharpen pc = pc {_accidental = sharpen (_accidental pc)}
  flatten pc = pc {_accidental = flatten (_accidental pc)}
  quarterSharpen pc = pc {_accidental = quarterSharpen (_accidental pc)}
  quarterFlatten pc = pc {_accidental = quarterFlatten (_accidental pc)}

instance Alterable Pitch where
  sharpen p = p {_pitchClass = sharpen (_pitchClass p)}
  flatten p = p {_pitchClass = flatten (_pitchClass p)}
  quarterSharpen p = p {_pitchClass = quarterSharpen (_pitchClass p)}
  quarterFlatten p = p {_pitchClass = quarterFlatten (_pitchClass p)}

noteNameToRational'' :: [(NoteName, Rational)]
noteNameToRational'' =
  [ (C, 0 % 1),
    (D, 2 % 1),
    (E, 4 % 1),
    (F, 5 % 1),
    (G, 7 % 1),
    (A, 9 % 1),
    (B, 11 % 1)
  ]

--

noteNameToRational' :: Map.Map NoteName Pitch.Accidental.Rq
noteNameToRational' =
  Map.fromList
    [ (C, 0 % 1),
      (D, 2 % 1),
      (E, 4 % 1),
      (F, 5 % 1),
      (G, 7 % 1),
      (A, 9 % 1),
      (B, 11 % 1)
    ]

pitchClassVal' :: PitchClass -> Pitch.Accidental.Rq
pitchClassVal' pitchClass = base + ac
  where
    base = fromMaybe (error "NoteName not found") $ Map.lookup nn noteNameToRational'
    nn = _noteName pitchClass
    ac = (_accidental pitchClass) ^. semitone

{-
base = case lookup nn noteNameToRational of
  Just val -> val
  Nothing -> 0 % 1
 -}

newtype Octave = Octave Int
  deriving (Eq, Show)

instance Num Octave where
  (Octave n1) + (Octave n2) = Octave (n1 + n2)
  (Octave n1) - (Octave n2) = Octave (n1 - n2)
  (Octave n1) * (Octave n2) = error "Multiplication not defined"
  abs (Octave n) = Octave (abs n)
  signum (Octave n) = Octave (signum n)
  fromInteger n = Octave (fromInteger n)

-- Utility functions
toOctave :: Int -> Octave
toOctave = Octave

fromOctave :: Octave -> Int
fromOctave (Octave n) = n


-- mkOct

data Pitch = Pitch
  { _pitchClass :: PitchClass,
    _octave :: Octave
  }
  deriving (Show)

makeLenses ''Pitch

instance Ord Pitch where
  compare :: Pitch -> Pitch -> Ordering
  compare p1 p2 = compare (pitchVal''' p1) (pitchVal''' p2)

class AffineSpace p where
  type Diff p :: Type
  
  (.+^) :: p -> Diff p -> p
  (.-.) :: p -> p -> Diff p


instance AffineSpace Pitch where
  type Diff Pitch = Interval
  
  p .+^ (Interval interval) = pitchValToSelectedPitch $ (pitchVal''' p + interval) 
  p1 .-. p2 = Interval $ (pitchVal''' p1 - pitchVal''' p2) 


newtype Interval = Interval Rational deriving (Show, Eq, Num)


class HasPitch a where
  pitch :: a -> Pitch

instance HasPitch Pitch where
  pitch = id


(=~) :: (HasPitch a) => a -> a -> Bool
p1 =~ p2 = (pitchVal''' (pitch p1)) == (pitchVal''' (pitch p2))

instance Eq Pitch where
  (==) :: Pitch -> Pitch -> Bool
  p1 == p2 = (_pitchClass p1 == _pitchClass p2) && (_octave p1 == _octave p2)


createPitch :: PitchClass -> Int -> Pitch
createPitch pc oct = Pitch pc (Octave oct)

mkP :: PitchClass -> Int -> Pitch
mkP pc oct = Pitch pc (Octave oct)

mkPitch :: PitchClass -> Int -> Pitch
mkPitch = createPitch

isValidPitchClass :: PitchClass -> Bool
isValidPitchClass pc = (_noteName pc) `elem` [C, D, E, F, G, A, B]

-- isValidOctave :: Int -> Bool
-- isValidOctave oct = oct >= -1 && oct <= 10

-- octaveVal :: Octave -> Int
-- octaveVal (Octave oct) = (oct - 4) * 12

class HasOctave a where
  getOctave :: a -> Octave
  setOctave :: Octave -> a -> a

octaveUp :: (HasOctave a, Num Octave) => a -> a
octaveUp pitch = setOctave (getOctave pitch + 1) pitch

octaveDown :: (HasOctave a, Num Octave) => a -> a
octaveDown pitch = setOctave (getOctave pitch - 1) pitch

instance HasOctave Pitch where
  getOctave = _octave
  setOctave octave pitch = pitch {_octave = octave}

isValidOctave :: Int -> Bool
isValidOctave oct = oct >= -1 && oct <= 10

octaveVal :: Octave -> Int
octaveVal (Octave oct) = (oct - 4) * 12

pitchVal''' :: Pitch -> Rational
pitchVal''' pitch_ = pcVal + fromIntegral octVal
  where
    pcVal = pitchClassVal (pitch_ ^. pitchClass)
    octVal = octaveVal (pitch_ ^. octave)

pitchVal :: Pitch -> PitchVal
pitchVal pitch_ = pcVal + fromIntegral octVal
  where
    pcVal = pitchClassVal (pitch_ ^. pitchClass)
    octVal = octaveVal (pitch_ ^. octave)

{-
>>> gts6 = createPitch' G twelfthSharp 6
>>> pitchVal' gts6

 -}

createPitch' :: NoteName -> Pitch.Accidental.Accidental -> Int -> Pitch
createPitch' pc acc = createPitch (createPitchClass pc acc)

-- mkPC
mkPC :: NoteName -> Pitch.Accidental.Accidental -> PitchClass
mkPC n acc = PitchClass {_noteName = n, _accidental = acc}

allPitchClasses :: [PitchClass]
allPitchClasses = [mkPC note acc | note <- [C .. B], acc <- accidentals]

allPitches :: [Pitch]
allPitches = [createPitch pc oct | pc <- allPitchClasses, oct <- [1 .. 9]]

-- Define your lookup table
pitchValToPitchMap :: MapS.Map PitchVal Pitch
pitchValToPitchMap = MapS.fromList [(pitchVal p, p) | p <- allPitches]

-- Function to convert from PitchVal to Pitch using the lookup table
pitchValtoPitch :: PitchVal -> Maybe Pitch
pitchValtoPitch pv = MapS.lookup pv pitchValToPitchMap

pitchValToPitchesMap :: MapS.Map PitchVal [Pitch]
pitchValToPitchesMap = MapS.fromListWith (++) [(pitchVal p, [p]) | p <- allPitches]

-- Function to convert from PitchVal to list of corresponding pitches
pitchValToPitches :: PitchVal -> [Pitch]
pitchValToPitches pv = MapS.findWithDefault [] pv pitchValToPitchesMap

-- Function to check if an Accidental is natural
isNatural :: Accidental -> Bool
isNatural natural = True
isNatural _ = False

-- Function to compare two pitches based on the rules
comparePitches :: Pitch -> Pitch -> Ordering
comparePitches p1 p2
  | isNatural (acc1 p1) && isNatural (acc2 p2) = compare (absSemitones1 p1) (absSemitones2 p2)
  | isNatural (acc1 p1) = LT
  | isNatural (acc2 p2) = GT
  | otherwise = compare (absSemitones1 p1) (absSemitones2 p2)
  where
    acc1 = _accidental . _pitchClass
    acc2 = _accidental . _pitchClass
    absSemitones1 = abs . _accSemitones . acc1
    absSemitones2 = abs . _accSemitones . acc2

-- Function to determine the priority of an accidental
accidentalPriority :: Accidental -> Int
accidentalPriority natural = 0
accidentalPriority sharp = 1
accidentalPriority flat = 2
accidentalPriority quarterSharp = 3
accidentalPriority quarterFlat = 4
accidentalPriority _ = 5

-- Function to compare two pitches based on the rules
comparePitches' :: Pitch -> Pitch -> Ordering
comparePitches' p1 p2
  | accPriority1 /= accPriority2 = compare accPriority1 accPriority2
  | otherwise = compare (absSemitones1 p1) (absSemitones1 p2)
  where
    accPriority1 = accidentalPriority $ _accidental $ _pitchClass p1
    accPriority2 = accidentalPriority $ _accidental $ _pitchClass p2
    absSemitones1 = abs . _accSemitones . _accidental . _pitchClass
    absSemitones2 = abs . _accSemitones . _accidental . _pitchClass

-- Function to select a pitch based on the rules
selectPitch' :: [Pitch] -> Pitch
selectPitch' = minimumBy comparePitches

-- Function to select a pitch based on the rules
selectPitch :: [Pitch] -> Pitch
selectPitch = minimumBy comparePitches

pitchValToSelectedPitch :: PitchVal -> Pitch
pitchValToSelectedPitch pv = selectPitch $ pitchValToPitches pv

pitchValToSelectedPitch' :: PitchVal -> Pitch
pitchValToSelectedPitch' pv = selectPitch' $ pitchValToPitches pv

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

-- pitchValtoPitch :: PitchVal -> Pitch
-- pitchValtoPitch pv =

{- class HasPitch a where
  pitchSemitones :: Lens' a Rational

instance HasPitch Pitch where
  pitchSemitones = lens getter setter
    where
      getter pitch = pitch ^. pitchClass . accidental . semitone
      setter pitch newSemitones = pitch & pitchClass . accidental %~ updateAccidental newSemitones
        where
          updateAccidental semis acc = acc & semitones .~ semisa

instance HasPitch PitchClass where
  pitchSemitones = accidental . semitone
 -}

-- pitchToLilyPond :: Pitch -> String
-- pitchToLilyPond pitch = noteNameStr ++ accidentalStr ++ octaveStr
--   where
--     noteNameStr = case _noteName (_pitchClass pitch) of
--       C -> "c"
--       D -> "d"
--       -- ... and so on for other note names
--     accidentalStr = case _accidental (_pitchClass pitch) of
--       Accidental Natural _ _ _ -> ""
--       Accidental Sharp _ _ _ -> "is"
--       Accidental Flat _ _ _ -> "es"
--       -- ... handle other accidentals here
--     octaveStr = replicate (fromIntegral (_octave pitch) - 4) '\''

-- ------------------- pitchToLilyPond :: Pitch -> String ------------------- --

pitchToLily pitch = noteNameStr ++ accidentalStr ++ octaveStr
  where
    noteNameStr = case _noteName (_pitchClass pitch) of
      C -> "c"
      D -> "d"
      E -> "e"
      F -> "f"
      G -> "g"
      A -> "a"
      B -> "b"
    accidentalStr = case _accidental (_pitchClass pitch) of
      Accidental Natural _ _ _ -> ""
      Accidental Sharp _ _ _ -> "is"
      Accidental Flat _ _ _ -> "es"
      Accidental QuarterSharp _ _ _ -> "ih"
      Accidental QuarterFlat _ _ _ -> "eh"
      _ -> "todo"
    octaveOffset = (fromOctave $ _octave pitch) - 4 :: Int
    octaveStr =
      if octaveOffset > 0
        then replicate octaveOffset '\''
        else replicate (-octaveOffset) ','

class ToLilyString a where
  toLilyString :: a -> String

instance ToLilyString NoteName where
  toLilyString = \case
    C -> "c"
    D -> "d"
    E -> "e"
    F -> "f"
    G -> "g"
    A -> "a"
    B -> "b"

instance ToLilyString Octave where
  toLilyString (Octave n) =
    let octaveOffset = n - 4
     in if octaveOffset > 0
          then replicate octaveOffset '\''
          else replicate (-octaveOffset) ','

instance ToLilyString Accidental where
  toLilyString = \case
    Accidental Natural _ _ _ -> ""
    Accidental Sharp _ _ _ -> "is"
    Accidental Flat _ _ _ -> "es"
    Accidental QuarterSharp _ _ _ -> "ih"
    Accidental QuarterFlat _ _ _ -> "eh"
    Accidental ThreeQuartersSharp _ _ _ -> "isih"
    Accidental ThreeQuartersFlat _ _ _ -> "eseh"
    _ -> "todo"

instance ToLilyString Pitch where
  toLilyString = \p -> (toLilyString $ _noteName $ _pitchClass p) ++ (toLilyString $ _accidental $ _pitchClass p) ++ (toLilyString $ _octave p)
