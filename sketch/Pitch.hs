{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Data.Fixed
import Data.Function (on)
import Data.List (minimumBy)
import Data.Ratio
import Data.Kind (Type)

data NoteName = C | D | E | F | G | A | B deriving (Show, Eq, Enum, Bounded, Ord)
data PitchSpace = PitchSpace {
    _pitch :: Rational,
    _note :: Note,
    _octave :: Int
} deriving (Eq, Show)

data Note = Note {
    _noteName :: NoteName,
    _accidental :: Accidental
} deriving (Eq, Show)

data Accidental = Accidental {
    _accidentalName :: String,
    _shift :: Rational
} deriving (Eq, Show)

makeLenses ''PitchSpace
makeLenses ''Note
makeLenses ''Accidental
makeFields ''PitchSpace  -- generates lenses for nested fields 

-- update the Note based on the modified pitch
updateNote :: PitchSpace -> PitchSpace
updateNote ps = ps & note .~ Note noteName acc where
    (noteName, acc) = getClosestNoteAndAccidental (ps ^. pitch)

class AffineSpace p where
  type Diff p :: Type
  (.+^) :: p -> Diff p -> p
  (.-.) :: p -> p -> Diff p

class Num a => VectorSpace a where
  type Scalar a :: Type
  (*^) :: Scalar a -> a -> a

rationalModulo12 :: Rational -> Rational
rationalModulo12 r = r `mod'` 12

newtype Interval = Interval Rational deriving (Show, Eq, Num)

instance VectorSpace Interval where
  type Scalar Interval = Rational
  (*^) s (Interval i) = Interval (s * i)


instance AffineSpace PitchSpace where
  type Diff PitchSpace = Interval

  p@(PitchSpace r note octave) .+^ (Interval i) =
    let r' = r + i
        (noteName, accidental) = getClosestNoteAndAccidental r'
        octave' = rationalToOctave r'
    in PitchSpace r' (Note noteName accidental) octave'
    
  (PitchSpace r1 _ _) .-. (PitchSpace r2 _ _) = Interval (r1 - r2)

  

type RationalNote = (NoteName, Rational)

rationalToOctave :: Rational -> Int
rationalToOctave r = floor (r / 12)

noteNameToRational :: [RationalNote]
noteNameToRational =
  [ (C, 0 % 1),
    (D, 2 % 1),
    (E, 4 % 1),
    (F, 5 % 1),
    (G, 7 % 1),
    (A, 9 % 1),
    (B, 11 % 1)
  ]

type RationalAccidental = (String, Rational)

accidentals :: [RationalAccidental]
accidentals =
  [ ("f", (-1) % 1),
    ("", 0 % 1),
    ("s", 1 % 1),
    ("qf", (-1) % 2),
    ("qs", 1 % 2),
    ("tqf", (-3) % 2),
    ("tqs", 3 % 2),
    ("ss", 2 % 1),
    ("ff", (-2) % 1),
    ("rs", 2 % 3),
    ("rf", (-2) % 3),
    ("xs", 1 % 3),
    ("xf", (-1) % 3),
    ("ts", 1 % 6),
    ("tf", (-1) % 6),
    ("es", 1 % 4),
    ("ef", (-1) % 4),
    ("fts", 5 % 6),
    ("ftf", (-5) % 6),
    ("sts", 7 % 6),
    ("stf", (-7) % 6),
    ("fxs", 5 % 3),
    ("fxf", (-5) % 3),
    ("ets", 11 % 6),
    ("etf", (-11) % 6),
    ("trf", (-4) % 3),
    ("trs", 4 % 3),
    ("tes", 3 % 4),
    ("tef", (-3) % 4)
  ]



findClosestAccidental :: Rational -> Accidental
findClosestAccidental r =
  let (name, shift) = minimumBy (compare `on` (abs . (r -) . snd)) accidentals
  in Accidental name shift

findClosestNote :: Rational -> Rational
findClosestNote r =
  snd $ minimumBy (compare `on` (abs . (r -) . snd)) noteNameToRational


calculateDifference :: Rational -> Rational
calculateDifference r = r - findClosestNote r


getClosestNoteAndAccidental :: Rational -> (NoteName, Accidental)
getClosestNoteAndAccidental r =
  let closestNoteRational = findClosestNote mod12
      closestNote = fst $ head $ filter ((==closestNoteRational) . snd) noteNameToRational
      closestAccidental = findClosestAccidental (calculateDifference mod12)
      mod12 = rationalModulo12 r
  in (closestNote, closestAccidental)


rationalToPitchSpace :: Rational -> PitchSpace
rationalToPitchSpace r =
  let (closestNote, closestAccidental) = getClosestNoteAndAccidental r
      octave = rationalToOctave r
   in PitchSpace r (Note closestNote closestAccidental) octave





{-

 p1 = rationalToPitchSpace (1)
 -- PitchSpace {pitch = 1 % 1, note = Note {noteName = C, accidental = Accidental {accidentalName = "s", shift = 1 % 1}}, octave = 0}

 p2 = rationalToPitchSpace (5.5)
 -- PitchSpace {pitch = 11 % 2, note = Note {noteName = F, accidental = Accidental {accidentalName = "qs", shift = 1 % 2}}, octave = 0}


i = p2 .-. p1 
-- Interval (9 % 2)

p1 .+^ i
-- PitchSpace {pitch = 11 % 2, note = Note {noteName = F, accidental = Accidental {accidentalName = "qs", shift = 1 % 2}}, octave = 0}

p2 .+^ i
-- PitchSpace {pitch = 10 % 1, note = Note {noteName = A, accidental = Accidental {accidentalName = "s", shift = 1 % 1}}, octave = 0}

p2 .+^ (-i)
-- PitchSpace {pitch = 1 % 1, note = Note {noteName = C, accidental = Accidental {accidentalName = "s", shift = 1 % 1}}, octave = 0}

(1%2) *^ i
-- Interval (9 % 4)

(2) *^ i
-- Interval (9 % 1)

  -}




  {- 
  
  import Data.Ratio

p1 :: PitchSpace
p1 = rationalToPitchSpace (1 % 1)

p2 :: PitchSpace
p2 = rationalToPitchSpace (5.5)

-- Accessing fields using lenses
p1Pitch :: Rational
p1Pitch = p1 ^. pitch  -- Equivalent to 'pitch p1'

p2NoteName :: NoteName
p2NoteName = p2 ^. note . noteName  -- Equivalent to 'noteName (note p2)'

p1Octave :: Int
p1Octave = p1 ^. octave  -- Equivalent to 'octave p1'

-- Modifying fields using lenses
modifiedP1 :: PitchSpace
modifiedP1 = p1 & pitch %~ (+ (1 % 2))  -- Add 1/2 to the pitch of p1

modifiedP2 :: PitchSpace
modifiedP2 = p2 & (note . accidental . shift) %~ (* 2)  -- Multiply the shift of the accidental of the note in p2 by 2

-- Combining lenses
accidentalNameInP2 :: String
accidentalNameInP2 = p2 ^. note . accidental . accidentalName  -- Equivalent to 'accidentalName (note p2)'

-- Modifying fields conditionally using lenses
adjustedPitchSpace :: Rational -> PitchSpace -> PitchSpace
adjustedPitchSpace threshold ps =
    ps & pitch %~ (\p -> if p > threshold then p - 1 else p)  -- If pitch > threshold, subtract 1 from the pitch

adjustedP1 :: PitchSpace
adjustedP1 = adjustedPitchSpace (3 % 2) p1

adjustedP2 :: PitchSpace
adjustedP2 = adjustedPitchSpace (5 % 1) p2


ghci> p1 = rationalToPitchSpace (1 % 1)
ghci> p2 = rationalToPitchSpace (5.5)
ghci> p1Pitch = p1 ^. pitch
ghci> p1Pitch 
1 % 1
ghci> p2NoteName = p2 ^. note . noteName
ghci> p2
p2          p2NoteName
ghci> p2NoteName 
F
ghci> modifiedP1 = updateNote $ p1 & pitch %~ (+ (1 % 2))
ghci> modifiedP1 
PitchSpace {_pitch = 3 % 2, _note = Note {_noteName = D, _accidental = Accidental {_accidentalName = "qf", _shift = (-1) % 2}}, _octave = 0}
ghci> modifiedP2 = updateNote $ p2 & pitch %~ (+ (13 % 2))
ghci> modifiedP2
PitchSpace {_pitch = 12 % 1, _note = Note {_noteName = C, _accidental = Accidental {_accidentalName = "", _shift = 0 % 1}}, _octave = 0}
ghci> 


ghci> modifiedP1 = updateNote $ p1 & pitch %~ (+ (1 % 2))
ghci> modifiedP1 
PitchSpace {_pitch = 3 % 2, _note = Note {_noteName = D, _accidental = Accidental {_accidentalName = "qf", _shift = (-1) % 2}}, _octave = 0}
ghci> modifiedP2 = updateNote $ p2 & pitch %~ (+ (13 % 2))
ghci> modifiedP2
PitchSpace {_pitch = 12 % 1, _note = Note {_noteName = C, _accidental = Accidental {_accidentalName = "", _shift = 0 % 1}}, _octave = 0}
ghci> modifiedP3 = updateNote $ p2 & pitch %~ (+ (15 % 2))
ghci> modifiedP3
PitchSpace {_pitch = 13 % 1, _note = Note {_noteName = C, _accidental = Accidental {_accidentalName = "s", _shift = 1 % 1}}, _octave = 0}
ghci> modifiedP4 = updateNote $ p2 & pitch %~ (+ (15 % 1))
ghci> modifiedP4
PitchSpace {_pitch = 41 % 2, _note = Note {_noteName = A, _accidental = Accidental {_accidentalName = "qf", _shift = (-1) % 2}}, _octave = 0}
ghci> 
   -}