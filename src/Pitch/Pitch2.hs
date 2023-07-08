{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Data.Maybe (fromMaybe)
import Data.Ratio

{-

cs' = Pitch C Sharp (Octave 0)
df' = Pitch D Flat (Octave 0)

-- Enarmonic ?
cs' =~ df'
> True

c 0 t4
> Note (Pitch C Natural (Octave 0)) Quarter

note1 = Note cs' t4
> (Pitch C Sharp 0) Quarter

note2 = Note df' t8
> (Pitch D Flat 0) Eighth

note1 =~ note2
> True

pitch cs' =~ pitch note2
> True

-- Pitch and Note:
cs' =~ note2
> True

-- # and #. operators:

note2 =~ cs'
middleC # 4
> Note (Pitch C Natural 0) Quarter

middleC #. 4
> Note (Pitch C Natural 0) (Dotted Quarter)

middleC < cs'
> True
-}

data Dur = Whole | Half | Quarter | Eighth | Sixteenth | T32 | T64 | T128 | T256 | T512 | T1024 | Dotted Dur
  deriving (Show, Eq)

data NoteName = A | B | C | D | E | F | G
  deriving (Show, Eq)

data Accidental = Sharp | Flat | Natural | SemiSharp | SemiFlat | SesquiSharp | SesquiFlat | CustomAccidental Rational
  deriving (Show, Eq)

instance Ord Accidental where
  compare a b = compare (accidentalOrder a) (accidentalOrder b)
    where
      accidentalOrder :: Accidental -> Rational
      accidentalOrder Sharp = 1 % 1
      accidentalOrder Flat = (-1) % 1
      accidentalOrder Natural = 0 % 1
      accidentalOrder SemiSharp = 1 % 2
      accidentalOrder SemiFlat = (-1) % 2
      accidentalOrder SesquiSharp = 3 % 2
      accidentalOrder SesquiFlat = (-3) % 2
      accidentalOrder (CustomAccidental value) = value

newtype Octave = Octave Int
  deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Show Octave where
  show (Octave oct) = show oct

class HasOctaves a where
  octave :: a -> Octave

instance HasOctaves Octave where
  octave = id

instance HasOctaves Int where
  octave = Octave . fromIntegral

instance HasOctaves Integer where
  octave = Octave . fromIntegral

instance HasOctaves Float where
  octave = Octave . round

data Pitch = Pitch NoteName Accidental Octave
  deriving (Show, Eq)

data Note = Note Pitch Dur
  deriving (Show, Eq)

class HasPitch a where
  pitch :: a -> Pitch
  toHertz :: a -> Float
  pitchRat :: a -> Rational

calculatePitchRat :: HasPitch a => a -> Rational
calculatePitchRat a =
  let (Pitch notename accidental (Octave oct)) = pitch a
      basePitchValue = fromMaybe 0 (lookup notename basePitchTable)
      accidentalValue = case accidental of
        Sharp -> 1 % 1
        Flat -> (-1) % 1
        Natural -> 0 % 1
        SemiSharp -> 1 % 2
        SemiFlat -> (-1) % 2
        SesquiSharp -> 3 % 2
        SesquiFlat -> (-3) % 2
        CustomAccidental value -> value
      totalValue = toRational basePitchValue + accidentalValue + toRational (oct * 12)
   in totalValue

basePitchTable :: [(NoteName, Int)]
basePitchTable =
  [ (C, 0),
    (D, 2),
    (E, 4),
    (F, 5),
    (G, 7),
    (A, 9),
    (B, 11)
  ]

instance HasPitch Pitch where
  pitch = id
  pitchRat = calculatePitchRat
  toHertz p = midiCps (fromRational (pitchRat p) - 60)

instance HasPitch Note where
  pitch (Note p _) = p
  pitchRat = calculatePitchRat . pitch
  toHertz = toHertz . pitch

instance Ord Pitch where
  compare p1 p2 = compare (pitchRat p1) (pitchRat p2)

instance Ord Note where
  compare n1 n2 = compare (pitchRat $ pitch n1) (pitchRat $ pitch n2)

-- Enharmonic?
(=~) :: (HasPitch a, HasPitch b) => a -> b -> Bool
a =~ b = pitchRat a == pitchRat b

durToRat :: Dur -> Rational
durToRat = \case
  Whole -> 1 % 1
  Half -> 1 % 2
  Quarter -> 1 % 4
  Eighth -> 1 % 8
  Sixteenth -> 1 % 16
  T32 -> 1 % 32
  T64 -> 1 % 64
  T128 -> 1 % 128
  T256 -> 1 % 256
  T512 -> 1 % 512
  T1024 -> 1 % 1024
  Dotted nl -> durToRat nl * (3 % 2)

ratToDur :: Rational -> Dur
ratToDur r
  | r == 1 % 1 = Whole
  | r == 1 % 2 = Half
  | r == 1 % 4 = Quarter
  | r == 1 % 8 = Eighth
  | r == 1 % 16 = Sixteenth
  | r == 1 % 32 = T32
  | r == 1 % 64 = T64
  | r == 1 % 128 = T128
  | r == 1 % 256 = T256
  | r == 1 % 512 = T512
  | r == 1 % 1024 = T1024
  | r > 0 && denominator r == 2 && numerator r `rem` 3 == 0 = Dotted (ratToDur (r * (2 % 3)))
  | otherwise = error "Invalid rational value for duration"

intToDur :: Int -> Dur
intToDur n = ratToDur (toRational (1 % n))

pitchToRat :: Pitch -> Rational
pitchToRat (Pitch notename accidental (Octave oct)) =
  let pitchValue = fromMaybe (0 % 1) (lookup notename [(C, 0 % 1), (D, 2 % 1), (E, 4 % 1), (F, 5 % 1), (G, 7 % 1), (A, 9 % 1), (B, 11 % 1)])

      accidentalValue = case accidental of
        Sharp -> 1 % 1
        Flat -> (-1 % 1)
        Natural -> 0 % 1
        SemiSharp -> 1 % 2
        SemiFlat -> (-1 % 2)
        SesquiSharp -> 3 % 2
        SesquiFlat -> (-3 % 2)
        CustomAccidental value -> value

      totalValue = pitchValue + accidentalValue + toRational (oct * 12)
   in totalValue

midiCps :: Float -> Float
midiCps note = 440.0 * 2.0 ** ((note - 69.0) * (1.0 / 12.0))

middleC :: Pitch
middleC = Pitch C Natural (Octave 0)

createNote :: NoteName -> Accidental -> Octave -> Dur -> Note
createNote noteName accidental oct = Note (Pitch noteName accidental oct)

createNote' :: Pitch -> Int -> Note
createNote' p dur = Note p (intToDur dur)

infixl 8 #, #.

(#) :: Pitch -> Int -> Note
(#) = createNote'

(#.) :: Pitch -> Int -> Note
p #. dur = withDot $ createNote' p dur

withDot :: Note -> Note
withDot (Note p dur) = Note p $ Dotted dur

c, cs, css, cf, csf :: Int -> Dur -> Note
c o = createNote C Natural (octave o)
css o = createNote C SemiSharp (Octave o)
cs o = createNote C Sharp (Octave o)
cf o = createNote C Flat (Octave o)
csf o = createNote C SemiFlat (Octave o)

d, ds, dss, df, dsf :: Int -> Dur -> Note
d o = createNote D Natural (Octave o)
ds o = createNote D Sharp (Octave o)
df o = createNote D Flat (Octave o)
dss o = createNote D SemiSharp (Octave o)
dsf o = createNote D SemiFlat (Octave o)

e, es, ess, ef, esf :: Int -> Dur -> Note
e o = createNote E Natural (Octave o)
es o = createNote E Sharp (Octave o)
ef o = createNote E Flat (Octave o)
ess o = createNote E SemiSharp (Octave o)
esf o = createNote E SemiFlat (Octave o)

f, fs, fss, ff, fsf :: Int -> Dur -> Note
f o = createNote F Natural (Octave o)
fs o = createNote F Sharp (Octave o)
ff o = createNote F Flat (Octave o)
fss o = createNote F SemiSharp (Octave o)
fsf o = createNote F SemiFlat (Octave o)

g, gs, gss, gf, gsf :: Int -> Dur -> Note
g o = createNote G Natural (Octave o)
gs o = createNote G Sharp (Octave o)
gf o = createNote G Flat (Octave o)
gss o = createNote G SemiSharp (Octave o)
gsf o = createNote G SemiFlat (Octave o)

a, as, ass, af, asf :: Int -> Dur -> Note
a o = createNote A Natural (Octave o)
as o = createNote A Sharp (Octave o)
af o = createNote A Flat (Octave o)
ass o = createNote A SemiSharp (Octave o)
asf o = createNote A SemiFlat (Octave o)

b, bs, bss, bf, bsf :: Int -> Dur -> Note
b o = createNote B Natural (Octave o)
bs o = createNote B Sharp (Octave o)
bf o = createNote B Flat (Octave o)
bss o = createNote B SemiSharp (Octave o)
bsf o = createNote B SemiFlat (Octave o)

t1, t2, t4, t8, t16, t32, t64, t128, t256, t512, t1024 :: Dur
[t1, t2, t4, t8, t16, t32, t64, t128, t256, t512, t1024] = [toDur n | n <- [1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024]]
  where
    toDur :: Int -> Dur
    toDur = intToDur

d1, d2, d4, d8, d16, d32, d64, d128, d256, d512, d1024 :: Dur
[d1, d2, d4, d8, d16, d32, d64, d128, d256, d512, d1024] = [Dotted dur | dur <- [t1, t2, t4, t8, t16, t32, t64, t128, t256, t512, t1024]]
