{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Eta reduce" #-}


import           Data.List   (sortBy)
import           Data.Maybe  (fromMaybe)
import           Data.Ratio
import           Text.Parsec as Parsec (Parsec, between, char, digit, many,
                                        many1, oneOf, optionMaybe, parse,
                                        sepBy1, spaces, string, try, (<|>))

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
        accidentalOrder Sharp                    = 1 % 1
        accidentalOrder Flat                     = (-1) % 1
        accidentalOrder Natural                  = 0 % 1
        accidentalOrder SemiSharp                = 1 % 2
        accidentalOrder SemiFlat                 = (-1) % 2
        accidentalOrder SesquiSharp              = 3 % 2
        accidentalOrder SesquiFlat               = (-3) % 2
        accidentalOrder (CustomAccidental value) = value

data Octave where
    Octave :: Int -> Octave
    deriving (Show, Eq, Ord)

data Pitch = Pitch NoteName Accidental Octave
    deriving (Show, Eq)

data MusicElement where
    Note :: Pitch -> Dur -> MusicElement
    Rest :: Dur -> MusicElement
    Chord :: [Pitch] -> Dur -> MusicElement
    Tuplet :: Rational -> [MusicElement] -> MusicElement
    deriving (Show, Eq)

type Music = [MusicElement]

durToRat :: Dur -> Rational
durToRat Whole       = 1 % 1
durToRat Half        = 1 % 2
durToRat Quarter     = 1 % 4
durToRat Eighth      = 1 % 8
durToRat Sixteenth   = 1 % 16
durToRat T32         = 1 % 32
durToRat T64         = 1 % 64
durToRat T128        = 1 % 128
durToRat T256        = 1 % 256
durToRat T512        = 1 % 512
durToRat T1024       = 1 % 1024
durToRat (Dotted nl) = durToRat nl * (3 % 2)

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
    let
        pitchValue = fromMaybe (0%1) (lookup notename [(C, 0%1), (D, 2%1), (E, 4%1), (F, 5%1), (G, 7%1), (A, 9%1), (B, 11%1)])

        accidentalValue = case accidental of
            Sharp                  -> 1 % 1
            Flat                   -> (-1 % 1)
            Natural                -> 0 % 1
            SemiSharp              -> 1 % 2
            SemiFlat               -> (-1 % 2)
            SesquiSharp            -> 3 % 2
            SesquiFlat             -> (-3 % 2)
            CustomAccidental value -> value

        totalValue = (pitchValue + accidentalValue) + toRational (oct * 12)
    in totalValue

chordPitchToRat :: MusicElement -> [Rational]
chordPitchToRat (Note pitch _) = [pitchToRat pitch]
chordPitchToRat (Chord pitches _) = map pitchToRat pitches
chordPitchToRat _ = error "Function can only convert Notes and Chords to List of Rational"


midiCps :: Float -> Float
midiCps note = 440.0 * (2.0 ** ((note - 69.0) * (1.0 / 12.0)))

pitchToHertz :: Pitch -> Float
pitchToHertz pitch = midiCps (fromRational (pitchToRat pitch) - 60)

noteToHertz :: MusicElement -> Float
noteToHertz (Note pitch _) = pitchToHertz pitch
noteToHertz _              = error "Function can only convert Notes to Hertz"


{-
c 4
fs 8


-}

createNote :: NoteName -> Accidental -> Octave -> Dur -> MusicElement
createNote noteName accidental octave dur = Note (Pitch noteName accidental octave) dur

c, cs, css, cf, csf :: Int -> Dur -> MusicElement
c o dur = createNote C Natural (Octave o) dur
css o dur = createNote C SemiSharp (Octave o) dur
cs o dur = createNote C Sharp (Octave o) dur
cf o dur = createNote C Flat (Octave o) dur
csf o dur = createNote C SemiFlat (Octave o) dur

d, ds, dss, df, dsf :: Int -> Dur -> MusicElement
d o dur = createNote D Natural (Octave o) dur
ds o dur = createNote D Sharp (Octave o) dur
df o dur = createNote D Flat (Octave o) dur
dss o dur = createNote D SemiSharp (Octave o) dur
dsf o dur = createNote D SemiFlat (Octave o) dur

e, es, ess, ef, esf :: Int -> Dur -> MusicElement
e o dur = createNote E Natural (Octave o) dur
es o dur = createNote E Sharp (Octave o) dur
ef o dur = createNote E Flat (Octave o) dur
ess o dur = createNote E SemiSharp (Octave o) dur
esf o dur = createNote E SemiFlat (Octave o) dur

f, fs, fss, ff, fsf :: Int -> Dur -> MusicElement
f o dur = createNote F Natural (Octave o) dur
fs o dur = createNote F Sharp (Octave o) dur
ff o dur = createNote F Flat (Octave o) dur
fss o dur = createNote F SemiSharp (Octave o) dur
fsf o dur = createNote F SemiFlat (Octave o) dur

g, gs, gss, gf, gsf :: Int -> Dur -> MusicElement
g o dur = createNote G Natural (Octave o) dur
gs o dur = createNote G Sharp (Octave o) dur
gf o dur = createNote G Flat (Octave o) dur
gss o dur = createNote G SemiSharp (Octave o) dur
gsf o dur = createNote G SemiFlat (Octave o) dur

a, as, ass, af, asf :: Int -> Dur -> MusicElement
a o dur = createNote A Natural (Octave o) dur
as o dur = createNote A Sharp (Octave o) dur
af o dur = createNote A Flat (Octave o) dur
ass o dur = createNote A SemiSharp (Octave o) dur
asf o dur = createNote A SemiFlat (Octave o) dur

b, bs, bss, bf, bsf :: Int -> Dur -> MusicElement
b o dur = createNote B Natural (Octave o) dur
bs o dur = createNote B Sharp (Octave o) dur
bf o dur = createNote B Flat (Octave o) dur
bss o dur = createNote B SemiSharp (Octave o) dur
bsf o dur = createNote B SemiFlat (Octave o) dur

t1, t2, t4, t8, t16, t32, t64, t128, t256, t512, t1024 :: Dur
d1, d2, d4, d8, d16, d32, d64, d128, d256, d512, d1024 :: Dur

t1 = Whole;    t2 = Half;    t4 = Quarter;    t8 = Eighth;    t16 = Sixteenth;    t32 = T32
t64 = T64;    t128 = T128;    t256 = T256;    t512 = T512;    t1024 = T1024

d1 = Dotted Whole;    d2 = Dotted Half;    d4 = Dotted Quarter;    d8 = Dotted Eighth;    d16 = Dotted Sixteenth
d32 = Dotted T32;    d64 = Dotted T64;    d128 = Dotted T128;    d256 = Dotted T256;    d512 = Dotted T512
d1024 = Dotted T1024



noteC4 :: MusicElement
noteC4 = c 0 t4


--ghci> c 0 t4
-- Note (Pitch C Natural (Octave 0)) Quarter



{------------------ Parser LilyPond Syntax ---------------------}

type Parser = Parsec.Parsec String ()


parseNoteName :: Parser NoteName
parseNoteName = do
    pitch <- Parsec.oneOf "abcdefg"
    return $ case pitch of
        'a' -> A
        'b' -> B
        'c' -> C
        'd' -> D
        'e' -> E
        'f' -> F
        'g' -> G
        _   -> error "Invalid note name"

parseAccidental :: Parser Accidental
parseAccidental = do
    acc <- Parsec.optionMaybe (Parsec.try (Parsec.string "isih")
                               Parsec.<|> Parsec.try (Parsec.string "eseh")
                               Parsec.<|> Parsec.try (Parsec.string "is")
                               Parsec.<|> Parsec.try (Parsec.string "es")
                               Parsec.<|> Parsec.try (Parsec.string "ih")
                               Parsec.<|> Parsec.try (Parsec.string "eh"))
    return $ case acc of
        Just "is"   -> Sharp
        Just "es"   -> Flat
        Just "ih"   -> SemiSharp
        Just "eh"   -> SemiFlat
        Just "isih" -> SesquiSharp
        Just "eseh" -> SesquiFlat
        _           -> Natural

parseOctave :: Parser Octave
parseOctave = do
    octave <- Parsec.many (Parsec.oneOf "',")
    return $ Octave (length (filter (== '\'') octave) - length (filter (== ',') octave))

parsePitch :: Parser Pitch
parsePitch = do
    notename <- parseNoteName
    accidental <- parseAccidental
    octave <- parseOctave
    return $ Pitch notename accidental octave

parseDur :: Parser Dur
parseDur = do
    len <- Parsec.optionMaybe (Parsec.many1 Parsec.digit)
    dot <- Parsec.optionMaybe (Parsec.char '.')
    return $ case (len, dot) of
        (Just "1", Nothing)    -> Whole
        (Just "2", Nothing)    -> Half
        (Just "4", Nothing)    -> Quarter
        (Just "8", Nothing)    -> Eighth
        (Just "16", Nothing)   -> Sixteenth
        (Just "32", Nothing)   -> T32
        (Just "64", Nothing)   -> T64
        (Just "128", Nothing)  -> T128
        (Just "256", Nothing)  -> T256
        (Just "512", Nothing)  -> T512
        (Just "1024", Nothing) -> T1024
        (Just "1", Just _)     -> Dotted Whole
        (Just "2", Just _)     -> Dotted Half
        (Just "4", Just _)     -> Dotted Quarter
        (Just "8", Just _)     -> Dotted Eighth
        (Just "16", Just _)    -> Dotted Sixteenth
        (Just "32", Just _)    -> Dotted T32
        (Just "64", Just _)    -> Dotted T64
        (Just "128", Just _)   -> Dotted T128
        (Just "256", Just _)   -> Dotted T256
        (Just "512", Just _)   -> Dotted T512
        (Just "1024", Just _)  -> Dotted T1024
        _                      -> Eighth

parseNote :: Parser MusicElement
parseNote = do
    spaces
    pitch <- parsePitch
    dur <- parseDur
    spaces
    return (Note pitch dur)


parseChord :: Parser MusicElement
parseChord = do
    _ <- Parsec.char '<'
    notes <- Parsec.sepBy1 parsePitch (Parsec.spaces >> Parsec.spaces)
    _ <- Parsec.char '>'
    Chord notes <$> parseDur



parseFraction :: Parser Rational
parseFraction = do
    num <- Parsec.many1 Parsec.digit
    _ <- Parsec.char '/'
    denom <- Parsec.many1 Parsec.digit
    return (read num % read denom)

-- parseTuplet :: Parser MusicElement
-- parseTuplet = do
--     _ <- Parsec.string "\\tuplet"
--     Parsec.spaces
--     frac <- parseFraction
--     Parsec.spaces
--     elements <- Parsec.between (Parsec.char '{' >> Parsec.spaces) (Parsec.char '}') (Parsec.many (parseMusicElement <* Parsec.spaces))
--     return $ Tuplet frac elements

parseTuplet :: Parser MusicElement
parseTuplet = do
    _ <- Parsec.string "\\tuplet"
    Parsec.spaces
    frac <- parseFraction
    Parsec.spaces
    elements <- Parsec.between (Parsec.char '{' >> Parsec.spaces) (Parsec.char '}') (Parsec.many (parseTupletElement <* Parsec.spaces))
    return $ Tuplet frac elements

parseTupletElement :: Parser MusicElement
parseTupletElement = parseNote Parsec.<|> parseChord Parsec.<|> parseTuplet


parseMusicElement :: Parser MusicElement
parseMusicElement = parseNote Parsec.<|> parseChord Parsec.<|> parseTuplet

parseMusic :: Parser Music
parseMusic = Parsec.between (Parsec.char '{' >> Parsec.spaces) (Parsec.char '}') (Parsec.many (parseMusicElement <* Parsec.spaces))


main :: IO ()
main = do
    let example = "{ a4 \\tuplet 3/2 { cis8 \\tuplet 3/2 { d8 e8 f8 } } <a c e>1. <a c e>2 <f a c e>4 <a c>8 <g c e>4 }"
    let parsedExample = Parsec.parse parseMusic "" example
    print parsedExample

{-
Right
  [ Note (Pitch A Natural (Octave 0)) Quarter
  , Tuplet (3 % 2)
    [ Note (Pitch C Sharp (Octave 0)) Eighth
    , Tuplet (3 % 2)
      [ Note (Pitch D Natural (Octave 0)) Eighth
      , Note (Pitch E Natural (Octave 0)) Eighth
      , Note (Pitch F Natural (Octave 0)) Eighth
      ]
    ]
  , Chord [Pitch A Natural (Octave 0), Pitch C Natural (Octave 0), Pitch E Natural (Octave 0)] (Dotted Whole)
  , Chord [Pitch A Natural (Octave 0), Pitch C Natural (Octave 0), Pitch E Natural (Octave 0)] Half
  , Chord [Pitch F Natural (Octave 0), Pitch A Natural (Octave 0), Pitch C Natural (Octave 0), Pitch E Natural (Octave 0)] Quarter
  , Chord [Pitch A Natural (Octave 0), Pitch C Natural (Octave 0)] Eighth
  , Chord [Pitch G Natural (Octave 0), Pitch C Natural (Octave 0), Pitch E Natural (Octave 0)] Quarter
  ]
-}
