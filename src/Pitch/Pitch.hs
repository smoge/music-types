{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Maybe  (fromMaybe)
import           Data.Ratio  ((%))
import           Text.Parsec as Parsec (Parsec, between, char, digit, many,
                                        many1, oneOf, optionMaybe, parse,
                                        sepBy1, spaces, string, try, (<|>))

data NoteLength = Whole | Half | Quarter | Eighth | Sixteenth | T32 | T64 | T128 | T256 | T512 | T1024 | Dotted NoteLength
    deriving (Show, Eq)

data Pitch = A | B | C | D | E | F | G deriving (Show, Eq)


data Accidental = Sharp | Flat | Natural | SemiSharp | SemiFlat | SesquiSharp | SesquiFlat | CustomAccidental Rational
    deriving (Show, Eq)

data Octave where
    Octave :: Int -> Octave
    deriving (Show, Eq, Ord)

data MusicElement where
    Note :: Pitch -> Accidental -> Octave -> NoteLength -> MusicElement
    Chord :: [(Pitch, Accidental, Octave)] -> NoteLength -> MusicElement
    Tuplet :: Rational -> [MusicElement] -> MusicElement
    Rest :: NoteLength -> MusicElement
    deriving (Show, Eq)


type Music = [MusicElement]


noteLengthToRational :: NoteLength -> Rational
noteLengthToRational Whole       = 1 % 1
noteLengthToRational Half        = 1 % 2
noteLengthToRational Quarter     = 1 % 4
noteLengthToRational Eighth      = 1 % 8
noteLengthToRational Sixteenth   = 1 % 16
noteLengthToRational T32         = 1 % 32
noteLengthToRational T64         = 1 % 64
noteLengthToRational T128        = 1 % 128
noteLengthToRational T256        = 1 % 256
noteLengthToRational T512        = 1 % 512
noteLengthToRational T1024       = 1 % 1024
noteLengthToRational (Dotted nl) = noteLengthToRational nl * (3 % 2)


noteToRational :: (Pitch, Accidental, Octave) -> Rational
noteToRational (pitch, accidental, Octave octave) =
    let
        pitchValue = fromMaybe (0%1) (lookup pitch [(C, 0%1), (D, 2%1), (E, 4%1), (F, 5%1), (G, 7%1), (A, 9%1), (B, 11%1)])

        accidentalValue = case accidental of
            Sharp                  -> 1 % 1
            Flat                   -> (-1 % 1)
            Natural                -> 0 % 1
            SemiSharp              -> 1 % 2
            SemiFlat               -> (-1 % 2)
            SesquiSharp            -> 3 % 2
            SesquiFlat             -> (-3 % 2)
            CustomAccidental value -> value

        totalValue = (pitchValue + accidentalValue) + toRational (octave * 12)
    in totalValue


pitchToRational :: MusicElement -> [Rational]
pitchToRational (Note pitch accidental octave _) = [noteToRational (pitch, accidental, octave)]
pitchToRational (Chord pitches _) = map noteToRational pitches
pitchToRational _ = error "Function can only convert Notes and Chords to Rational"



type Parser = Parsec.Parsec String ()

parsePitch :: Parser Pitch
parsePitch = do
    pitch <- Parsec.oneOf "abcdefg"
    return $ case pitch of
        'a' -> A
        'b' -> B
        'c' -> C
        'd' -> D
        'e' -> E
        'f' -> F
        'g' -> G
        _   -> error "Invalid pitch"

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

parseLength :: Parser NoteLength
parseLength = do
    len <- Parsec.optionMaybe (Parsec.oneOf "1248")
    dot <- Parsec.optionMaybe (Parsec.char '.')
    return $ case (len, dot) of
        (Just '1', Nothing) -> Whole
        (Just '2', Nothing) -> Half
        (Just '4', Nothing) -> Quarter
        (Just '8', Nothing) -> Eighth
        (Just '1', Just _)  -> Dotted Whole
        (Just '2', Just _)  -> Dotted Half
        (Just '4', Just _)  -> Dotted Quarter
        (Just '8', Just _)  -> Dotted Eighth
        _                   -> Quarter

parseNote :: Parser MusicElement
parseNote = do
    pitch <- parsePitch
    accidental <- parseAccidental
    octave <- parseOctave
    Note pitch accidental octave <$> parseLength

parseChordNote :: Parser (Pitch, Accidental, Octave)
parseChordNote = do
    pitch <- parsePitch
    accidental <- parseAccidental
    octave <- parseOctave
    return (pitch, accidental, octave)

parseChord :: Parser MusicElement
parseChord = do
    _ <- Parsec.char '<'
    notes <- Parsec.sepBy1 parseChordNote (Parsec.char ' ')
    _ <- Parsec.char '>'
    Chord notes <$> parseLength

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
    [
        Note A Natural (Octave 0) Quarter,
        Tuplet (3 % 2)
        [
            Note C Sharp (Octave 0) Eighth,
            Tuplet (3 % 2)
            [
                Note D Natural (Octave 0) Eighth,
                Note E Natural (Octave 0) Eighth,
                Note F Natural (Octave 0) Eighth
            ]
        ],
        Chord
        [
            (A, Natural, Octave 0),
            (C, Natural, Octave 0),
            (E, Natural, Octave 0)
        ] (Dotted Whole),
        Chord
        [
            (A, Natural, Octave 0),
            (C, Natural, Octave 0),
            (E, Natural, Octave 0)
        ] Half,
        Chord
        [
            (F, Natural, Octave 0),
            (A, Natural, Octave 0),
            (C, Natural, Octave 0),
            (E, Natural, Octave 0)
        ] Quarter,
        Chord
        [
            (A, Natural, Octave 0),
            (C, Natural, Octave 0)
        ] Eighth,
        Chord
        [
            (G, Natural, Octave 0),
            (C, Natural, Octave 0),
            (E, Natural, Octave 0)
        ] Quarter
    ]


-}
