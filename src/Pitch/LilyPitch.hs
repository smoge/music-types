{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Ratio  ((%))
import           Text.Parsec as Parsec

data NoteLength = Whole | Half | Quarter | Eighth | Sixteenth | Dotted NoteLength
    deriving (Show, Eq)

data Pitch = A | B | C | D | E | F | G deriving (Show, Eq)

data Accidental = Sharp | Flat | Natural | SemiSharp | SemiFlat | SesquiSharp | SesquiFlat
    deriving (Show, Eq)

data Octave where
    Octave :: Int -> Octave
    deriving (Show, Eq, Ord)

data MusicElement where
    Note :: Pitch -> Accidental -> Octave -> NoteLength -> MusicElement
    Chord :: [(Pitch, Accidental, Octave)] -> NoteLength -> MusicElement
    Tuplet :: Rational -> [MusicElement] -> MusicElement
    deriving (Show, Eq)

type Music = [MusicElement]
type Parser = Parsec String ()

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
                               <|> Parsec.try (Parsec.string "eseh")
                               <|> Parsec.try (Parsec.string "is")
                               <|> Parsec.try (Parsec.string "es")
                               <|> Parsec.try (Parsec.string "ih")
                               <|> Parsec.try (Parsec.string "eh"))
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
parseTupletElement = parseNote <|> parseChord


parseMusicElement :: Parser MusicElement
parseMusicElement = parseNote <|> parseChord <|> parseTuplet

parseMusic :: Parser Music
parseMusic = Parsec.between (Parsec.char '{' >> Parsec.spaces) (Parsec.char '}') (Parsec.many (parseMusicElement <* Parsec.spaces))


main :: IO ()
main = do
    let example = "{ a4 \\tuplet 3/2 { cis8 d8 e8 } <a c e>1. <a c e>2 <f a c e>4 <a c>8 <g c e>4 }"
    let parsedExample = Parsec.parse parseMusic "" example
    print parsedExample

{-
Right
    [ Note A Natural (Octave 0) Quarter,
      Tuplet (3 % 2)
          [ Note C Sharp (Octave 0) Eighth,
            Note D Natural (Octave 0) Eighth,
            Note E Natural (Octave 0) Eighth
          ],
      Chord [(A, Natural, Octave 0), (C, Natural, Octave 0), (E, Natural, Octave 0)] (Dotted Whole),
      Chord [(A, Natural, Octave 0), (C, Natural, Octave 0), (E, Natural, Octave 0)] Half,
      Chord [(F, Natural, Octave 0), (A, Natural, Octave 0), (C, Natural, Octave 0), (E, Natural, Octave 0)] Quarter,
      Chord [(A, Natural, Octave 0), (C, Natural, Octave 0)] Eighth,
      Chord [(G, Natural, Octave 0), (C, Natural, Octave 0), (E, Natural, Octave 0)] Quarter
    ]

-}

