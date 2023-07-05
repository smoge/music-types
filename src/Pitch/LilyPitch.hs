{-# LANGUAGE GADTs             #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import qualified Text.Parsec         as Parsec
import           Text.Parsec.String  (Parser)

data NoteLength = Whole | Half | Quarter | Eighth | Sixteenth deriving (Show)

data Pitch = A | B | C | D | E | F | G deriving (Show)

data Accidental = Sharp | Flat | Natural | SemiSharp | SemiFlat | SesquiSharp | SesquiFlat deriving (Show)

data Octave where
  Octave :: Int -> Octave
  deriving (Show)

data MusicElement where
  Note :: Pitch -> Accidental -> Octave -> NoteLength -> MusicElement
  Chord :: [(Pitch, Accidental, Octave)] -> NoteLength -> MusicElement
  deriving (Show)


type Music = [MusicElement]

data Metronome where
  Metronome :: NoteLength -> Int -> Metronome

instance Show Metronome where
  show :: Metronome -> String
  show (Metronome noteLength bpm) = showNoteLength noteLength ++ " = " ++ show bpm

showNoteLength :: NoteLength -> String
showNoteLength Whole     = "𝅝"
showNoteLength Half      = "𝅗𝅥"
showNoteLength Quarter   = "𝅘𝅥"
showNoteLength Eighth    = "♪"
showNoteLength Sixteenth = "𝅘𝅥𝅯"

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

parseAccidental :: Text.Parsec.String.Parser Accidental
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
    length <- Parsec.optionMaybe (Parsec.oneOf "1248")
    return $ case length of
        Just '1' -> Whole
        Just '2' -> Half
        Just '4' -> Quarter
        Just '8' -> Eighth
        _        -> Quarter -- default

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


parseMusicElement :: Parser MusicElement
parseMusicElement = parseNote <|> parseChord

parseMusic :: Parser Music
parseMusic = Parsec.between (Parsec.char '{' >> Parsec.spaces) (Parsec.char '}') (Parsec.many (parseMusicElement <* Parsec.spaces))

main :: IO ()
main = do
    let example = "{ <a' ceh e>1 <aih c, e>2 <f aih c e>4 <a c>8 <g c e>4 }"
    let parsedExample = Parsec.parse parseMusic "" example
    print parsedExample

{-
Right [
    Chord [(A,Natural,Octave 1),(C,SemiFlat,Octave 0),(E,Natural,Octave 0)] Whole,
    Chord [(A,SemiSharp,Octave 0),(C,Natural,Octave (-1)),(E,Natural,Octave 0)] Half,
    Chord [(F,Natural,Octave 0),(A,SemiSharp,Octave 0),(C,Natural,Octave 0),(E,Natural,Octave 0)]
    Quarter,Chord [(A,Natural,Octave 0),(C,Natural,Octave 0)] Eighth,
    Chord [(G,Natural,Octave 0),(C,Natural,Octave 0),(E,Natural,Octave 0)] Quarter
    ]
-}

