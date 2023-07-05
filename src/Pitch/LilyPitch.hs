{-# LANGUAGE GADTs             #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Text.Parsec        as Parsec
import           Text.Parsec.String (Parser)

data NoteLength = Whole | Half | Quarter | Eighth | Sixteenth deriving (Show)

data Pitch = A | B | C | D | E | F | G deriving (Show)

data Accidental = Sharp | Flat | Natural | SemiSharp | SemiFlat | SesquiSharp | SesquiFlat deriving (Show)

data Octave where
  Octave :: Int -> Octave
  deriving (Show)

data MusicElement = Note Pitch Accidental Octave NoteLength deriving (Show)

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

parseMusicElement :: Parser MusicElement
parseMusicElement = parseNote

parseMusic :: Parser Music
parseMusic = Parsec.between (Parsec.char '{' >> Parsec.spaces) (Parsec.char '}') (Parsec.many (parseMusicElement <* Parsec.spaces))

main :: IO ()
main = do
    let example = "{ ceseh'4 ces, ceh,, c'' cih cis cisih }"
    let parsedExample = Parsec.parse parseMusic "" example
    print parsedExample

{-
Right [Note C SesquiFlat (Octave 1) Quarter,Note C Flat (Octave (-1)) Quarter,Note C SemiFlat (Octave (-2)) Quarter,Note C Natural (Octave 2) Quarter,Note C SemiSharp (Octave 0) Quarter,Note C Sharp (Octave 0) Quarter,Note C SesquiSharp (Octave 0) Quarter]
-}

