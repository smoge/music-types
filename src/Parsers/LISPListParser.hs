{-# OPTIONS_GHC -Wno-unused-matches #-}

module Parsers.LISPListParser
( parseLISPFromString
, lispToList
) where

import           Text.Parsec          (ParseError, many, parse, (<|>))
import           Text.Parsec.Language (haskellDef)
import           Text.Parsec.String   (Parser)
import qualified Text.Parsec.Token    as Token


-- Algebraic data type to represent either an Atom (Int) or a List (nested or not)
data LISP = Atom Int | List [LISP] deriving Show

-- Lexer to recognize integers and parentheses
lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser haskellDef

parens :: Parser a -> Parser a
parens = Token.parens lexer

integer :: Parser Int
integer = fromIntegral <$> Token.integer lexer

-- Parser for the LISP structure
parseLISP :: Parser LISP
parseLISP = parseAtom <|> parseList

-- Parser for the Atoms
parseAtom :: Parser LISP
parseAtom = Atom <$> integer

-- Parser for the Lists
parseList :: Parser LISP
parseList = List <$> parens (many parseLISP)

-- Function to parse a string into a LISP data structure
parseLISPFromString :: String -> Either ParseError LISP
parseLISPFromString = parse parseLISP "LISP"

-- Example usage
main :: IO ()
main = do
    let input = "(1 1 1 1 (1 (1 1 1 1 1)))"
    print $ parseLISPFromString input


lispToList :: Either ParseError LISP -> Maybe [Int]
lispToList (Right (List lisp)) = Just $ concatMap atomToList lisp
lispToList _                   = Nothing

-- lispToList $ parseLISPFromString "(1 1 1 1 (1 (1 1 1 1 1)))"
-- Just [1,1,1,1,1,1,1,1,1,1]

-- atomToList :: LISP -> [Int]
-- atomToList (Atom x)    = [x]
-- atomToList (List lisp) = concatMap atomToList lisp

-- stringToList :: String -> Maybe [Int]
-- stringToList input = lispToList $ parseLISPFromString input
