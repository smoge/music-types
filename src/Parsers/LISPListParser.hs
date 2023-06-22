{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import Control.Applicative
import Data.Char (isDigit)

data Value = Number Int | List [Value] deriving Show

value :: Parser Value
value = List <$> (char '(' *> sepBy value space <* char ')')
        <|> Number . read . B.unpack <$> takeWhile1 isDigitOrMinus

isDigitOrMinus :: Char -> Bool
isDigitOrMinus c = Data.Char.isDigit c || c == '-'

parseLISP :: B.ByteString -> Either String Value
parseLISP input = parseOnly (value <* endOfInput) input

main :: IO ()
main = do
    let example = "(1 (2 (3 -4)) 5 (6 (7 (8 (-9 10)))))"
    let parsed = parseLISP $ B.pack example
    print parsed

-- Right (List [Number 1,List [Number 2,List [Number 3,Number (-4)]],Number 5,List [Number 6,List [Number 7,List [Number 8,List [Number (-9),Number 10]]]]])

