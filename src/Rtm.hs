{-# LANGUAGE DeriveGeneric #-}

module Rtm
  ( RtmMeasure
  , RtmValue(..)
  , RtmProportions(..)
  , Rtm(..)
  , getRtmProportions
  , formatRtmProportions
  , formatRtmValue
  ) where

import           Control.Applicative ((*>), (<$>), (<*))
import           Data.Either         (fromRight)
import           Data.Ratio          ((%))
import           GHC.Generics        (Generic)
import           Numeric.Natural
import           Text.Parsec
import           Text.Parsec.String  (Parser)
import           Text.Printf
import           Text.Show.Pretty    (ppShow)

type Duration = Rational

data RtmValue
  = RtmNote Natural
  | RtmRest Natural
  | RtmLeaf Natural RtmProportions
  deriving (Eq, Ord, Show, Generic)

data RtmProportions = RtmProportions [RtmValue]
  deriving (Eq, Ord, Show, Generic)

type RtmMeasure = (Duration, RtmProportions)
type Rtm = [RtmMeasure]

parseRtmValue :: Parser RtmValue
parseRtmValue = parseRtmNote <|> parseRtmRest <|> parseRtmLeaf

parseRtmNote :: Parser RtmValue
parseRtmNote = RtmNote . read <$> many1 digit

parseRtmRest :: Parser RtmValue
parseRtmRest = RtmRest . read <$> (char '-' *> many1 digit)

parseRtmLeaf :: Parser RtmValue
parseRtmLeaf = do
  _ <- char '('
  n <- read <$> many1 digit
  _ <- spaces
  values <- parseRtmProportions
  _ <- char ')'
  return (RtmLeaf n values)

parseRtmProportions :: Parser RtmProportions
parseRtmProportions = do
  _ <- char '('
  values <- sepBy parseRtmValue spaces
  _ <- char ')'
  return (RtmProportions values)

getRtmProportions :: String -> Either String RtmProportions
getRtmProportions input = case parse parseRtmProportions "" input of
  Left err          -> Left $ show err
  Right proportions -> Right proportions

formatRtmProportions :: Int -> RtmProportions -> String
formatRtmProportions _ (RtmProportions []) = ""
formatRtmProportions n (RtmProportions (x:xs)) =
  indent n ++ formatRtmValue n x ++ "\n" ++ formatRtmProportions n (RtmProportions xs)

formatRtmValue :: Int -> RtmValue -> String
formatRtmValue n (RtmNote i) = printf "RtmNote %d" i
formatRtmValue n (RtmRest i) = printf "RtmRest %d" i
formatRtmValue n (RtmLeaf i p) =
  printf "RtmLeaf\n%s%d\n%s" (indent (n + 1)) i (formatRtmProportions (n + 1) p)

indent :: Int -> String
indent n = replicate (2 * n) ' '


-- Function to calculate the sum of RtmNotes, RtmRests, and the first value of RtmLeaf
sumValues :: RtmProportions -> Natural
sumValues (RtmProportions values) = go values
  where
    go :: [RtmValue] -> Natural
    go []                                        = 0
    go (RtmNote _ : xs)                          = go xs
    go (RtmRest _ : xs)                          = go xs
    go (RtmLeaf i (RtmProportions (x : _)) : xs) = i + go (x : xs)
    go (RtmLeaf _ _ : xs)                        = go xs

-- Example tree
exampleTree :: RtmProportions
exampleTree = RtmProportions
  [ RtmNote 1
  , RtmLeaf 2 (RtmProportions [ RtmNote 3, RtmRest 4 ])
  , RtmNote 5
  , RtmLeaf 6 (RtmProportions [ RtmNote 7, RtmLeaf 8 (RtmProportions [ RtmRest 9, RtmNote 10 ]) ])
  ]

-- Test the sumValues function
testSumValues :: IO ()
testSumValues = do
  let result = sumValues exampleTree
  putStrLn $ "Sum of values: " ++ show result


{-

# Some references:

  - https://support.ircam.fr/docs/om/om6-manual/co/RT1.html
  - https://www.tenor-conference.org/proceedings/2017/18_Jacquemard_tenor2017.pdf
  - http://repmus.ircam.fr/_media/jacquemard/strn-mcm.pdf
  
-}