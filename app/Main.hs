module Main (main) where
import Rtm
import Text.Printf
import Data.Either         (fromRight)


-- Quick test
main :: IO ()
main = do
    let result = fromRight (error "Parsing error") (getRtmProportions "(1 -1 (1 (1 (1 (1 -1 1)) 1)) 1 (1 (1 -1 (1 (1 1 1 1 -1)) 1)) 1)")
    printf (formatRtmProportions 0 result)

