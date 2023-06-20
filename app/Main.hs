module Main (main) where
import           Rtm

-- Quick test
main :: IO ()
main = do
    let result = fromRight (error "Parsing error") (getRtmProportions "(1 -1 (1 (1 (1 (1 -1 1)) 1)) 1 (1 (1 -1 (1 (1 1 1 1 -1)) 1)) 1)")
    printf (formatRtmProportions result)