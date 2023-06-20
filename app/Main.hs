module Main (main) where

--import Lib
import           Rtm

-- main :: IO ()
-- main = someFunc

-- main :: IO ()
-- main = printRtm1


main :: IO ()
main = do
    let result = fromRight (error "Parsing error") (getRtmProportions "(1 -1 (1 (1 (1 (1 -1 1)) 1)) 1 (1 (1 -1 (1 (1 1 1 1 -1)) 1)) 1)")
    printf (formatRtmProportions result)