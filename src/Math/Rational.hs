module Math.Rational
  ( showRational,
    roundToNearestDivision72ET,
    showNearestDivision72ET,
    roundToNearestDivision24ET,
    showNearestDivision24ET,
  )
where

import Data.Fixed
import Data.Ratio
import Test.QuickCheck

-- | Splits a rational number into its integral and fractional parts.
showRational :: Rational -> (Int, Rational)
showRational = properFraction

-- | Rounds a rational number to the nearest multiple of 1/6.
roundToNearestDivision72ET :: Rational -> Rational
roundToNearestDivision72ET x =
  let division = 1 % 6
      ratioAsDecimal = fromRational x :: Double
      roundedValue = round (ratioAsDecimal / fromRational division)
   in (roundedValue % 1) * division

-- | Rounds a rational number to the nearest multiple of 1/6.
roundToNearestDivision24ET :: Rational -> Rational
roundToNearestDivision24ET x =
  let division = 1 % 2
      ratioAsDecimal = fromRational x :: Double
      roundedValue = round (ratioAsDecimal / fromRational division)
   in (roundedValue % 1) * division

-- | Returns the integral and fractional parts of a rational number after rounding it to the nearest multiple of 1/6.
showNearestDivision72ET :: Rational -> (Int, Rational)
showNearestDivision72ET = showRational . roundToNearestDivision72ET

showNearestDivision24ET :: Rational -> (Int, Rational)
showNearestDivision24ET = showRational . roundToNearestDivision24ET

-- TESTS

-- For showRational: When you add the integral and fractional parts together, you should get the original number.
prop_showRational :: Rational -> Bool
prop_showRational r =
  let (i, f) = showRational r
   in r == (fromIntegral i + f)

-- For roundToNearestDivision72ET: Ensure that the result is always a multiple of 1/6.
prop_roundToNearestDivision72ET :: Rational -> Bool
prop_roundToNearestDivision72ET r =
  let result = roundToNearestDivision72ET r
      remainder = result * 6
   in denominator remainder == 1

-- For roundToNearestDivision24ET: Ensure that the result is always a multiple of 1/2.
prop_roundToNearestDivision24ET :: Rational -> Bool
prop_roundToNearestDivision24ET r =
  let result = roundToNearestDivision24ET r
      remainder = result * 2
   in denominator remainder == 1

-- For showNearestDivision72ET: Similar to showRational, but after rounding the number.
prop_showNearestDivision72ET :: Rational -> Bool
prop_showNearestDivision72ET r =
  let rounded = roundToNearestDivision72ET r
      (i, f) = showNearestDivision72ET r
   in rounded == (fromIntegral i + f)

-- For showNearestDivision24ET: Similar to showRational, but after rounding the number using 24ET.
prop_showNearestDivision24ET :: Rational -> Bool
prop_showNearestDivision24ET r =
  let rounded = roundToNearestDivision24ET r
      (i, f) = showNearestDivision24ET r
   in rounded == (fromIntegral i + f)

-- Run tests
main :: IO ()
main = do
  quickCheck prop_showRational
  quickCheck prop_roundToNearestDivision72ET
  quickCheck prop_roundToNearestDivision24ET
  quickCheck prop_showNearestDivision72ET
  quickCheck prop_showNearestDivision24ET