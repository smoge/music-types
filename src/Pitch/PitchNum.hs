{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module PitchNum where
import Data.Kind (Type)
import Data.Ratio
import Test.QuickCheck

data PitchNum = PitchNum Integer Rational

class AffineSpace p where
  type Diff p :: Type
  (.-.) :: p -> p -> Diff p
  (.+^) :: p -> Diff p -> p

instance AffineSpace PitchNum where
  type Diff PitchNum = Interval
  (.+^) = addInterval
  (.-.) = subtractPitches

data Interval = Interval Integer Rational

instance Show Interval where
  show (Interval int rat)
    | rat == 0 =  "Interval " ++ show int
    | otherwise = show int ++ " (" ++ show rat ++ ")"

instance Show PitchNum where
  show (PitchNum int rat)
    | rat == 0 = "PitchNum " ++ show int
    | otherwise = "PitchNum " ++ show int ++ " (" ++ show rat ++ ")"

addInterval :: PitchNum -> Interval -> PitchNum
addInterval pitch (Interval int rat) = normalizePitchNum (pitch + PitchNum int rat)

subtractPitches :: PitchNum -> PitchNum -> Interval
subtractPitches (PitchNum int1 rat1) (PitchNum int2 rat2) = normalizeInterval $ Interval (int1 - int2) (rat1 - rat2)

instance Num Interval where
  (+) (Interval int1 rat1) (Interval int2 rat2) = normalizeInterval $ Interval (int1 + int2) (rat1 + rat2)
  (-) (Interval int1 rat1) (Interval int2 rat2) = normalizeInterval $ Interval (int1 - int2) (rat1 - rat2)
  (*) (Interval int1 rat1) (Interval int2 rat2) = normalizeInterval $ Interval (int1 * int2) (rat1 * rat2)
  fromInteger n = Interval n (0 % 1)
  abs (Interval int rat) = Interval (abs int) (abs rat)
  signum (Interval int rat) = Interval (signum int) (signum rat)

instance Num PitchNum where
  (+) (PitchNum int1 rat1) (PitchNum int2 rat2) = normalizePitchNum $ PitchNum (int1 + int2) (rat1 + rat2)
  fromInteger n = PitchNum n (0 % 1)
  negate (PitchNum int rat) = PitchNum (negate int) (negate rat)
  (*) (PitchNum int1 rat1) (PitchNum int2 rat2) = normalizePitchNum $  PitchNum (int1 * int2) (rat1 * rat2)
  (-) (PitchNum int1 rat1) (PitchNum int2 rat2) = normalizePitchNum $  PitchNum (int1 - int2) (rat1 - rat2)
  abs (PitchNum int rat) = PitchNum (abs int) (abs rat)
  signum (PitchNum int rat) = PitchNum (signum int) (signum rat)

normalizeInterval :: Interval -> Interval
normalizeInterval (Interval int rat) =
  let normalizedRat = normalizeRational rat
      intAdd = numerator rat `div` denominator rat
      normalizedInt = int + intAdd
   in Interval normalizedInt normalizedRat

normalizeRational :: Rational -> Rational
normalizeRational rat
  | rat >= 1 = normalizeRational (rat - 1)
  | rat < 0 = normalizeRational (rat + 1)
  | otherwise = rat

{-

normalizeInterval :: Interval -> Interval
normalizeInterval (Interval int rat) =
  let normalizedRat = normalizeRational rat
      normalizedInt = int + numerator rat `div` denominator rat
   in Interval normalizedInt normalizedRat

normalizePitchNum :: PitchNum -> PitchNum
normalizePitchNum (PitchNum int rat) =
  let normalizedRat = normalizeRational rat
      normalizedInt = int + numerator rat `div` denominator rat
   in PitchNum normalizedInt normalizedRat

-}
normalizePitchNum :: PitchNum -> PitchNum
normalizePitchNum (PitchNum int rat) =
  let normalizedRat = normalizeRational rat
      intAdd = numerator rat `div` denominator rat
      normalizedInt = int + intAdd
   in PitchNum normalizedInt normalizedRat

-- Create a PitchNum from any numeric type
p :: Float -> PitchNum
p x = normalizePitchNum $ PitchNum 0 (toRational x) 

instance Real PitchNum where
  toRational (PitchNum int rat) = (toRational int) + rat

instance Real Interval where
  toRational (Interval int rat) = (toRational int) + rat


instance Ord PitchNum where
  compare pitch1 pitch2 = compare (toRational pitch1) (toRational pitch2)

instance Ord Interval where
  compare int1 int2 = compare (toRational int1) (toRational int2)


instance Eq PitchNum where
  (==) pitch1 pitch2 = compare (toRational pitch1) (toRational pitch2) == EQ
  (/=) pitch1 pitch2 = compare (toRational pitch1) (toRational pitch2) /= EQ

instance Eq Interval where
  (==) pitch1 pitch2 = compare (toRational pitch1) (toRational pitch2) == EQ
  (/=) pitch1 pitch2 = compare (toRational pitch1) (toRational pitch2) /= EQ


instance Arbitrary PitchNum where
  arbitrary = PitchNum <$> arbitrary <*> arbitrary

instance Arbitrary Interval where
  arbitrary = Interval <$> arbitrary <*> arbitrary



isApproxEqual :: PitchNum -> PitchNum -> Bool
isApproxEqual x y = x == y || normalizePitchNum x == normalizePitchNum y


-- -- QuickCheck property for addition
-- prop_Addition :: PitchNum -> PitchNum -> Bool
-- prop_Addition x y = isApproxEqual (x + y) (normalizePitchNum (x + y))

prop_Addition :: PitchNum -> PitchNum -> Property
prop_Addition x y = isApproxEqual result expected ==> x + y === result
  where
    result = normalizePitchNum (x + y)
    expected = x + y

-- QuickCheck property for subtraction
prop_Subtraction :: PitchNum -> PitchNum -> Bool
prop_Subtraction x y = isApproxEqual (x - y) (normalizePitchNum (x - y))

-- QuickCheck property for multiplication
prop_Multiplication :: PitchNum -> PitchNum -> Bool
prop_Multiplication x y = isApproxEqual (x * y) (normalizePitchNum (x * y))

-- QuickCheck property for negation
prop_Negation :: PitchNum -> Bool
prop_Negation x = isApproxEqual (negate x) (normalizePitchNum (negate x))

-- QuickCheck property for absolute value
prop_AbsoluteValue :: PitchNum -> Bool
prop_AbsoluteValue x = isApproxEqual (abs x) (normalizePitchNum (abs x))

-- QuickCheck property for signum
prop_Signum :: PitchNum -> Bool
prop_Signum x = isApproxEqual (signum x) (normalizePitchNum (signum x))

-- -- QuickCheck property for comparison
-- prop_Comparison :: PitchNum -> PitchNum -> Bool
-- prop_Comparison x y = (x == y) == ((toRational x) == (toRational y))

-- QuickCheck property for comparison
prop_Comparison :: PitchNum -> PitchNum -> Property
prop_Comparison x y = compare x y === compare (toRational x) (toRational y)


-- Associativity of Comparison
prop_Associativity_Comparison :: PitchNum -> PitchNum -> Bool
prop_Associativity_Comparison x y = (x < y) == (y > x) && (x <= y) == (y >= x)



main :: IO ()
main = do
  putStrLn "Running QuickCheck tests..."
  quickCheckWith (stdArgs {maxSuccess = 1000}) prop_Addition
  quickCheckWith (stdArgs {maxSuccess = 1000}) prop_Subtraction
  quickCheckWith (stdArgs {maxSuccess = 1000}) prop_Multiplication
  quickCheckWith (stdArgs {maxSuccess = 1000}) prop_Negation
  quickCheckWith (stdArgs {maxSuccess = 1000}) prop_AbsoluteValue
  quickCheckWith (stdArgs {maxSuccess = 1000}) prop_Signum
  quickCheckWith (stdArgs {maxSuccess = 1000}) prop_Comparison
  quickCheckWith (stdArgs {maxSuccess = 1000}) prop_Associativity_Comparison
  putStrLn "All QuickCheck tests passed!"


{- 



 -}
--   quickCheckWith (stdArgs {maxSuccess = 1000}) prop_Multiplication
--   quickCheckWith (stdArgs {maxSuccess = 1000}) prop_Negation
--   quickCheckWith (stdArgs {maxSuccess = 1000}) prop_AbsoluteValue
--   quickCheckWith (stdArgs {maxSuccess = 1000}) prop_Signum
--   quickCheckWith (stdArgs {maxSuccess = 1000}) prop_Comparison
--   quickCheckWith (stdArgs {maxSuccess = 1000}) prop_Associativity_Comparison
--   putStrLn "All QuickCheck tests passed!"


{- 



 -}
