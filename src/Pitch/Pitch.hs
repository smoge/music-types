{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}


module Pitch.Pitch
  ( NoteName (..),
    Pitch (..),
    HasPitch (..),
    (=~),
    Accidental (..),
    AccidentalName (..),
    defaultAccidental,
    createPitch,
    PitchClass (..),
    createPitchClass,
    accidentals,
    noteNameToRational,
    pitchClassVal,
    pitchVal,
    Alterable (..),
    Octave (..),
    fromOctave,
    ToLilyString (..)
  )
where

import Pitch.Accidental


import Control.Lens
-- import Control.Lens.TH
-- import Data.Fixed (mod')
import Data.Maybe 
import Data.Ratio 

import Data.List 
import Test.QuickCheck
import Data.Ord

import qualified Data.Map.Strict as MapS



import qualified Data.Map as Map

data NoteName = C | D | E | F | G | A | B deriving (Eq, Enum, Ord, Show)

-- instance Show NoteName where
--     show C = "c"
--     show D = "d"
--     show E = "e"
--     show F = "f"
--     show G = "g"
--     show A = "a"
--     show B = "b"

-- instance Read NoteName where
--     readsPrec _ "c" = [(C, "")]
--     readsPrec _ "d" = [(D, "")]
--     readsPrec _ "e" = [(E, "")]
--     readsPrec _ "f" = [(F, "")]
--     readsPrec _ "g" = [(G, "")]
--     readsPrec _ "a" = [(A, "")]
--     readsPrec _ "b" = [(B, "")]
--     readsPrec _ s   = []  -- fallback case


-- newtype PitchVal = PitchVal Rational
--   deriving (Eq, Show, Num, Ord)
type PitchVal = Rational

data PitchClass = PitchClass
  { _noteName :: NoteName,
    _accidental :: Pitch.Accidental.Accidental
  } deriving (Show)



instance Ord PitchClass where
  compare :: PitchClass -> PitchClass -> Ordering
  compare pc1 pc2 = compare (pitchClassVal pc1) (pitchClassVal pc2)

instance Eq PitchClass where
  pc1 == pc2 =  pitchClassVal pc1 == pitchClassVal pc2

-- instance Show PitchClass where
--   show pc = show (_noteName pc) ++ show (_accidental pc)

-- instance Read PitchClass where
--     readsPrec _ s = 
--         [ (PitchClass noteName accidental, rest2)
--         | (noteNameStr, rest1) <- lex s
--         , noteName <- [read noteNameStr]
--         , (accidentalStr, rest2) <- lex rest1
--         , accidental <- [read accidentalStr]
--         ]

-- instance Read Pitch.Pitch.PitchClass where
--     readsPrec _ input = 
--         [ (Pitch.Pitch.PitchClass nn acc, rest2)
--         | (nnStr, rest1) <- lex input
--         , nn <- maybeToList $ readNoteName nnStr
--         , (accStr, rest2) <- lexWithDefault rest1
--         , acc <- maybeToList $ lookupAccidental accStr
--         ]

-- -- Helper function to get NoteName. 
-- -- It should return Maybe NoteName instead of using error.
-- readNoteName :: String -> Maybe Pitch.Pitch.NoteName
-- readNoteName str = case str of
--     "c" -> Just Pitch.Pitch.C
--     "d" -> Just Pitch.Pitch.D
--     "e" -> Just Pitch.Pitch.E
--     "f" -> Just Pitch.Pitch.F
--     "g" -> Just Pitch.Pitch.G
--     "a" -> Just Pitch.Pitch.A
--     "b" -> Just Pitch.Pitch.B
--     _   -> Nothing

-- -- If there's no accidental, just continue with the rest of the string
-- lexWithDefault :: String -> [(String, String)]
-- lexWithDefault s = case lex s of
--     []          -> [("", s)]
--     pairs@(_:_) -> pairs



-- -- Helper function to lookup accidental by abbreviation
-- lookupAccidental :: String -> Maybe Pitch.Accidental.Accidental
-- lookupAccidental abbrev = 
--     find (\(Pitch.Accidental.Accidental _ a _ _) -> a == abbrev) Pitch.Accidental.accidentals


-- readPitchClass :: String -> Pitch.Pitch.PitchClass
-- readPitchClass s = 
--     let (noteStr, accStr) = splitAt 1 s
--         note = fromMaybe Pitch.Pitch.C $ readNoteName noteStr
--         acc = read accStr :: Pitch.Accidental.Accidental
--     in Pitch.Pitch.PitchClass note acc
    
-- readNoteName :: String -> Pitch.Pitch.NoteName
-- readNoteName "c" = Pitch.Pitch.C
-- readNoteName "d" = Pitch.Pitch.D
-- readNoteName "e" = Pitch.Pitch.E
-- readNoteName "f" = Pitch.Pitch.F
-- readNoteName "g" = Pitch.Pitch.G
-- readNoteName "a" = Pitch.Pitch.A
-- readNoteName "b" = Pitch.Pitch.B
-- readNoteName _   = error "Invalid Note Name"



-- pitchClassVal :: PitchClass -> Rq
-- pitchClassVal pc = base + acVal
--   where
--     base = fromMaybe (0 % 1) (lookup (_noteName pc) noteNameToRational)
--     acVal = _accSemitones (_accidental pc)


pitchClassVal :: PitchClass -> Pitch.Accidental.Rq
pitchClassVal pc = base + acVal
  where
    base = fromMaybe (0 % 1) (lookup (_noteName pc) noteNameToRational) :: Pitch.Accidental.Rq
    acVal = _accSemitones (_accidental pc) :: Pitch.Accidental.Rq

noteNameToRational :: [(NoteName, Pitch.Accidental.Rq)]
noteNameToRational = 
  [ (C, 0 % 1),
    (D, 2 % 1),
    (E, 4 % 1),
    (F, 5 % 1),
    (G, 7 % 1),
    (A, 9 % 1),
    (B, 11 % 1)
  ]




class Pitchable a where
    makePitch :: NoteName -> Pitch.Accidental.Accidental -> Octave -> a
instance Pitchable Pitch where
    makePitch n acc oct = Pitch { _pitchClass = PitchClass {_noteName = n, _accidental = acc}, _octave = oct } -- Using a default octave, modify as necessary

class PitchClassable a where
    createPitchClass :: NoteName -> Pitch.Accidental.Accidental -> a

instance PitchClassable PitchClass where
    createPitchClass n acc = PitchClass {_noteName = n, _accidental = acc}

instance PitchClassable Pitch where
    createPitchClass n acc = Pitch { _pitchClass = PitchClass {_noteName = n, _accidental = acc}, _octave = 4 } -- Using a default octave, modify as necessary



class Alterable a where
  sharpen :: a -> a
  flatten :: a -> a
  quarterSharpen :: a -> a
  quarterFlatten :: a -> a

instance Alterable Accidental where
  sharpen = over semitone (+ (1 % 1))
  flatten = over semitone (subtract (1 % 1))
  quarterSharpen = over semitone (+ (1 % 2))
  quarterFlatten = over semitone (subtract (1 % 2))


instance Alterable PitchClass where
  sharpen pc = pc {_accidental = sharpen (_accidental pc)}
  flatten pc = pc {_accidental =  flatten (_accidental pc)}
  quarterSharpen pc = pc {_accidental =  quarterSharpen (_accidental pc)}
  quarterFlatten pc = pc {_accidental =  quarterFlatten (_accidental pc)}

instance Alterable Pitch where
  sharpen p = p {_pitchClass = sharpen (_pitchClass p)}
  flatten p = p {_pitchClass = flatten (_pitchClass p)}
  quarterSharpen p = p {_pitchClass = quarterSharpen (_pitchClass p)}
  quarterFlatten p = p {_pitchClass = quarterFlatten (_pitchClass p)}

noteNameToRational'' :: [(NoteName, Rational)]
noteNameToRational'' = 
  [ (C, 0 % 1),
    (D, 2 % 1),
    (E, 4 % 1),
    (F, 5 % 1),
    (G, 7 % 1),
    (A, 9 % 1),
    (B, 11 % 1)
  ]





-- 

noteNameToRational' :: Map.Map NoteName Pitch.Accidental.Rq
noteNameToRational' = Map.fromList
  [ (C, 0 % 1),
    (D, 2 % 1),
    (E, 4 % 1),
    (F, 5 % 1),
    (G, 7 % 1),
    (A, 9 % 1),
    (B, 11 % 1)
  ]

pitchClassVal' :: PitchClass -> Pitch.Accidental.Rq
pitchClassVal' pitchClass = base + ac
  where
    base = fromMaybe (error "NoteName not found") $ Map.lookup nn noteNameToRational'
    nn =  _noteName pitchClass 
    ac = (_accidental pitchClass) ^.  semitone



{- 
base = case lookup nn noteNameToRational of
  Just val -> val
  Nothing -> 0 % 1
 -}
 
newtype Octave = Octave Int
  deriving (Eq, Show)

instance Num Octave where
  (+) (Octave n1) (Octave n2) = Octave (n1 + n2)
  (-) (Octave n1) (Octave n2) = Octave (n1 - n2)
  (*) (Octave n1) (Octave n2) = undefined 
  abs (Octave n) = undefined 
  signum (Octave n) = Octave (signum n)
  fromInteger n = Octave (fromInteger n)
 


fromOctave :: Octave -> Int
fromOctave (Octave n) = n


-- instance Show Octave where
--   show (Octave n)
--     | x == 4    = ""
--     | x > 4     = replicate (x - 4) '\''
--     | x < 4     = replicate (abs (4 - x)) ','
--     | otherwise = error "Unreachable pattern in show for Octave. This should never happen."
--     where x = n 


-- instance Read Octave where
--     readsPrec _ s
--       | null s         = [(Octave 4, "")]
--       | head s == '\'' = [(Octave (4 + length prefix), rest)]
--       | head s == ','  = [(Octave (4 - length prefix), rest)]
--       | otherwise      = [] -- fallback case
--       where 
--         (prefix, rest) = span (== head s) s

{- 
newtype Octave = Octave { unOctave :: Int }
  deriving (Eq, Show, Num, Bounded)

createOctave :: Int -> Maybe Octave
createOctave oct
  | oct >= minBound && oct <= maxBound = Just (Octave oct)
  | otherwise = Nothing
 -}
createOctave :: Int -> Octave
createOctave     = Octave 

-- mkOct

data Pitch = Pitch
  { _pitchClass :: PitchClass,
    _octave :: Octave
  } deriving (Show)
 

makeLenses ''Pitch

instance Ord Pitch where
  compare :: Pitch -> Pitch -> Ordering
  compare p1 p2 = compare (pitchVal''' p1) (pitchVal''' p2)

instance Eq Pitch where
  p1 == p2 =  pitchVal p1 == pitchVal p2

-- instance Show Pitch where
--   show p = show (_pitchClass p)  ++ show (_octave p)

-- instance Read Pitch where
--     readsPrec _ s = 
--         [ (Pitch pitchClass octave, rest2) 
--         | (pitchClassStr, rest1) <- lex s
--         , pitchClass <- [read pitchClassStr]
--         , (octaveStr, rest2) <- lex rest1
--         , octave <- [read octaveStr]
--         ]

-- instance Read Pitch where
--     readsPrec _ s = 
--         [ (Pitch pc oct, rest2)
--         | (pcStr, rest1) <- lex s
--         , (octStr, rest2) <- span (`elem` ",'") rest1
--         , pc <- [read pcStr :: PitchClass]
--         , oct <- [read octStr :: Octave]
--         ]



-- instance Read Pitch where
--     readsPrec _ s = 
--         [ (Pitch pitchClass octave, rest2) 
--         | (pitchClassStr, rest1) <- lex s
--         , pitchClass <- [read pitchClassStr]
--         , (octaveStr, rest2) <- lex rest1
--         , octave <- [read octaveStr]
--         ]


class HasPitch a where
  pitch :: a -> Pitch
  modifyPitch :: (Pitch -> Pitch) -> a -> a

instance HasPitch Pitch where
  pitch = id  -- Identity function, returns the same pitch
  modifyPitch f p = f p
--   fifthUp p = modifyPitch (transpose 7) p 




-- Assuming you have defined NoteName, createPitchClass, and alterationFromValue functions



-- class HasPitch a where
--   pitch :: a -> PitchVal

--  pitch = pitchVal

(=~) :: (HasPitch a) => a -> a -> Bool
a =~ b = pitch a == pitch b

-- instance HasPitch Pitch where
--   pitch = id

createPitch :: PitchClass -> Int ->  Pitch
createPitch pc oct = Pitch pc (Octave oct)

mkP :: PitchClass -> Int ->  Pitch
mkP pc oct = Pitch pc (Octave oct)


mkPitch :: PitchClass -> Int -> Pitch
mkPitch = createPitch

isValidPitchClass :: PitchClass -> Bool
isValidPitchClass pc = (_noteName pc)  `elem` [C, D, E, F, G, A, B]

-- isValidOctave :: Int -> Bool
-- isValidOctave oct = oct >= -1 && oct <= 10

-- octaveVal :: Octave -> Int
-- octaveVal (Octave oct) = (oct - 4) * 12

class HasOctave a where
  getOctave :: a -> Octave
  setOctave :: Octave -> a -> a

octaveUp :: (HasOctave a, Num Octave) => a -> a
octaveUp pitch = setOctave (getOctave pitch + 1) pitch

octaveDown :: (HasOctave a, Num Octave) => a -> a
octaveDown pitch = setOctave (getOctave pitch - 1) pitch

instance HasOctave Pitch where
  getOctave = _octave
  setOctave octave pitch = pitch { _octave = octave }






isValidOctave :: Int -> Bool
isValidOctave oct = oct >= -1 && oct <= 10

octaveVal :: Octave -> Int
octaveVal (Octave oct) = (oct - 4) * 12


pitchVal''' :: Pitch -> Rational
pitchVal''' pitch_ = pcVal + fromIntegral octVal
  where
    pcVal = pitchClassVal (pitch_ ^. pitchClass)
    octVal = octaveVal (pitch_ ^. octave)


pitchVal :: Pitch -> PitchVal
pitchVal pitch_ = pcVal + fromIntegral octVal
  where
    pcVal = pitchClassVal (pitch_ ^. pitchClass)
    octVal = octaveVal (pitch_ ^. octave)

-- pitch :: a -> PitchVal
-- pitch = pitchVal

-- pitchVal' :: Pitch -> (Int, Pitch.Accidental.Rq)
-- pitchVal' pitch_ = properFraction (pcVal + fromIntegral octVal)
--   where
--     pcVal = pitchClassVal (pitch_ ^. pitchClass)
--     octVal = octaveVal (pitch_ ^. octave)

{- 
>>> gts6 = createPitch' G twelfthSharp 6
>>> pitchVal' gts6

 -}




createPitch' :: NoteName -> Pitch.Accidental.Accidental -> Int -> Pitch
createPitch' pc acc = createPitch (createPitchClass pc acc)



-- mkPC
mkPC :: NoteName -> Pitch.Accidental.Accidental -> PitchClass
mkPC n acc = PitchClass {_noteName = n, _accidental = acc}

-- data PitchInput = PitchInput NoteName Accidental

-- class MkPC a => PitchClass where
--   mkPC :: a -> PitchClass

-- instance MkPC (NoteName, Accidental) where
--   mkPC (n, acc) = PitchClass {_noteName = n, _accidental = acc}


-- instance MkPC PitchInput where
--   mkPC (PitchInput n acc) = PitchClass {_noteName = n, _accidental = acc}

-- data PitchInput = PitchInput String String

-- instance MkPC PitchInput where
--   mkPC (PitchInput n acc) = case (readMaybe n, readMaybe acc) of
--                               (Just noteName, Just accidental) -> PitchClass {_noteName = noteName, _accidental = accidental}
--                               _ -> error "Invalid input"



-- -- Arbitrary instance for NoteName
-- instance Arbitrary NoteName where
--   arbitrary = Test.QuickCheck.elements [C, D, E, F, G, A, B]

-- -- Arbitrary instance for Accidental
-- instance Arbitrary Pitch.Accidental.Accidental where
--   arbitrary :: Gen Pitch.Accidental.Accidental
--   arbitrary = Test.QuickCheck.elements Pitch.Accidental.accidentals

-- -- Arbitrary instance for Octave
-- instance Arbitrary Octave where
--   arbitrary = Octave <$> choose (2, 8)

-- -- Generate a PitchClass with arbitrary NoteName and Accidental
-- instance Arbitrary PitchClass where
--   arbitrary = PitchClass <$> arbitrary <*> arbitrary

-- -- Generate a Pitch with arbitrary PitchClass and Octave
-- instance Arbitrary Pitch where
--   arbitrary = Pitch <$> arbitrary <*> arbitrary

-- instance Arbitrary PitchVal where
--     arbitrary = PitchVal <$> arbitrary


-- allAccidentals :: [Pitch.Accidental.Accidental]
-- allAccidentals = [Pitch.Accidental.sharp, Pitch.Accidental.flat, Pitch.Accidental.natural, Pitch.Accidental.quarterFlat, Pitch.Accidental.quarterSharp]

allPitchClasses :: [PitchClass]
allPitchClasses = [mkPC note acc | note <- [C .. B], acc <- accidentals]

allPitches :: [Pitch]
allPitches = [createPitch pc oct | pc <- allPitchClasses, oct <- [1.. 9]]

-- Define your lookup table
pitchValToPitchMap :: MapS.Map PitchVal Pitch
pitchValToPitchMap = MapS.fromList [(pitchVal p, p) | p <- allPitches]

-- Function to convert from PitchVal to Pitch using the lookup table
pitchValtoPitch :: PitchVal -> Maybe Pitch
pitchValtoPitch pv = MapS.lookup pv pitchValToPitchMap


pitchValToPitchesMap :: MapS.Map PitchVal [Pitch]
pitchValToPitchesMap = MapS.fromListWith (++) [(pitchVal p, [p]) | p <- allPitches]

-- Function to convert from PitchVal to list of corresponding pitches
pitchValToPitches :: PitchVal -> [Pitch]
pitchValToPitches pv = MapS.findWithDefault [] pv pitchValToPitchesMap


-- Function to check if an Accidental is natural
isNatural :: Accidental -> Bool
isNatural natural = True
isNatural _ = False

-- Function to compare two pitches based on the rules
comparePitches :: Pitch -> Pitch -> Ordering
comparePitches p1 p2
  | isNatural (acc1 p1) && isNatural (acc2 p2) = compare (absSemitones1 p1) (absSemitones2 p2)
  | isNatural (acc1 p1) = LT
  | isNatural (acc2 p2) = GT
  | otherwise = compare (absSemitones1 p1) (absSemitones2 p2)
  where
    acc1 = _accidental . _pitchClass
    acc2 = _accidental . _pitchClass
    absSemitones1 = abs . _accSemitones . acc1
    absSemitones2 = abs . _accSemitones . acc2


-- Function to determine the priority of an accidental
accidentalPriority :: Accidental -> Int
accidentalPriority natural = 0
accidentalPriority sharp = 1
accidentalPriority flat = 2
accidentalPriority quarterSharp = 3
accidentalPriority quarterFlat = 4
accidentalPriority _ = 5



-- Function to compare two pitches based on the rules
comparePitches' :: Pitch -> Pitch -> Ordering
comparePitches' p1 p2
  | accPriority1 /= accPriority2 = compare accPriority1 accPriority2
  | otherwise = compare (absSemitones1 p1) (absSemitones1 p2)
  where
    accPriority1 = accidentalPriority $ _accidental $ _pitchClass p1
    accPriority2 = accidentalPriority $ _accidental $ _pitchClass p2
    absSemitones1 = abs . _accSemitones . _accidental . _pitchClass
    absSemitones2 = abs . _accSemitones . _accidental . _pitchClass

-- Function to select a pitch based on the rules
selectPitch' :: [Pitch] -> Pitch
selectPitch' = minimumBy comparePitches
-- Function to select a pitch based on the rules
selectPitch :: [Pitch] -> Pitch
selectPitch = minimumBy comparePitches

-- Function to select one pitch based on rules
-- selectPitch :: [Pitch] -> Pitch
-- selectPitch = minimumBy (comparing (\p -> (_accSemitones . _accidental . _pitchClass) p))

pitchValToSelectedPitch :: PitchVal -> Pitch
pitchValToSelectedPitch pv = selectPitch $ pitchValToPitches pv

pitchValToSelectedPitch':: PitchVal -> Pitch
pitchValToSelectedPitch' pv = selectPitch' $ pitchValToPitches pv


-- enharmonicPair :: Gen (Pitch, Pitch)
-- enharmonicPair = do
--     pitch1 <- arbitrary
--     let possibleEnharmonics = filter (\p -> p /= pitch1 && pitchVal p == pitchVal pitch1) allPitches
--     if null possibleEnharmonics
--        then enharmonicPair  -- recursively try again
--        else do
--            pitch2 <- Test.QuickCheck.elements possibleEnharmonics
--            return (pitch1, pitch2)


-- -- -- A generator for a pair of enharmonic pitches
-- -- enharmonicPair :: Gen (Pitch, Pitch)
-- -- enharmonicPair = do
-- --     pitch1 <- arbitrary
-- --     let possibleEnharmonics = filter (\p -> p /= pitch1 && pitchVal p == pitchVal pitch1) allPitches
-- --     if null possibleEnharmonics
-- --        then enharmonicPair  -- recursively try again
-- --        else do
-- --            pitch2 <- Test.QuickCheck.elements possibleEnharmonics
-- --            return (pitch1, pitch2)

-- prop_EnharmonicEqualityCustom :: Property
-- prop_EnharmonicEqualityCustom = forAll enharmonicPair $ \(p1, p2) -> p1 =~ p2

-- prop_EnharmonicEqualityModified :: Pitch -> Pitch -> Bool
-- prop_EnharmonicEqualityModified p1 p2
--     | p1 /= p2 && pitchVal p1 == pitchVal p2 = p1 =~ p2
--     | otherwise = False



-- showPitch :: Pitch -> String
-- showPitch pitch = show (_noteName $ _pitchClass pitch) ++ " " ++ show (_accName $ (_accidental $ _pitchClass pitch))


-- prop_EnharmonicEquality :: Pitch -> Pitch -> Property
-- prop_EnharmonicEquality p1 p2 =
--     (p1 /= p2 && pitchVal p1 == pitchVal p2) ==> (p1 =~ p2)

-- -- You can add more properties if required...


-- -- Modify the property to return a more detailed result using the showPitch function
-- prop_EnharmonicEqualityCustomVerbose :: Property
-- prop_EnharmonicEqualityCustomVerbose = forAll enharmonicPair $ \(p1, p2) ->
--     counterexample (showPitch p1 ++ " and " ++ showPitch p2) (p1 =~ p2)



-- main :: IO ()
-- main = do
--     verboseCheckWith (stdArgs {maxSuccess = 1000}) prop_EnharmonicEqualityCustomVerbose






{- 

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}

import Control.Monad
import Language.Haskell.TH


data PitchClass = ... deriving (Lift, ...)

pitchClasses :: [(String, PitchClass)]
pitchClasses =
  [ ("c", _c),
    ("cs", _cs),
    ("df", _df),
    ("d", _d),
    ("ds", _ds),
    ("ef", _ef),
    ("e", _e),
    ("f", _f),
    ("fs", _fs),
    ("g", _g),
    ("gs", _gs),
    ("af", _af),
    ("a", _a),
    ("as", _as),
    ("bf", _bf),
    ("b", _b)
  ]

octaves :: [Int]
octaves = [-4 .. 4] -- Adjust according to your requirement

-- Function to create pitch names
pitchName :: String -> Int -> String
pitchName name oct
  | oct < 0 = name ++ replicate (abs oct) '_'
  | oct > 0 = name ++ replicate oct '\''
  | otherwise = name

generatePitches :: Q [Dec]
generatePitches =
  concat
    <$> forM
      octaves
      ( \oct ->
          forM
            pitchClasses
            ( \(name, pc) -> do
                let fname = mkName (pitchName name oct)
                let expr = [|mkPitch pc oct|]
                return (ValD (VarP fname) (NormalB expr) [])
            )
      )

$(generatePitches)


-}


-- abrev :: Traversal' Accidental String
-- abrev f acc = fmap (\newAbbrev -> acc & abbreviation .~ newAbbrev) (f (acc ^. abbreviation))

-- changePitchSemitones :: Rational -> Pitch -> Pitch
-- changePitchSemitones semis pitch =
--   let pc = pitch ^. pitchClass
--       oct = pitch ^. octave
--       newPC = changePitchClassSemitones semis pc
--       newOct = changeOctaveSemitones semis oct
--    in createPitch (fromMaybe pc newPC) (fromMaybe oct newOct)

-- changePitchClassSemitones :: Rational -> PitchClass -> Maybe PitchClass
-- changePitchClassSemitones semis pc = do
--   let currentSemitones = pc ^. accidental . semitone
--       newSemitones = currentSemitones + semis
--       acc = pc ^. accidental
--       newAcc = acc & semitone .~ newSemitones
--    in Just $ pc & accidental .~ newAcc

-- changeOctaveSemitones :: Rational -> Octave -> Maybe Octave
-- changeOctaveSemitones semis oct = do
--   let currentOctave = oct
--       newOctave = currentOctave + fromIntegral (truncate semis `div` 12)
--    in createOctave newOctave

--

{-
createPitch :: PitchClass -> Int -> Maybe Pitch
createPitch pc o = do
  oct <- createOctave o
  return $ Pitch pc oct

 -}

{-

c = createPitchClass C natural

pitchClassVal c
-- 0 % 1

fs = createPitchClass F sharp

pitchClassVal fs
-- 6 % 1

fqs = createPitchClass F quarterSharp

pitchClassVal fqs
-- 11 % 2

middleC = createPitch c 4 
fSharp5 = createPitch fs 5
-- Pitch {_pitchClass = PitchClass {_noteName = F, _accidental = Accidental {_accName = Sharp, _accAbbreviation = "s", _accArrow = Nothing, _accSemitones = 1 % 1}}, _octave = Octave 5}

pitchVal middleC
-- 0 % 1

pitchVal fSharp5
-- 18 % 1
 -}



{- 
pitch middleC

c = createPitchClass C natural

fSharp = createPitchClass F sharp

middleC = createPitch' C natural 4

middleC ^.  semitone

updatedPitchClass :: PitchClass
updatedPitchClass = c & semitone .~ 4.5

updatedPitchClass :: PitchClass
updatedPitchClass = c & accidental . semitone .~ 4.5


updatedPitch :: Pitch 
updatedPitch = middleC @ semitone .~ 4.5

-- Accessing fields using lenses
cName = c ^. noteName 
-- C

cAcc = c ^. accidental
-- Accidental {_accName = Natural, _accAbbreviation = "", _accArrow = Nothing, _accSemitones = 0 % 1}

fSharpAccSemitones = fSharp ^. accidental ^. semitone
-- 1 % 1

middleCOctave = middleC ^. octave
-- Octave 4

-- Modifying fields using lenses
modifiedC = c & noteName .~ D
-- PitchClass {_noteName = D, _accidental = Accidental {_accName = Natural, _accAbbreviation = "", _accArrow = Nothing, _accSemitones = 0 % 1}}

modifiedC ^. noteName
-- D

modifiedFS = fSharp & accidental . semitone .~ 2
-- PitchClass {_noteName = F, _accidental = Accidental {_accName = DoubleSharp, _accAbbreviation = "ss", _accArrow = Nothing, _accSemitones = 2 % 1}}

modifiedMiddleC = middleC & octave %~ (+ 1)
-- Pitch {_pitchClass = PitchClass {_noteName = C, _accidental = Accidental {_accName = Natural, _accAbbreviation = "", _accArrow = Nothing, _accSemitones = 0 % 1}}, _octave = Octave 5}

-- Composing lenses for nested access
modifiedMiddleCName = middleC & pitchClass . noteName .~ D
-- Pitch {_pitchClass = PitchClass {_noteName = D, _accidental = Accidental {_accName = Natural, _accAbbreviation = "", _accArrow = Nothing, _accSemitones = 0 % 1}}, _octave = Octave 4}

modifiedMiddleCAccSemitones = middleC & pitchClass . accidental . semitone %~ (+ 1.5)
-- Pitch {_pitchClass = PitchClass {_noteName = C, _accidental = Accidental {_accName = ThreeQuartersSharp, _accAbbreviation = "tqs", _accArrow = Nothing, _accSemitones = 3 % 2}}, _octave = Octave 4}

-- Chaining lenses with the (.~) operator
modifiedMiddleCNameAcc = middleC & pitchClass . noteName .~ D & pitchClass . accidental .~ flat
-- Pitch {_pitchClass = PitchClass {_noteName = D, _accidental = Accidental {_accName = Flat, _accAbbreviation = "f", _accArrow = Nothing, _accSemitones = (-1) % 1}}, _octave = Octave 4}

 -}


-- pitchValtoPitch :: PitchVal -> Pitch
-- pitchValtoPitch pv = 



{- class HasPitch a where
  pitchSemitones :: Lens' a Rational

instance HasPitch Pitch where
  pitchSemitones = lens getter setter
    where
      getter pitch = pitch ^. pitchClass . accidental . semitone
      setter pitch newSemitones = pitch & pitchClass . accidental %~ updateAccidental newSemitones
        where
          updateAccidental semis acc = acc & semitones .~ semisa

instance HasPitch PitchClass where
  pitchSemitones = accidental . semitone
 -}


-- pitchToLilyPond :: Pitch -> String
-- pitchToLilyPond pitch = noteNameStr ++ accidentalStr ++ octaveStr
--   where
--     noteNameStr = case _noteName (_pitchClass pitch) of
--       C -> "c"
--       D -> "d"
--       -- ... and so on for other note names
--     accidentalStr = case _accidental (_pitchClass pitch) of
--       Accidental Natural _ _ _ -> ""
--       Accidental Sharp _ _ _ -> "is"
--       Accidental Flat _ _ _ -> "es"
--       -- ... handle other accidentals here
--     octaveStr = replicate (fromIntegral (_octave pitch) - 4) '\''

-- ------------------- pitchToLilyPond :: Pitch -> String ------------------- --


pitchToLily pitch = noteNameStr ++ accidentalStr ++ octaveStr
  where
    noteNameStr = case _noteName (_pitchClass pitch) of
      C -> "c"
      D -> "d"
      E -> "e"
      F -> "f"
      G -> "g"
      A -> "a"
      B -> "b"
    accidentalStr = case _accidental (_pitchClass pitch) of
      Accidental Natural _ _ _ -> ""
      Accidental Sharp _ _ _ -> "is"
      Accidental Flat _ _ _ -> "es"
      Accidental QuarterSharp  _ _ _ -> "ih"
      Accidental QuarterFlat  _ _ _ -> "eh"
      _ -> "todo"
    octaveOffset =  (fromOctave $ _octave pitch) - 4 :: Int
    octaveStr = if octaveOffset > 0
                then replicate octaveOffset '\''
                else replicate (-octaveOffset) ','



class ToLilyString a where
    toLilyString :: a -> String

instance ToLilyString NoteName where
    toLilyString = \case
        C -> "c"
        D -> "d"
        E -> "e"
        F -> "f"
        G -> "g"
        A -> "a"
        B -> "b"

instance ToLilyString Octave where
    toLilyString (Octave n) = 
        let octaveOffset = n - 4
        in if octaveOffset > 0
           then replicate octaveOffset '\''
           else replicate (-octaveOffset) ','

instance ToLilyString Accidental where
    toLilyString = \case
        Accidental Natural _ _ _ -> ""
        Accidental Sharp _ _ _ -> "is"
        Accidental Flat _ _ _ -> "es"
        Accidental QuarterSharp  _ _ _ -> "ih"
        Accidental QuarterFlat  _ _ _ -> "eh"
        Accidental ThreeQuartersSharp  _ _ _ -> "isih"
        Accidental ThreeQuartersFlat  _ _ _ -> "eseh"
        _ -> "todo"


instance ToLilyString Pitch where
    toLilyString = \p -> (toLilyString $ _noteName $ _pitchClass p) ++ (toLilyString $ _accidental $ _pitchClass p) ++ (toLilyString $ _octave p)
