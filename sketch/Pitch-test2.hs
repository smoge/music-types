import Data.Fixed
import Data.Function (on)
import Data.List (minimumBy)
import Data.Ratio

type NoteName = Char

type RationalNote = (NoteName, Rational)

noteNameToRational :: [RationalNote]
noteNameToRational =
  [ ('C', 0 % 1),
    ('D', 2 % 1),
    ('E', 4 % 1),
    ('F', 5 % 1),
    ('G', 7 % 1),
    ('A', 9 % 1),
    ('B', 11 % 1)
  ]

findClosestNote :: Rational -> NoteName
findClosestNote r =
  fst $ minimumBy (compare `on` (abs . (r -) . snd)) noteNameToRational

calculateDifference :: Rational -> Rational
calculateDifference r =
  r - snd (minimumBy (compare `on` (abs . (r -) . snd)) noteNameToRational)

type AccidentalName = String

type RationalAccidental = (AccidentalName, Rational)

accidentals :: [RationalAccidental]
accidentals =
  [ ("f", (-1) % 1),
    ("", 0 % 1),
    ("s", 1 % 1),
    ("qf", (-1) % 2),
    ("qs", 1 % 2),
    ("tqf", (-3) % 2),
    ("tqs", 3 % 2),
    ("ss", 2 % 1),
    ("ff", (-2) % 1),
    ("rs", 2 % 3),
    ("rf", (-2) % 3),
    ("xs", 1 % 3),
    ("xf", (-1) % 3),
    ("ts", 1 % 6),
    ("tf", (-1) % 6),
    ("es", 1 % 4),
    ("ef", (-1) % 4),
    ("fts", 5 % 6),
    ("ftf", (-5) % 6),
    ("sts", 7 % 6),
    ("stf", (-7) % 6),
    ("fxs", 5 % 3),
    ("fxf", (-5) % 3),
    ("ets", 11 % 6),
    ("etf", (-11) % 6),
    ("trf", (-4) % 3),
    ("trs", 4 % 3),
    ("tes", 3 % 4),
    ("tef", (-3) % 4)
  ]

findClosestAccidental :: Rational -> AccidentalName
findClosestAccidental r =
  fst $ minimumBy (compare `on` (abs . (r -) . snd)) accidentals

getClosestNoteAndAccidental :: Rational -> (NoteName, AccidentalName)
getClosestNoteAndAccidental r =
  let closestNote = findClosestNote mod12
      closestAccidental = findClosestAccidental (calculateDifference mod12)
      mod12 = rationalModulo12 r
   in (closestNote, closestAccidental)

rationalToOctave :: Rational -> Int
rationalToOctave r = (round (fromRational r :: Float) `div` 12) + 4

rationalModulo12 :: Rational -> Rational
rationalModulo12 r = r `mod'` 12

rationalToPitch :: Rational -> (Rational, NoteName, AccidentalName, Int)
rationalToPitch r =
  let (closestNote, closestAccidental) = getClosestNoteAndAccidental r
      octave = rationalToOctave r
   in (r, closestNote, closestAccidental, octave)

{-
ghci > map rationalToPitch [0 % 1, 1 % 6 .. 24 % 1]
[(0 % 1, 'C', "", 4), (1 % 6, 'C', "ts", 4), (1 % 3, 'C', "xs", 4), (1 % 2, 'C', "qs", 4), (2 % 3, 'C', "rs", 4), (5 % 6, 'C', "fts", 4), (1 % 1, 'C', "s", 4), (7 % 6, 'D', "ftf", 4), (4 % 3, 'D', "rf", 4), (3 % 2, 'D', "qf", 4), (5 % 3, 'D', "xf", 4), (11 % 6, 'D', "tf", 4), (2 % 1, 'D', "", 4), (13 % 6, 'D', "ts", 4), (7 % 3, 'D', "xs", 4), (5 % 2, 'D', "qs", 4), (8 % 3, 'D', "rs", 4), (17 % 6, 'D', "fts", 4), (3 % 1, 'D', "s", 4), (19 % 6, 'E', "ftf", 4), (10 % 3, 'E', "rf", 4), (7 % 2, 'E', "qf", 4), (11 % 3, 'E', "xf", 4), (23 % 6, 'E', "tf", 4), (4 % 1, 'E', "", 4), (25 % 6, 'E', "ts", 4), (13 % 3, 'E', "xs", 4), (9 % 2, 'E', "qs", 4), (14 % 3, 'F', "xf", 4), (29 % 6, 'F', "tf", 4), (5 % 1, 'F', "", 4), (31 % 6, 'F', "ts", 4), (16 % 3, 'F', "xs", 4), (11 % 2, 'F', "qs", 4), (17 % 3, 'F', "rs", 4), (35 % 6, 'F', "fts", 4), (6 % 1, 'F', "s", 4), (37 % 6, 'G', "ftf", 4), (19 % 3, 'G', "rf", 4), (13 % 2, 'G', "qf", 4), (20 % 3, 'G', "xf", 4), (41 % 6, 'G', "tf", 4), (7 % 1, 'G', "", 4), (43 % 6, 'G', "ts", 4), (22 % 3, 'G', "xs", 4), (15 % 2, 'G', "qs", 4), (23 % 3, 'G', "rs", 4), (47 % 6, 'G', "fts", 4), (8 % 1, 'G', "s", 4), (49 % 6, 'A', "ftf", 4), (25 % 3, 'A', "rf", 4), (17 % 2, 'A', "qf", 4), (26 % 3, 'A', "xf", 4), (53 % 6, 'A', "tf", 4), (9 % 1, 'A', "", 4), (55 % 6, 'A', "ts", 4), (28 % 3, 'A', "xs", 4), (19 % 2, 'A', "qs", 4), (29 % 3, 'A', "rs", 4), (59 % 6, 'A', "fts", 4), (10 % 1, 'A', "s", 4), (61 % 6, 'B', "ftf", 4), (31 % 3, 'B', "rf", 4), (21 % 2, 'B', "qf", 4), (32 % 3, 'B', "xf", 4), (65 % 6, 'B', "tf", 4), (11 % 1, 'B', "", 4), (67 % 6, 'B', "ts", 4), (34 % 3, 'B', "xs", 4), (23 % 2, 'B', "qs", 5), (35 % 3, 'B', "rs", 5), (71 % 6, 'B', "fts", 5), (12 % 1, 'C', "", 5), (73 % 6, 'C', "ts", 5), (37 % 3, 'C', "xs", 5), (25 % 2, 'C', "qs", 5), (38 % 3, 'C', "rs", 5), (77 % 6, 'C', "fts", 5), (13 % 1, 'C', "s", 5), (79 % 6, 'D', "ftf", 5), (40 % 3, 'D', "rf", 5), (27 % 2, 'D', "qf", 5), (41 % 3, 'D', "xf", 5), (83 % 6, 'D', "tf", 5), (14 % 1, 'D', "", 5), (85 % 6, 'D', "ts", 5), (43 % 3, 'D', "xs", 5), (29 % 2, 'D', "qs", 5), (44 % 3, 'D', "rs", 5), (89 % 6, 'D', "fts", 5), (15 % 1, 'D', "s", 5), (91 % 6, 'E', "ftf", 5), (46 % 3, 'E', "rf", 5), (31 % 2, 'E', "qf", 5), (47 % 3, 'E', "xf", 5), (95 % 6, 'E', "tf", 5), (16 % 1, 'E', "", 5), (97 % 6, 'E', "ts", 5), (49 % 3, 'E', "xs", 5), (33 % 2, 'E', "qs", 5), (50 % 3, 'F', "xf", 5), (101 % 6, 'F', "tf", 5), (17 % 1, 'F', "", 5), (103 % 6, 'F', "ts", 5), (52 % 3, 'F', "xs", 5), (35 % 2, 'F', "qs", 5), (53 % 3, 'F', "rs", 5), (107 % 6, 'F', "fts", 5), (18 % 1, 'F', "s", 5), (109 % 6, 'G', "ftf", 5), (55 % 3, 'G', "rf", 5), (37 % 2, 'G', "qf", 5), (56 % 3, 'G', "xf", 5), (113 % 6, 'G', "tf", 5), (19 % 1, 'G', "", 5), (115 % 6, 'G', "ts", 5), (58 % 3, 'G', "xs", 5), (39 % 2, 'G', "qs", 5), (59 % 3, 'G', "rs", 5), (119 % 6, 'G', "fts", 5), (20 % 1, 'G', "s", 5), (121 % 6, 'A', "ftf", 5), (61 % 3, 'A', "rf", 5), (41 % 2, 'A', "qf", 5), (62 % 3, 'A', "xf", 5), (125 % 6, 'A', "tf", 5), (21 % 1, 'A', "", 5), (127 % 6, 'A', "ts", 5), (64 % 3, 'A', "xs", 5), (43 % 2, 'A', "qs", 5), (65 % 3, 'A', "rs", 5), (131 % 6, 'A', "fts", 5), (22 % 1, 'A', "s", 5), (133 % 6, 'B', "ftf", 5), (67 % 3, 'B', "rf", 5), (45 % 2, 'B', "qf", 5), (68 % 3, 'B', "xf", 5), (137 % 6, 'B', "tf", 5), (23 % 1, 'B', "", 5), (139 % 6, 'B', "ts", 5), (70 % 3, 'B', "xs", 5), (47 % 2, 'B', "qs", 6), (71 % 3, 'B', "rs", 6), (143 % 6, 'B', "fts", 6), (24 % 1, 'C', "", 6)]

 -}