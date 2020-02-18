{-# LANGUAGE TypeApplications #-}
-- | Tools for creating and manipulation Pitch Factor Diagrams, a tool for representing musical 
-- intervals and examining their relations.
module Boopadoop.Diagram where

import Data.Ratio
import Data.Bits
import Data.Monoid
import Data.List
import qualified Data.Set as Set
import Data.Numbers.Primes
import Data.Align (salign)
import Boopadoop.Rhythm (SummaryChar(sumUp))

data TwelveTone = TwelveTone Int

instance Show TwelveTone where
  show p = ("0123456789ab" !! ttPitch p) : show (ttOctave p)

ttOctave :: TwelveTone -> Int
ttOctave (TwelveTone k) = k `div` 12

ttPitch :: TwelveTone -> Int
ttPitch (TwelveTone k) = k `mod` 12

ttScaleDown :: TwelveTone -> TwelveTone -> TwelveTone
ttScaleDown root p = ttMap (subtract delta) p
  where
    delta = case ttInterval root p of
      0 -> 1
      1 -> 1
      2 -> 2
      3 -> 1
      4 -> 2
      5 -> 1
      6 -> 1
      7 -> 2
      8 -> 1
      9 -> 2
      10 -> 1
      11 -> 2
      _ -> error "Too big of interval"


ttScaleUp :: TwelveTone -> TwelveTone -> TwelveTone
ttScaleUp root p = ttMap (+delta) p
  where
    delta = case ttInterval root p of
      0 -> 2
      1 -> 1
      2 -> 2
      3 -> 1
      4 -> 1
      5 -> 2
      6 -> 1
      7 -> 2
      8 -> 1
      9 -> 2
      10 -> 1
      11 -> 1
      _ -> error "Too big of interval"

ttMap :: (Int -> Int) -> TwelveTone -> TwelveTone
ttMap f (TwelveTone k) = TwelveTone (f k)

ttDiff :: TwelveTone -> TwelveTone -> TwelveTone
ttDiff (TwelveTone a) (TwelveTone b) = TwelveTone (a - b)

ttInterval :: TwelveTone -> TwelveTone -> Int
ttInterval a b = ttPitch b - ttPitch a


-- | 12 tone equal temperament semitone ratio. Equal to @2 ** (1/12)@.
semi :: Floating a => a
semi = 2 ** (1/12)

-- | 12 tone equal temperament ratios for all semitones in an octave.
allSemis :: Floating a => [a]
allSemis = map (semi **) . map fromIntegral $ [0..11 :: Int]

-- | List multiples of the single octave semitone ratios upto a certain amount.
takeFinAlignments :: Floating a => Int -> [[a]]
takeFinAlignments fin = map (\k -> map (*k) . map fromIntegral $ [1.. fin]) allSemis

newtype PitchClass = ClassFactors {getClassFactors :: [Integer]}

instance Eq PitchClass where
  (==) a b = (==) @PitchFactorDiagram (classInOctave 0 a) (classInOctave 0 b)

instance Ord PitchClass where
  compare a b = compare @PitchFactorDiagram (classInOctave 0 a) (classInOctave 0 b)

instance Show PitchClass where
  show pfd = take 5 (show (diagramToRatio @Double $ classInOctave 0 pfd) ++ repeat '0') ++ " {x," ++ (init . tail $ show (getClassFactors pfd)) ++ "}"

getPitchClass :: PitchFactorDiagram -> PitchClass
getPitchClass = ClassFactors . drop 1 . getFactors

classInOctave :: Integer -> PitchClass -> PitchFactorDiagram
classInOctave k = addPFD (scalePFD k octave) . normalizePFD . Factors . (0:) . getClassFactors

getOctave :: PitchFactorDiagram -> Integer
getOctave = floor . logBase 2 . diagramToRatio @Double

complPitchClass :: PitchClass -> PitchClass
complPitchClass = ClassFactors . fmap negate . getClassFactors

octave :: PitchFactorDiagram
octave = Factors [1]

-- | A pitch factor diagram is a list of prime exponents that represents a rational number
-- via 'diagramToRatio'. These are useful because pitches with few prime factors, that is,
-- small 'PitchFactorDiagram's with small factors in them, are generally consonant, and
-- many interesting just intonation intervals can be written this way (see 'Boopadoop.Interval.perfectFifth'
-- and 'Boopadoop.Interval.majorThird').
newtype PitchFactorDiagram = Factors {getFactors :: [Integer]}

instance Eq PitchFactorDiagram where
  (==) a b = (==) @Rational (diagramToRatio a) (diagramToRatio b)

instance Show PitchFactorDiagram where
  show pfd = take 5 (show (diagramToRatio @Double pfd) ++ repeat '0') ++ " {" ++ (concat . intersperse "," . fmap showSigned . take 5 $ getFactors pfd ++ repeat 0) ++ "}"
    where
      showSigned x = if x >= 0 then '+' : show x else show x

instance Ord PitchFactorDiagram where
  compare a b = compare @Rational (diagramToRatio a) (diagramToRatio b)

instance SummaryChar PitchFactorDiagram where
  sumUp pfd = "0123456789ab" !! ((`mod` 12) . round $ diagramToSemi @Double pfd)

-- | 'mempty' is the unison PFD, with ratio @1@.
instance Monoid PitchFactorDiagram where
  mempty = Factors []
  mappend = addPFD
-- | 'PitchFactorDiagram's are combined by multiplying their underlying ratios (adding factors).
instance Semigroup PitchFactorDiagram where
  (<>) = addPFD

pettyDissMeasure :: PitchFactorDiagram -> Integer
pettyDissMeasure = floor . diagramToRatio @Double . Factors . map abs . getFactors

-- | Convert a factor diagram to the underlying ratio by raising each prime (starting from two) to the power in the factor list. For instance, going up two perfect fifths and down three major thirds yields:
-- @
--  diagramToRatio (Factors [4,2,-3]) = (2 ^^ 4) * (3 ^^ 2) * (5 ^^ (-3)) = 144/125
-- @
diagramToRatio :: Fractional a => PitchFactorDiagram -> a
diagramToRatio = product . zipWith (^^) (map fromIntegral (primes @Int)) . getFactors

-- | Similar to 'diagramToRatio', but simplifies the resulting ratio to the simplest ratio within @0.05@.
diagramToFloatyRatio :: PitchFactorDiagram -> Rational
diagramToFloatyRatio = flip approxRational 0.05 . diagramToRatio @Double

-- | Convert a PFD to its decimal number of semitones. Useful for approximating weird ratios in a twelvetone scale:
-- @
--  diagramToSemi (normalizePFD $ Factors [0,0,0,1]) = diagramToSemi (countPFD (7/4)) = 9.688259064691248
-- @
diagramToSemi :: Floating a => PitchFactorDiagram -> a
diagramToSemi = (12 *) . logBase 2 . diagramToRatio

-- | Normalize a PFD by raising or lowering it by octaves until its ratio lies between @1@ (unison) and @2@ (one octave up).
-- This operation is idempotent.
normalizePFD :: PitchFactorDiagram -> PitchFactorDiagram
normalizePFD (Factors []) = Factors []
normalizePFD (Factors (_:xs)) = Factors $ (negate . floor . logBase 2 . diagramToRatio @Double . Factors . (0:) $ xs) : xs

-- | Same as 'countPFD' but makes an effort to simplify the ratio from a 'Double' slightly to the simplest rational number within @0.0001@.
countPFDFuzzy :: Double -> PitchFactorDiagram
countPFDFuzzy = countPFD . flip approxRational 0.01

-- | Calculates the 'PitchFactorDiagram' corresponding to a given frequency ratio by finding prime factors of the numerator and denominator.
countPFD :: Rational -> PitchFactorDiagram
countPFD k = Factors $ go (primeFactors $ numerator k,primeFactors $ denominator k) primes
  where
    count = (genericLength .) . filter
    go :: ([Integer],[Integer]) -> [Integer] -> [Integer]
    go ([],[]) _ = []
    go (nfs,dfs) (p:ps) = count (==p) nfs - count (==p) dfs : go (filter (/=p) nfs,filter (/=p) dfs) ps
    go _ [] = error "The primes ended"

-- | Converts a PFD into an operation on frequencies. @'intervalOf' 'Boopadoop.Interval.perfectFifth' 'Boopadoop.concertA'@ is the just intonation E5.
intervalOf :: PitchFactorDiagram -> (Double -> Double)
intervalOf = (*) . (diagramToRatio)

-- | Scale a PFD by raising the underlying ratio to the given power. @'scalePFD' 2 'Boopadoop.Interval.perfectFifth' = 'addPFD' 'Boopadoop.Interval.octave' 'Boopadoop.Interval.majorSecond'@
scalePFD :: Integer -> PitchFactorDiagram -> PitchFactorDiagram
scalePFD lambda = Factors . map (*lambda) . getFactors

-- | Inverts a PFD. @'invertPFD' = 'scalePFD' (-1)@
invertPFD :: PitchFactorDiagram -> PitchFactorDiagram
invertPFD = scalePFD (-1)

-- | Adds two PFDs together by multiplying their ratios. @'addPFD' minorThird 'Boopadoop.Interval.majorThird' = 'Boopadoop.Interval.perfectFifth'@
addPFD :: PitchFactorDiagram -> PitchFactorDiagram -> PitchFactorDiagram
addPFD a b = Factors . map getSum $ salign (map Sum $ getFactors a) (map Sum $ getFactors b)

makePFDGoUp :: PitchFactorDiagram -> PitchFactorDiagram
makePFDGoUp pfd = if pfd > Factors [] then pfd else invertPFD pfd

-- | Prints the natural numbers from the given value up to @128@, highlighting primes and powers of two.
-- Interesting musical intervals are build out of the relative distance of a prime between the two
-- nearest powers of two.
printTheSequence :: Int -> IO ()
printTheSequence k 
  | k > 128 = putStrLn ""
  | k .&. (k-1) == 0 = putStr ("|\n[" ++ show k ++ "]") >> printTheSequence (k+1)
  | isPrime k = putStr ("(" ++ show k ++ ")") >> printTheSequence (k+1)
  | otherwise = putStr " . " >> printTheSequence (k+1)

newtype ChordVoicing = ChordVoicing {getVoices :: Set.Set PitchFactorDiagram}

newtype Chord = Chord {getNotes :: Set.Set PitchClass}

instance SummaryChar ChordVoicing where
  sumUp = head . show . countVoices

chordSize :: Chord -> Int
chordSize = Set.size . getNotes

countVoices :: ChordVoicing -> Int
countVoices = Set.size . getVoices

chordOf :: [PitchClass] -> Chord
chordOf = Chord . Set.fromList

voiceChord :: [PitchFactorDiagram] -> ChordVoicing
voiceChord = ChordVoicing . Set.fromList

rebaseChord :: PitchFactorDiagram -> ChordVoicing -> ChordVoicing
rebaseChord p = onVoices (addPFD p)

chordOver :: PitchFactorDiagram -> Chord -> ChordVoicing
chordOver pfd = ChordVoicing . Set.map (addPFD pfd . classInOctave 0) . getNotes

chordPitches :: Chord -> [PitchClass]
chordPitches = Set.toList . getNotes

listVoices :: ChordVoicing -> [PitchFactorDiagram]
listVoices = Set.toList . getVoices

onVoices :: (PitchFactorDiagram -> PitchFactorDiagram) -> ChordVoicing -> ChordVoicing
onVoices f (ChordVoicing c) = ChordVoicing $ Set.map f c

onPitches :: (PitchClass -> PitchClass) -> Chord -> Chord
onPitches f (Chord c) = Chord $ Set.map f c

addPitch :: PitchClass -> Chord -> Chord
addPitch p (Chord c) = Chord (Set.insert p c)

invChrd :: Int -> ChordVoicing -> ChordVoicing
invChrd 0 c = c
invChrd k (ChordVoicing c) = let (p,c') = Set.deleteFindMin c in invChrd (k-1) (ChordVoicing $ Set.insert (addPFD (Factors [1]) p) c')

addPC :: PitchClass -> PitchClass -> PitchClass
addPC a b = ClassFactors . map getSum $ salign (map Sum $ getClassFactors a) (map Sum $ getClassFactors b)

{-
voiceChord :: [Int] -> Chord -> Chord
voiceChord voicing c = if maximum voicing < chordSize c
  then let ps = Set.toList (getNotes c) in Chord . foldl z Set.empty . map (\k -> ps !! k) $ voicing
  else error $ "voiceChord " ++ show voicing ++ " called on chord " ++ show c ++ " of size " ++ show (chordSize c)
  where
    z s p = if (\m -> case m of {Just _ -> True; Nothing -> False}) (Set.lookupGE p s) then z s (addPFD (Factors [1]) p) else Set.insert p s
-}

chordRoot :: Chord -> PitchClass
chordRoot = Set.findMin . getNotes
{-
closedFPH :: Int -> Chord -> Chord
closedFPH = closedFPHOver 0

closedFPHOver :: Int -> Int -> Chord -> Chord
closedFPHOver bass ov c = if chordSize c >= 3
  then voiceChord [bass,ov,(ov + 1) `mod` 3, (ov + 2) `mod` 3] c
  else error $ "closedFPHOver chord of size " ++ show (chordSize c)
-}
instance Show Chord where
  show c = "<" ++ (init . tail . show . chordPitches $ c) ++ ">"

prettyShowChord :: Chord -> String
prettyShowChord (Chord c) = init . unlines $ ["/="] ++ fmap show (Set.toList c) ++ ["\\="]

consonantHalfway :: PitchFactorDiagram -> PitchFactorDiagram -> PitchFactorDiagram
consonantHalfway x y = countPFDFuzzy $ num/denom
  where
    num = diagramToRatio y - diagramToRatio x
    --denom = sum $ zipWith (\a b -> fromIntegral a * log (fromIntegral b)) (zipWith (-) (getFactors y) (getFactors x ++ repeat 0)) primes
    denom = log $ diagramToRatio $ addPFD y (invertPFD x)

chordCeil :: PitchFactorDiagram -> Chord -> PitchFactorDiagram
chordCeil p c = let (_l,g) = Set.split pc (getNotes c) in case Set.lookupMin g of
  Just g' -> classInOctave (getOctave p) g'
  Nothing -> classInOctave (getOctave p + 1) $ Set.findMin (getNotes c)
  where
    pc = getPitchClass p

chordFloor :: PitchFactorDiagram -> Chord -> PitchFactorDiagram
chordFloor p c = let (l,_g) = Set.split pc (getNotes c) in case Set.lookupMax l of
  Just l' -> classInOctave (getOctave p) l'
  Nothing -> classInOctave (getOctave p - 1) $ Set.findMax (getNotes c)
  where
    pc = getPitchClass p
