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

-- | 12 tone equal temperament semitone ratio. Equal to @2 ** (1/12)@.
semi :: Floating a => a
semi = 2 ** (1/12)

-- | 12 tone equal temperament ratios for all semitones in an octave.
allSemis :: Floating a => [a]
allSemis = map (semi **) . map fromIntegral $ [0..11 :: Int]

-- | List multiples of the single octave semitone ratios upto a certain amount.
takeFinAlignments :: Floating a => Int -> [[a]]
takeFinAlignments fin = map (\k -> map (*k) . map fromIntegral $ [1.. fin]) allSemis

-- | A pitch factor diagram is a list of prime exponents that represents a rational number
-- via 'diagramToRatio'. These are useful because pitches with few prime factors, that is,
-- small 'PitchFactorDiagram's with small factors in them, are generally consonant, and
-- many interesting just intonation intervals can be written this way (see 'Boopadoop.Interval.perfectFifth'
-- and 'Boopadoop.Interval.majorThird').
newtype PitchFactorDiagram = Factors {getFactors :: [Integer]} deriving Eq

instance Show PitchFactorDiagram where
  show pfd = show (diagramToRatio pfd) ++ " {" ++ show (getFactors pfd) ++ "}"

instance Ord PitchFactorDiagram where
  compare a b = compare (diagramToRatio a) (diagramToRatio b)

instance SummaryChar PitchFactorDiagram where
  sumUp pfd = "0123456789ab" !! ((`mod` 12) . floor $ diagramToSemi pfd)

-- | 'mempty' is the unison PFD, with ratio @1@.
instance Monoid PitchFactorDiagram where
  mempty = Factors []
  mappend = addPFD
-- | 'PitchFactorDiagram's are combined by multiplying their underlying ratios (adding factors).
instance Semigroup PitchFactorDiagram where
  (<>) = addPFD

-- | Convert a factor diagram to the underlying ratio by raising each prime (starting from two) to the power in the factor list. For instance, going up two perfect fifths and down three major thirds yields:
-- @
--  diagramToRatio (Factors [4,2,-3]) = (2 ^^ 4) * (3 ^^ 2) * (5 ^^ (-3)) = 144/125
-- @
diagramToRatio :: Fractional a => PitchFactorDiagram -> a
diagramToRatio = product . zipWith (^^) (map fromIntegral primes) . getFactors

-- | Similar to 'diagramToRatio', but simplifies the resulting ratio to the simplest ratio within @0.05@.
diagramToFloatyRatio :: PitchFactorDiagram -> Rational
diagramToFloatyRatio = flip approxRational 0.05 . diagramToRatio

-- | Convert a PFD to its decimal number of semitones. Useful for approximating weird ratios in a twelvetone scale:
-- @
--  diagramToSemi (normalizePFD $ Factors [0,0,0,1]) = diagramToSemi (countPFD (7/4)) = 9.688259064691248
-- @
diagramToSemi :: Floating a => PitchFactorDiagram -> a
diagramToSemi = (12 *) . logBase 2 . realToFrac . diagramToRatio . normalizePFD

-- | Normalize a PFD by raising or lowering it by octaves until its ratio lies between @1@ (unison) and @2@ (one octave up).
-- This operation is idempotent.
normalizePFD :: PitchFactorDiagram -> PitchFactorDiagram
normalizePFD (Factors []) = Factors []
normalizePFD (Factors (_:xs)) = Factors $ (negate . floor . logBase 2 . realToFrac . diagramToRatio . Factors . (0:) $ xs) : xs

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

-- | Converts a PFD into an operation on frequencies. @'intervalOf' 'Boopadoop.Interval.perfectFifth' 'Boopadoop.concertA'@ is the just intonation E5.
intervalOf :: PitchFactorDiagram -> (Double -> Double)
intervalOf = (*) . (realToFrac . diagramToRatio)

-- | Scale a PFD by raising the underlying ratio to the given power. @'scalePFD' 2 'Boopadoop.Interval.perfectFifth' = 'addPFD' 'Boopadoop.Interval.octave' 'Boopadoop.Interval.majorSecond'@
scalePFD :: Integer -> PitchFactorDiagram -> PitchFactorDiagram
scalePFD lambda = Factors . map (*lambda) . getFactors

-- | Inverts a PFD. @'invertPFD' = 'scalePFD' (-1)@
invertPFD :: PitchFactorDiagram -> PitchFactorDiagram
invertPFD = scalePFD (-1)

-- | Adds two PFDs together by multiplying their ratios. @'addPFD' minorThird 'Boopadoop.Interval.majorThird' = 'Boopadoop.Interval.perfectFifth'@
addPFD :: PitchFactorDiagram -> PitchFactorDiagram -> PitchFactorDiagram
addPFD a b = Factors . map getSum $ salign (map Sum $ getFactors a) (map Sum $ getFactors b)

-- | Prints the natural numbers from the given value up to @128@, highlighting primes and powers of two.
-- Interesting musical intervals are build out of the relative distance of a prime between the two
-- nearest powers of two.
printTheSequence :: Int -> IO ()
printTheSequence k 
  | k > 128 = putStrLn ""
  | k .&. (k-1) == 0 = putStr ("|\n[" ++ show k ++ "]") >> printTheSequence (k+1)
  | isPrime k = putStr ("(" ++ show k ++ ")") >> printTheSequence (k+1)
  | otherwise = putStr " . " >> printTheSequence (k+1)

newtype Chord = Chord {getNotes :: Set.Set PitchFactorDiagram}

chordOf :: [PitchFactorDiagram] -> Chord
chordOf = Chord . Set.fromList

rebaseChord :: PitchFactorDiagram -> Chord -> Chord
rebaseChord p = onNotes (addPFD p)

chordPitches :: Chord -> [PitchFactorDiagram]
chordPitches = Set.toList . getNotes

onNotes :: (PitchFactorDiagram -> PitchFactorDiagram) -> Chord -> Chord
onNotes f (Chord c) = Chord $ Set.map f c

addPitch :: PitchFactorDiagram -> Chord -> Chord
addPitch p (Chord c) = Chord (Set.insert p c)

invChrd :: Int -> Chord -> Chord
invChrd 0 c = c
invChrd k (Chord c) = let (p,c') = Set.deleteFindMin c in invChrd (k-1) (Chord $ Set.insert (addPFD (Factors [1]) p) c')

instance Show Chord where
  show (Chord c) = unlines $ ["/="] ++ fmap show (Set.toList c) ++ ["\\="]

consonantHalfway :: PitchFactorDiagram -> PitchFactorDiagram -> PitchFactorDiagram
consonantHalfway x y = countPFDFuzzy $ num/denom
  where
    num = diagramToRatio y - diagramToRatio x
    --denom = sum $ zipWith (\a b -> fromIntegral a * log (fromIntegral b)) (zipWith (-) (getFactors y) (getFactors x ++ repeat 0)) primes
    denom = log $ diagramToRatio $ addPFD y (invertPFD x)
