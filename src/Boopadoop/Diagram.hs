{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
module Boopadoop.Diagram where

import Data.Ratio
import Data.Monoid
import Data.List
import qualified Data.Set as Set
import Data.Numbers.Primes
import Data.Align (salign)
import Boopadoop.TimeStream (SummaryChar(sumUp))

-- | Represents a note in an arbitrary twelve-toned scale.
newtype TwelveTone = MkTwelveTone 
  {getTTNum :: Int -- ^ Always in [0..11]
  } deriving (Eq,Num)

instance Monoid TwelveTone where
  mempty = twelveTone 0

-- | Interval addition modulo octaves
instance Semigroup TwelveTone where
  (<>) = addOctave

instance Monoid (Octaved TwelveTone) where
  mempty = inOctave 0 $ twelveTone 0

-- | Interval addition respecting octaves
instance Semigroup (Octaved TwelveTone) where
  a <> b = let (o,p) = (getTTNum (getPitchClass a) + getTTNum (getPitchClass b)) `divMod` 12 in Octaved (o + getOctave a + getOctave b) (twelveTone p)

instance Ord TwelveTone where
 compare a b = compare (getTTNum a) (getTTNum b)

-- | Smart constructor for @'TwelveTone'@ accepting only values in [0..11]
twelveTone :: Int -> TwelveTone
twelveTone k = if k `elem` [0..11] then MkTwelveTone k else error $ "twelveTone " ++ show k ++ " not in range"

-- | Convert from an octaved twelve tone to the corresponding non-octaved integer number of tones about the root
ttDeoctave :: Octaved TwelveTone -> Int
ttDeoctave ott = 12 * getOctave ott + getTTNum (getPitchClass ott)

instance SummaryChar TwelveTone where
  sumUp t = if getTTNum t < 0 || getTTNum t > 11 then error "SummaryChar improper TwelveTone" else "0123456789ab" !! getTTNum t

instance SummaryChar (Octaved TwelveTone) where
  sumUp (Octaved _ t) = sumUp t

instance Show TwelveTone where
  show = (:[]) . sumUp

instance Show (Octaved TwelveTone) where
  show (Octaved k t) = "Octaved " ++ show k ++ "; " ++ show t

-- | Produce a twelve toned note from a solfeck pitch symbol
ttFromSolfeck :: Char -> Maybe TwelveTone
ttFromSolfeck = fmap twelveTone . (`elemIndex` "0123456789ab")

-- | Invert a twelve tone pitch. Has @t <> 'invTT' t = 'mempty'@
invTT :: TwelveTone -> TwelveTone
invTT = twelveTone . (`mod` 12) . negate . getTTNum

-- | The major scale in chord form
ttMajor :: Chord TwelveTone
ttMajor = chordOf . fmap twelveTone $ [0,2,4,5,7,9,11]

-- | Go one position down the twelve tone major scale
ttScaleDown :: TwelveTone -> Octaved TwelveTone -> Octaved TwelveTone
ttScaleDown root p = (inOctave (-1) $ invTT delta) <> p
  where
    delta = twelveTone $ case ttDiff (getPitchClass p) root of
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

-- | Go one position up the twelve tone major scale
ttScaleUp :: TwelveTone -> Octaved TwelveTone -> Octaved TwelveTone
ttScaleUp root p = inOctave 0 (twelveTone delta) <> p
  where
    delta = case ttDiff (getPitchClass p) root of
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
ttMap f = twelveTone . (`mod` 12) . f . getTTNum

ttDiff :: TwelveTone -> TwelveTone -> Int
ttDiff a b = (getTTNum a - getTTNum b) `mod` 12

ttRelPitch :: TwelveTone -> Octaved TwelveTone -> Int
ttRelPitch r p = ttDiff r (getPitchClass p)

ttInterval :: Octaved TwelveTone -> Octaved TwelveTone -> Int
ttInterval a b = 12 * (getOctave b - getOctave a) + (getTTNum . getPitchClass) b - (getTTNum . getPitchClass) a

-- | 12 tone equal temperament semitone ratio. Equal to @2 ** (1/12)@.
semi :: Floating a => a
semi = 2 ** (1/12)

-- | 12 tone equal temperament ratios for all semitones in an octave.
allSemis :: Floating a => [a]
allSemis = map (semi **) . map fromIntegral $ [0..11 :: Int]

-- | List multiples of the single octave semitone ratios upto a certain amount.
takeFinAlignments :: Floating a => Int -> [[a]]
takeFinAlignments fin = map (\k -> map (*k) . map fromIntegral $ [1.. fin]) allSemis

inOctave :: Int -> a -> Octaved a
inOctave = Octaved
--addPFD (scalePFD k octave) . normalizePFD . Factors . (0:) . getClassFactors

data Octaved a = Octaved {getOctave :: Int, getPitchClass :: a} deriving (Functor,Eq)

instance Ord a => Ord (Octaved a) where
  compare (Octaved oa pa) (Octaved ob pb) = case compare oa ob of
    EQ -> compare pa pb
    x -> x

addOctave :: Semigroup (Octaved a) => a -> a -> a
addOctave a b = getPitchClass (inOctave 0 a <> inOctave 0 b)

complPitchClass :: PitchFactorDiagram -> PitchFactorDiagram
complPitchClass = Factors . fmap negate . getFactors

octave :: Monoid a => Octaved a
octave = inOctave 1 mempty

shiftOctave :: Int -> Octaved a -> Octaved a
shiftOctave dk (Octaved k a) = Octaved (k + dk) a

-- | A pitch factor diagram is a factorized representation of a rational number in [1,2).
-- See 'diagramToRatio'.
newtype PitchFactorDiagram = Factors {getFactors :: [Integer]}

instance Eq PitchFactorDiagram where
  (==) a b = (==) @Rational (diagramToRatio a) (diagramToRatio b)

instance Show PitchFactorDiagram where
  show pfd = take 5 (show (diagramToRatio @Double pfd) ++ repeat '0') ++ " {" ++ (concat . intersperse "," . fmap showSigned . take 5 $ getFactors pfd ++ repeat 0) ++ "}"
    where
      showSigned x = if x >= 0 then '+' : show x else show x

instance Show (Octaved PitchFactorDiagram) where
  show (Octaved k pfd) = "Octaved " ++ show k ++ "; " ++ show pfd

instance Ord PitchFactorDiagram where
  compare a b = compare @Rational (diagramToRatio a) (diagramToRatio b)

instance SummaryChar PitchFactorDiagram where
  sumUp pfd = if i < 0 || i > 63 then error "Bad PFD" else "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" !! i
    where
      i = round . (*64) . subtract 1 $ diagramToRatio @Double pfd

-- | 'mempty' is the unison PFD, with ratio @1@.
instance Monoid PitchFactorDiagram where
  mempty = Factors []

instance Monoid (Octaved PitchFactorDiagram) where
  mempty = inOctave 0 $ Factors []

instance Semigroup PitchFactorDiagram where
  (<>) = addOctave

-- | 'PitchFactorDiagram's are combined by multiplying their underlying ratios (adding factors).
instance Semigroup (Octaved PitchFactorDiagram) where
  oa <> ob = let pfd = map getSum $ salign (map Sum . getFactors . getPitchClass $ oa) (map Sum . getFactors . getPitchClass $ ob) in Octaved (getOctave oa + getOctave ob + getPFDNativeOctave (Factors pfd)) (Factors pfd)

pettyDissMeasure :: PitchFactorDiagram -> Integer
pettyDissMeasure = floor . diagramToRatio @Double . Factors . map abs . getFactors

-- | Convert a factor diagram to the underlying ratio by raising each prime (starting from three) to the power in the factor list. For instance:
-- @
--  diagramToRatio (Factors [2,-3]) = (3 ^^ 2) * (5 ^^ (-3)) = 9/125
-- @
--
-- Has codomain [1,2)
diagramToRatio :: Fractional a => PitchFactorDiagram -> a
diagramToRatio pfd = (2 ^^ negate (getPFDNativeOctave pfd)) * rawPFDRatio pfd

rawPFDRatio :: Fractional a => PitchFactorDiagram -> a
rawPFDRatio = product . zipWith (^^) (map fromIntegral (drop 1 $ primes @Int)) . getFactors

ratioBetween :: Fractional a => PitchFactorDiagram -> PitchFactorDiagram -> a
ratioBetween a b = diagramToRatio a / diagramToRatio b

-- | Gets the octave that the pitch factor diagram would be in if we didn't normalize it back to [1,2)
getPFDNativeOctave :: PitchFactorDiagram -> Int
getPFDNativeOctave = floor . logBase 2 . rawPFDRatio @Double

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
intervalOf :: Octaved PitchFactorDiagram -> (Double -> Double)
intervalOf pfd = (*) (2 ^^ getOctave pfd * diagramToRatio (getPitchClass pfd))

-- | Scale a PFD by raising the underlying ratio to the given power.
scalePFD :: Integer -> PitchFactorDiagram -> PitchFactorDiagram
scalePFD lambda = Factors . map (*lambda) . getFactors

-- | Inverts a PFD. @'invertPFD' = 'scalePFD' (-1)@. For instance, a perfect fifth will invert into a perfect fourth
invertPFD :: PitchFactorDiagram -> PitchFactorDiagram
invertPFD = scalePFD (-1)

-- | Adds two PFDs together by multiplying their ratios. @'addPFD' minorThird majorThird = 'Boopadoop.Interval.perfectFifth'@
-- Addition is performed modulo octaves
addPFD :: PitchFactorDiagram -> PitchFactorDiagram -> PitchFactorDiagram
addPFD a b = Factors . map getSum $ salign (map Sum $ getFactors a) (map Sum $ getFactors b)

makePFDGoUp :: Octaved PitchFactorDiagram -> Octaved PitchFactorDiagram
makePFDGoUp (Octaved k pfd) = if k >= 0 then Octaved k pfd else Octaved (negate k) (invertPFD pfd)

-- | Prints the natural numbers from the given value up to @128@, highlighting primes and powers of two.
-- Interesting musical intervals are build out of the relative distance of a prime between the two
-- nearest powers of two.
{-
printTheSequence :: Int -> IO ()
printTheSequence k 
  | k > 128 = putStrLn ""
  | k .&. (k-1) == 0 = putStr ("|\n[" ++ show k ++ "]") >> printTheSequence (k+1)
  | isPrime k = putStr ("(" ++ show k ++ ")") >> printTheSequence (k+1)
  | otherwise = putStr " . " >> printTheSequence (k+1)
-}

type ChordVoicing a = Chord (Octaved a)

newtype Chord a = Chord {getVoices :: Set.Set a}

chordMap :: (Ord a,Ord b) => (a -> b) -> Chord a -> Chord b
chordMap f = Chord . Set.map f . getVoices

instance SummaryChar (Chord a) where
  sumUp = head . show . countVoices

countVoices :: Chord a -> Int
countVoices = Set.size . getVoices

chordOf :: Ord a => [a] -> Chord a
chordOf = Chord . Set.fromList

rebaseChord :: (Ord a,Semigroup a) => a -> Chord a -> Chord a
rebaseChord p = chordMap (<> p)

getVoiceList :: Chord a -> [a]
getVoiceList = Set.toList . getVoices

addToChord :: Ord a => a -> Chord a -> Chord a
addToChord p (Chord c) = Chord (Set.insert p c)

invChrd :: (Monoid a,Ord a) => Int -> ChordVoicing a -> ChordVoicing a
invChrd 0 c = c
invChrd k (Chord c) = let (p,c') = Set.deleteFindMin c in invChrd (k-1) (Chord $ Set.insert (shiftOctave 1 p) c')

chordRoot :: Ord a => Chord a -> a
chordRoot = Set.findMin . getVoices

instance Show a => Show (Chord a) where
  show c = "<" ++ (init . tail . show . getVoiceList $ c) ++ ">"

prettyShowChord :: Show a => Chord a -> String
prettyShowChord (Chord c) = init . unlines $ ["/="] ++ fmap show (Set.toList c) ++ ["\\="]

chordCeil :: Ord a => Octaved a -> Chord a -> Octaved a
chordCeil p c = let (_l,g) = Set.split pc (getVoices c) in case Set.lookupMin g of
  Just g' -> inOctave (getOctave p) g'
  Nothing -> inOctave (getOctave p + 1) $ Set.findMin (getVoices c)
  where
    pc = getPitchClass p

chordFloor :: Ord a => Octaved a -> Chord a -> Octaved a
chordFloor p c = let (l,_g) = Set.split pc (getVoices c) in case Set.lookupMax l of
  Just l' -> inOctave (getOctave p) l'
  Nothing -> inOctave (getOctave p - 1) $ Set.findMax (getVoices c)
  where
    pc = getPitchClass p
