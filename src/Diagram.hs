module Diagram where

import Data.Ratio
import Data.Bits
import Data.Monoid
import Data.List
import Data.Numbers.Primes
import Data.Align (salign)

-- | 12 tone equal temperment semitone
semi :: Floating a => a
semi = 2 ** (1/12)

allSemis :: Floating a => [a]
allSemis = map (semi **) . map fromIntegral $ [0..11 :: Int]

takeFinAlignments :: Floating a => Int -> [[a]]
takeFinAlignments fin = map (\k -> map (*k) . map fromIntegral $ [1.. fin]) allSemis

-- A pitch factor diagram uniquely tells the just-intonated pitch of a note as follows:
--
newtype PitchFactorDiagram = Factors {getFactors :: [Integer]} deriving Show

instance Monoid PitchFactorDiagram where
  mempty = Factors []
  mappend = addPFD

instance Semigroup PitchFactorDiagram where
  (<>) = addPFD

diagramToRatio :: PitchFactorDiagram -> Rational
diagramToRatio = product . zipWith (^^) (map fromIntegral primes) . getFactors

diagramToFloatyRatio :: PitchFactorDiagram -> Rational
diagramToFloatyRatio = flip approxRational 0.05 . diagramToRatio

diagramToSemi :: Floating a => PitchFactorDiagram -> a
diagramToSemi = (12 *) . logBase 2 . realToFrac . diagramToRatio . normalizePFD

normalizePFD :: PitchFactorDiagram -> PitchFactorDiagram
normalizePFD (Factors []) = Factors []
normalizePFD (Factors (_:xs)) = Factors $ (negate . floor . logBase 2 . realToFrac . diagramToRatio . Factors . (0:) $ xs) : xs

countPFDFuzzy :: Double -> PitchFactorDiagram
countPFDFuzzy = countPFD . flip approxRational 0.0001

countPFD :: Rational -> PitchFactorDiagram
countPFD k = Factors $ go (primeFactors $ numerator k,primeFactors $ denominator k) primes
  where
    count = (genericLength .) . filter
    go :: ([Integer],[Integer]) -> [Integer] -> [Integer]
    go ([],[]) _ = []
    go (nfs,dfs) (p:ps) = count (==p) nfs - count (==p) dfs : go (filter (/=p) nfs,filter (/=p) dfs) ps

intervalOf :: PitchFactorDiagram -> Double -> Double
intervalOf = (*) . (realToFrac . diagramToRatio)

invertInterval :: PitchFactorDiagram -> PitchFactorDiagram
invertInterval = scaleInterval (-1)

scaleInterval :: Integer -> PitchFactorDiagram -> PitchFactorDiagram
scaleInterval lambda = Factors . map (*lambda) . getFactors

addPFD :: PitchFactorDiagram -> PitchFactorDiagram -> PitchFactorDiagram
addPFD a b = Factors . map getSum $ salign (map Sum $ getFactors a) (map Sum $ getFactors b)

octave :: PitchFactorDiagram
octave = Factors [1]

perfectFifth :: PitchFactorDiagram
perfectFifth = normalizePFD $ Factors [0,1]

majorThird :: PitchFactorDiagram
majorThird = normalizePFD $ Factors [0,0,1]

mysterySeven :: PitchFactorDiagram
mysterySeven = normalizePFD $ Factors [0,0,0,1]

majorSecond :: PitchFactorDiagram
majorSecond = normalizePFD $ Factors [0,2]

mystery25 :: PitchFactorDiagram
mystery25 = normalizePFD $ Factors [0,0,2]

counterExample :: PitchFactorDiagram
counterExample = Factors $ [-3,0,-2] ++ take 42 (repeat 0) ++ [1]

printTheSequence :: Int -> IO ()
printTheSequence k 
  | k > 1000 = putStrLn ""
  | k .&. (k-1) == 0 = putStr ("[" ++ show k ++ "]") >> printTheSequence (k+1)
  | isPrime k = putStr ("(" ++ show k ++ ")") >> printTheSequence (k+1)
  | otherwise = putStr " . " >> printTheSequence (k+1)
