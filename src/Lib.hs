{-# LANGUAGE FlexibleInstances #-}
module Lib where

import Data.WAVE as WAVE
import Control.Applicative
import Diagram
import Data.List

-- A Waveform is something we can extract samples from later
newtype Waveform a = Waveform {sample :: a -> a}
type DWave = Waveform Double
type Time = Double

instance Show (Waveform Double) where
  show = showDWave

showDWave :: DWave -> String
showDWave w = intercalate "\n" . transpose $ map sampleToString waveSamples
  where
    sampleToString k = replicate (quantLevel - k) '.' ++ "x" ++ replicate (quantLevel + k) '.'
    waveSamples = map (floor . (* realToFrac quantLevel) . sample w . (/sampleRate)) [0 .. 115]
    quantLevel = 15 :: Int
    sampleRate = 16000

sampleFrom :: (a -> a) -> Waveform a
sampleFrom = Waveform

sampleAt :: a -> Waveform a -> a
sampleAt = flip sample

-- | Pure sine wave of the given frequency
sinWave :: Double -> DWave
sinWave f = sampleFrom $ \t -> sin (2 * pi * f * t)

compactWave :: (Ord a,Num a) => (a,a) -> Waveform a
compactWave (low,high) = sampleFrom $ \t -> if t >= low && t < high
  then 1
  else 0

amplitudeModulate :: Num a => Waveform a -> Waveform a -> Waveform a
amplitudeModulate a b = sampleFrom $ \t -> sample a t * sample b t

phaseModulate :: Num a => a -> Waveform a -> Waveform a -> Waveform a
phaseModulate lambdaSpread modulation target = sampleFrom $ \t -> sample target (t + lambdaSpread * sample modulation t)

changeSpeed :: (Ord a,Fractional a) => a -> a -> a -> Waveform a -> Waveform a
changeSpeed startTime lerpTime newSpeed wave = sampleFrom $ \t -> sample wave $ if t < startTime
  then t
  else if t > startTime + lerpTime
    then startTime + newSpeed * t
    else startTime + (1 + ((t - startTime)/lerpTime) * (newSpeed - 1)) * t -- Lerp between sampling at 1 and sampling at newSpeed

balanceChord :: Fractional a => [Waveform a] -> Waveform a
balanceChord notes = sampleFrom $ \t -> sum (map (sampleAt t) notes) / fromIntegral chordSize
  where
    chordSize = length notes

mergeWaves :: Fractional a => [Waveform a] -> Waveform a
mergeWaves notes = sampleFrom $ \t -> sum (map (sampleAt t) notes)
  -- Average Frequency
  --,frequency = fmap (/(fromIntegral $ length notes)) . foldl (liftA2 (+)) (Just 0) . map frequency $ notes

waveformToWAVE :: Double -> DWave -> WAVE
waveformToWAVE outTime w = WAVE
  {waveHeader = WAVEHeader
    {waveNumChannels = 1
    ,waveFrameRate = sampleRate
    ,waveBitsPerSample = 32
    ,waveFrames = Just $ numFrames
    }
  ,waveSamples = [map (doubleToSample . sample w . (/sampleRate)) [0 .. fromIntegral (numFrames - 1)]]
  }
  where
    sampleRate :: Num a => a
    sampleRate = 44100
    numFrames = ceiling $ outTime * sampleRate

triWave :: Double -> DWave
triWave f = sampleFrom $ \t -> let r = (t * f) - fromIntegral (floor (t * f)) in if r < 0.25
  then 4 * r
  else if r < 0.75
    then 2 - (4 * r)
    else -4 + (4 * r)

testWave :: DWave -> IO ()
testWave w = print w >> pure w >>= putWAVEFile "test.wav" . waveformToWAVE 10 . amplitudeModulate (sampleFrom $ const 0.5)

testDiagram :: PitchFactorDiagram -> IO ()
testDiagram = putWAVEFile "diag.wav" . waveformToWAVE 3 . buildTestTrack . realToFrac . diagramToRatio . normalizePFD
  where
    buildTestTrack p = sequenceNotes [((0,1),sinWave concertA),((1,2),sinWave (concertA * p)),((2,3), buildChord [1,p] concertA)]

sequenceNotes :: (Ord a,Fractional a) => [((a,a),Waveform a)] -> Waveform a
sequenceNotes = mergeWaves . map (\(t,w) -> amplitudeModulate (compactWave t) $ timeShift (fst t) w)

buildChord :: [Double] -> Double -> DWave
buildChord relPitches root = balanceChord $ map (triWave . (root *)) relPitches

buildChordNoBalance :: [Double] -> Double -> DWave
buildChordNoBalance relPitches root = mergeWaves $ map (triWave . (root *)) relPitches

majorChordOver :: Double -> DWave
majorChordOver = buildChord
  [semi ** 0
  ,semi ** 4
  ,semi ** 7
  ]

minorChordOver :: Double -> DWave
minorChordOver = buildChord
  [semi ** 0
  ,semi ** 3
  ,semi ** 7
  ]

concertA :: Num a => a
concertA = 440

testProg :: DWave
testProg = sequenceNotes
  [((1,2),buildChord [4/4,5/4,6/4] (concertA * semi ** 0))
  ,((2,3),buildChord [5/5,6/5,7/5] (concertA * semi ** 0))
  ,((3,4),buildChord [6/6,7/6,8/6] (concertA * semi ** 0))
  ,((4,5),buildChord [semi ** (-5), semi ** 0, semi ** 4] (concertA * semi ** 7))
  ]

envelope :: Double -> Double -> Double -> Double -> Double -> Double -> DWave
envelope del att hol dec sus rel = sampleFrom $ \t -> if t < del
  then 0
  else if t - del < att
    then (t - del) / att
    else if t - del - att < hol
      then 1
      else if t - del - att - hol < dec
        then 1 + (t - del - att - hol)/dec * (sus - 1)
        else if t - del - att - hol - dec < rel
          then sus * (1 - (t - del - att - hol - dec)/rel)
          else 0

testDoot :: DWave
testDoot = amplitudeModulate (envelope 0.5 0.15 0 0.25 0.1 3) $ triWave concertA

timeShift :: Num a => a -> Waveform a -> Waveform a
timeShift dt = sampleFrom . (. subtract dt) . sample

equalTime :: Double -> [DWave] -> DWave
equalTime dt = sequenceNotes . foldl go []
  where
    go xs@(((_,t1),_):_) k = ((t1,t1 + dt),k):xs
    go [] k = [((0,dt),k)]

wackRatio :: DWave
wackRatio = sequenceNotes
  [((0,2),sinWave concertA)
  ,((2,3),buildChord [4/4,5/4,6/4] concertA)
  ,((3,4),sinWave concertA)
  ,((4,5),sinWave (concertA * 7 / 4))
  ,((5,6),buildChord [4/4,7/4] concertA)
  ,((6,7),buildChord [4/4,5/4,7/4] concertA)
  ,((7,8),buildChord [4/4,5/4,6/4,7/4] concertA)
  ]

tripleOscEmulation :: Double -> DWave
tripleOscEmulation f = balanceChord
  [sinWave f
  ,sinWave $ intervalOf (scaleInterval (-1) octave) f
  ,sinWave $ intervalOf (scaleInterval (-2) octave) f
  ]
