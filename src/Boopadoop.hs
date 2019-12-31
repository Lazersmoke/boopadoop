{-# LANGUAGE FlexibleInstances #-}
-- | A music theory library for just intonation and other mathematically pure ideas.
module Boopadoop 
  (module Boopadoop
  ,module Boopadoop.Diagram
  ,module Boopadoop.Rhythm
  ,module Boopadoop.Interval
  ) where

import Data.WAVE as WAVE
import Control.Applicative
import Boopadoop.Diagram
import Boopadoop.Rhythm
import Boopadoop.Interval
import Data.List

-- | A 'Waveform' is a function (of time) that we can later sample.
newtype Waveform t a = Waveform 
  {sample :: t -> a -- ^ 'sample' the 'Waveform' at a specified time
  }

-- | A 'Double' valued wave with time also in terms of 'Double'.
-- This models a real-valued waveform which typically has values in @[-1,1]@ and
-- is typically supported on either the entire real line ('sinWave') or on a compact subset ('compactWave')
type DWave = Waveform Double Double

-- | Show a waveform by pretty printing some of the actual waveform in dot matrix form.
instance Show (Waveform Double Double) where
  show w = intercalate "\n" . transpose $ map sampleToString waveSamples
    where
      sampleToString k = replicate (quantLevel - k) '.' ++ "x" ++ replicate (quantLevel + k) '.'
      waveSamples = map (floor . (* realToFrac quantLevel) . sample w . (/sampleRate)) [0 .. 115]
      quantLevel = 15 :: Int
      sampleRate = 16000

-- | Build a 'Waveform' by sampling the given function.
sampleFrom :: (t -> a) -> Waveform t a
sampleFrom = Waveform

-- | Sample a 'Waveform' at specified time. @'sampleAt' = 'flip' 'sample'@
sampleAt :: t -> Waveform t a -> a
sampleAt = flip sample

-- | Pure sine wave of the given frequency
sinWave :: Double -> DWave
sinWave f = sampleFrom $ \t -> sin (2 * pi * f * t)

-- | @'compactWave' (l,h)@ is a wave which is @1@ on @[l,h)@ and @0@ elsewhere
compactWave :: (Ord t,Num t) => (t,t) -> Waveform t Bool
compactWave (low,high) = sampleFrom $ \t -> t >= low && t < high

-- | Modulate the muting or non-muting of another wave with a @'Bool'@ value wave, such as @'compactWave'@.
modulateMuting :: Num a => Waveform t Bool -> Waveform t a -> Waveform t a
modulateMuting = modulate (\b s -> if b then s else 0)

-- | Modulate one wave with another according to the given function pointwise.
-- This means you can't implement 'phaseModulate' using only this combinator because phase modulation
-- requires information about the target wave at times other than the current time.
modulate :: (a -> b -> c) -> Waveform t a -> Waveform t b -> Waveform t c
modulate f a b = sampleFrom $ \t -> f (sample a t) (sample b t)

-- | Modulate the amplitude of one wave with another. This is simply pointwise multiplication:
-- @
--  'amplitudeModulate' = 'modulate' ('*')
-- @
amplitudeModulate :: Num a => Waveform t a -> Waveform t a -> Waveform t a
amplitudeModulate = modulate (*)

-- | Modulate the phase of one wave with another. Used in synthesis.
-- @
--  'phaseModulate' beta ('setVolume' 0.2 $ 'sinWave' 'concertA') ('setVolume' 0.38 $ 'triWave' 'concertA')
-- @
-- (try beta=0.0005)
phaseModulate :: Num t 
              => t -- ^ Tuning parameter. Modulation signal is @'amplitudeModulate'@d by @('const' beta)@
              -> Waveform t t -- ^ Modulation signal. Outputs the phase shift to apply
              -> Waveform t a -- ^ Target wave to be modulated
              -> Waveform t a
phaseModulate beta modulation target = sampleFrom $ \t -> sample target (t + beta * sample modulation t)

-- | Smoothly transition to playing a wave back at a different speed after some time
changeSpeed :: (Ord a,Fractional a) => a -> a -> a -> Waveform a a -> Waveform a a
changeSpeed startTime lerpTime newSpeed wave = sampleFrom $ \t -> sample wave $ if t < startTime
  then t
  else if t > startTime + lerpTime
    then startTime + newSpeed * t
    -- Lerp between sampling at 1 and sampling at newSpeed
    else startTime + (1 + ((t - startTime)/lerpTime) * (newSpeed - 1)) * t

-- | Play several waves on top of each other, normalizing so that e.g. playing three notes together doesn't triple the volume.
balanceChord :: Fractional a => [Waveform t a] -> Waveform t a
balanceChord notes = sampleFrom $ \t -> sum . map ((/ fromIntegral chordSize) . sampleAt t) $ notes
  where
    chordSize = length notes

-- | Play several waves on top of each other, without worrying about the volume. See 'balanceChord' for
-- a normalized version.
mergeWaves :: Fractional a => [Waveform t a] -> Waveform t a
mergeWaves notes = sampleFrom $ \t -> sum (map (sampleAt t) notes)
  -- Average Frequency
  --,frequency = fmap (/(fromIntegral $ length notes)) . foldl (liftA2 (+)) (Just 0) . map frequency $ notes

-- | @'waveformToWAVE' outputLength@ gives a @'WAVE'@ file object by sampling the given @'DWave'@ at @44100Hz@.
-- May disbehave or clip based on behavior of @'doubleToSample'@ if the DWave takes values outside of @[-1,1]@.
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

-- | Triangle wave of the given frequency
triWave :: (Ord a,RealFrac a) => a -> Waveform a a
triWave f = sampleFrom $ \t -> let r = (t * f) - fromIntegral (floor (t * f)) in if r < 0.25
  then 4 * r
  else if r < 0.75
    then 2 - (4 * r)
    else -4 + (4 * r)

-- | Output the first ten seconds of the given @'DWave'@ to the file @test.wav@ for testing.
-- The volume is also attenuated by 50% to not blow out your eardrums.
-- Also pretty prints the wave.
testWave :: DWave -> IO ()
testWave w = print w >> pure w >>= putWAVEFile "test.wav" . waveformToWAVE 10 . amplitudeModulate (sampleFrom $ const 0.5)

-- | Outputs a sound test of the given @'PitchFactorDiagram'@ as an interval above @'concertA'@ as a @'sinWave'@ to the file @diag.wav@ for testing.
testDiagram :: PitchFactorDiagram -> IO ()
testDiagram = putWAVEFile "diag.wav" . waveformToWAVE 3 . buildTestTrack . realToFrac . diagramToRatio . normalizePFD
  where
    buildTestTrack p = sequenceNotes [((0,1),sinWave concertA),((1,2),sinWave (concertA * p)),((2,3), buildChord [1,p] concertA)]

-- | Converts a rhythm of @'DWave'@ notes to a combined @'DWave'@ according to the timing rules of @'Beat'@.
sequenceToBeat :: Double -> Double -> Beat DWave -> DWave
sequenceToBeat startAt totalLength (RoseBeat bs) = let dt = totalLength / genericLength bs in fst $ foldl (\(w,i) b -> (mergeWaves . (:[w]) . sequenceToBeat (i * dt) dt $ b,i+1)) (sampleFrom $ const 0,0) bs
sequenceToBeat startAt totalLength Rest = sampleFrom $ const 0
sequenceToBeat startAt totalLength (Beat w) = modulateMuting (compactWave (startAt,startAt + totalLength)) $ timeShift startAt w

-- | Sequences some waves to play on the given time intervals.
sequenceNotes :: (Ord t,Fractional t,Fractional a) => [((t,t),Waveform t a)] -> Waveform t a
sequenceNotes = mergeWaves . map (\(t,w) -> modulateMuting (compactWave t) $ timeShift (fst t) w)

-- | Builds a chord out of the given ratios relative to the root pitch
-- @
--  buildChord ratios root
-- @
buildChord :: [Double] -> Double -> DWave
buildChord relPitches root = balanceChord $ map (triWave . (root *)) relPitches

-- | Builds a chord out of the given ratios relative to the root pitch, without normalizing the volume.
-- (Warning: may be loud)
buildChordNoBalance :: [Double] -> Double -> DWave
buildChordNoBalance relPitches root = mergeWaves $ map (triWave . (root *)) relPitches

-- | Builds a just-intonated major chord over the given root pitch
majorChordOver :: Double -> DWave
majorChordOver = buildChord
  [1
  ,diagramToRatio majorThird
  ,diagramToRatio perfectFifth
  ]

-- | Builds an equal temperament minor chord over the given root pitch
minorChordOver :: Double -> DWave
minorChordOver = buildChord
  [semi ** 0
  ,semi ** 3
  ,semi ** 7
  ]

-- | Concert A4 frequency is 440Hz
concertA :: Num a => a
concertA = 440

-- | Build an envelope waveform with the given parameters: Predelay Time, Attack Time, Hold Time, Decay Time, Sustain Level, Release Time
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

-- | Shift a wave in time to start at the specified time after its old start time
timeShift :: Num t => t -> Waveform t a -> Waveform t a
timeShift dt = sampleFrom . (. subtract dt) . sample

-- | Play several waves in a row with eqqual time each, using @'sequenceNotes'@.
equalTime :: Double -> [DWave] -> DWave
equalTime dt = sequenceNotes . foldl go []
  where
    go xs@(((_,t1),_):_) k = ((t1,t1 + dt),k):xs
    go [] k = [((0,dt),k)]

-- | Modify the amplitude of a wave by a constant multiple
setVolume :: Num a => a -> Waveform t a -> Waveform t a
setVolume = amplitudeModulate . sampleFrom . const

-- | The empty wave that is always zero when sampled
emptyWave :: Num a => Waveform t a
emptyWave = sampleFrom $ const 0
