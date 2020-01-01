{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
-- | A music theory library for just intonation and other mathematically pure ideas.
module Boopadoop 
  (module Boopadoop
  ,module Boopadoop.Diagram
  ,module Boopadoop.Rhythm
  ,module Boopadoop.Interval
  ,module Boopadoop.Discrete
  ) where

import Data.WAVE as WAVE
import Control.Applicative
import Boopadoop.Diagram
import Boopadoop.Rhythm
import Boopadoop.Interval
import Boopadoop.Discrete
import Data.List
import Data.Bits
import Data.Int
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.Vector.Unboxed as Vector
import Debug.Trace

-- | A 'Waveform' is a function (of time) that we can later sample.
newtype Waveform t a = Waveform 
  {sample :: t -> a -- ^ 'sample' the 'Waveform' at a specified time
  }

instance Functor (Waveform t) where
  fmap f w = sampleFrom $ f . sample w

-- | A 'Double' valued wave with time also in terms of 'Double'.
-- This models a real-valued waveform which typically has values in @[-1,1]@ and
-- is typically supported on either the entire real line ('sinWave') or on a compact subset ('compactWave')
type DWave = Waveform Double Double

-- | Show a waveform by pretty printing some of the actual waveform in dot matrix form.
instance Show (Waveform Double Double) where
  show w = intercalate "\n" . transpose $ map sampleToString waveSamples
    where
      sampleToString k = if k <= quantLevel && k >= -quantLevel
        then replicate (quantLevel - k) '.' ++ "x" ++ replicate (quantLevel + k) '.'
        else let m = "k = " ++ show k in m ++ replicate (quantLevel * 2 + 1 - length m) ' '
      waveSamples = map (floor . (* realToFrac quantLevel) . sample w . (/sampleRate)) [0 .. 115]
      quantLevel = 15 :: Int
      sampleRate = 6400

instance Show (Waveform Double Discrete) where
  show w = intercalate "\n" . transpose $ map sampleToString waveSamples
    where
      sampleToString k = if k <= quantLevel && k >= -quantLevel
        then replicate (quantLevel - k) '.' ++ "x" ++ replicate (quantLevel + k) '.'
        else let m = "k = " ++ show k in m ++ replicate (quantLevel * 2 + 1 - length m) ' '
      waveSamples = map ((+1) . (`div` (discFactor `div` quantLevel)) . fromIntegral . unDiscrete . sample w . (/sampleRate)) [0 .. 115]
      quantLevel = 15 :: Int
      sampleRate = 6400

instance Show (Waveform Tick Discrete) where
  show w = intercalate "\n" . transpose $ map sampleToString waveSamples
    where
      sampleToString k = if k <= quantLevel && k >= -quantLevel
        then replicate (quantLevel - k) '.' ++ "x" ++ replicate (quantLevel + k) '.'
        else let m = "k = " ++ show k in m ++ replicate (quantLevel * 2 + 1 - length m) ' '
      waveSamples = map ((+1) . (`div` (discFactor `div` quantLevel)) . fromIntegral . unDiscrete . sample w . (*skipRate)) [0 .. 115]
      quantLevel = 15 :: Int
      skipRate = 5

-- | Build a 'Waveform' by sampling the given function.
sampleFrom :: (t -> a) -> Waveform t a
sampleFrom f = Waveform $ \t -> t `seq` f t

-- | Sample a 'Waveform' at specified time. @'sampleAt' = 'flip' 'sample'@
sampleAt :: t -> Waveform t a -> a
sampleAt = flip sample

-- | Pure sine wave of the given frequency
sinWave :: Floating a => a -> Waveform a a
sinWave f = sampleFrom $ \t -> let !freq = 2 * pi * f in sin (freq * t)

-- | @'compactWave' (l,h)@ is a wave which is @'True'@ on @[l,h)@ and @'False'@ elsewhere
compactWave :: (Ord t,Num t) => (t,t) -> Waveform t Bool
compactWave (low,high) = sampleFrom $ \t -> t >= low && t < high

-- | @'muting' 'True'@ is @'id'@ while @'muting' 'False'@ is @'const' 0@.
muting :: Num a => Bool -> a -> a
muting b s = if b then s else 0

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
balanceChord notes = sampleFrom $ \t -> sum . map ((* (realToFrac . recip . fromIntegral . length $ notes)) . sampleAt t) $ notes

-- | Play several waves on top of each other, without worrying about the volume. See 'balanceChord' for
-- a normalized version.
mergeWaves :: Num a => [Waveform t a] -> Waveform t a
mergeWaves notes = sampleFrom $ \t -> sum (map (sampleAt t) notes)
  -- Average Frequency
  --,frequency = fmap (/(fromIntegral $ length notes)) . foldl (liftA2 (+)) (Just 0) . map frequency $ notes

-- | @'waveformToWAVE' outputLength@ gives a @'WAVE'@ file object by sampling the given @'DWave'@ at @44100Hz@.
-- May disbehave or clip based on behavior of @'doubleToSample'@ if the DWave takes values outside of @[-1,1]@.
waveformToWAVE :: Tick -> Int -> Wavetable -> WAVE
waveformToWAVE outTicks sampleRate w = WAVE
  {waveHeader = WAVEHeader
    {waveNumChannels = 1
    ,waveFrameRate = sampleRate
    ,waveBitsPerSample = 32
    ,waveFrames = Just $ fromIntegral outTicks
    }
  ,waveSamples = [map (unDiscrete . sample w) [0 .. outTicks - 1]]
  }

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
testWave :: Wavetable -> IO ()
testWave w = print w >> pure w >>= putWAVEFile "test.wav" . waveformToWAVE (2*32000) 32000 . amplitudeModulate (sampleFrom $ const 0.5)

-- | Outputs a sound test of the given @'PitchFactorDiagram'@ as an interval above @'concertA'@ as a @'sinWave'@ to the file @diag.wav@ for testing.
testDiagram :: PitchFactorDiagram -> IO ()
testDiagram = putWAVEFile "diag.wav" . waveformToWAVE (3*32000) 32000 . tickTable 32000 . fmap doubleToDiscrete . buildTestTrack . realToFrac . diagramToRatio . normalizePFD
  where
    buildTestTrack p = sequenceNotes [((0,1),sinWave concertA),((1,2),sinWave (concertA * p)),((2,3), buildChord [1,p] concertA)]

-- | Converts a rhythm of @'DWave'@ notes to a combined @'DWave'@ according to the timing rules of @'Beat'@.
sequenceToBeat :: Double -> Double -> Beat DWave -> DWave
sequenceToBeat startAt totalLength (RoseBeat bs) = let dt = totalLength / genericLength bs in fst $ foldl (\(w,i) b -> (mergeWaves . (:[w]) . sequenceToBeat (i * dt) dt $ b,i+1)) (sampleFrom $ const 0,0) bs
sequenceToBeat startAt totalLength Rest = sampleFrom $ const 0
sequenceToBeat startAt totalLength (Beat w) = modulate muting (compactWave (startAt,startAt + totalLength)) $ timeShift startAt w

-- | Sequences some waves to play on the given time intervals.
sequenceNotes :: (Ord t,Fractional t,Fractional a) => [((t,t),Waveform t a)] -> Waveform t a
sequenceNotes = mergeWaves . map (\(t,w) -> modulate muting (compactWave t) $ timeShift (fst t) w)

-- | Builds a chord out of the given ratios relative to the root pitch
-- @
--  buildChord ratios root
-- @
buildChord :: (Num a,RealFrac a) => [a] -> a -> Waveform a a
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

discreteConvolve :: (Num a, Num t) => Waveform t [(t,a)] -> Waveform t a -> Waveform t a
discreteConvolve profile w = sampleFrom $ \t -> sum . map (\(dt,amp) -> amp * sample w (t + dt)) $ sample profile t

wackyNotConvolution :: (a -> b -> c) -> Waveform t (Waveform t a) -> Waveform t b -> Waveform t c
wackyNotConvolution modf profile w = sampleFrom $ \t -> sample (modulate modf (sample profile t) w) t

sampledConvolution :: (RealFrac t, Fractional a, Show t, Show a) => t -> t -> Waveform t (Waveform t a) -> Waveform t a -> Waveform t a
sampledConvolution convolutionSampleRate convolutionRadius profile w = sampleFrom $ \t -> sum . map (\dt -> (*(realToFrac . recip $ convolutionSampleRate * convolutionRadius)) . (* sample w (t + dt)) . sample (sample profile t) $ dt) $ sampleDeltas
  where
    sampleDeltas = map ((/convolutionSampleRate) . realToFrac) [-samplesPerSide .. samplesPerSide]
    samplesPerSide = floor (convolutionRadius * convolutionSampleRate)
    sampleCount = 2 * samplesPerSide + 1

tickConvolution :: Fractional a => Tick -> Tick -> Waveform Tick (Waveform Tick a) -> Waveform Tick a -> Waveform Tick a
tickConvolution tickRadius skipRate profile w = sampleFrom $ \t -> sum . map (\dt -> (*realToFrac (recip $ fromIntegral stepsPerSide)) . (*sample w (t + dt)) . sample (sample profile t) $ dt) $ sampleDeltas
  where
    sampleDeltas = map (*skipRate) [-stepsPerSide.. stepsPerSide]
    stepsPerSide = tickRadius `div` skipRate

bandpassFilter :: Fractional a => Double -> Double -> Waveform Double a
bandpassFilter bandCenter bandSize = sampleFrom $ \t -> if t == 0 then 1 else realToFrac (sin (bandFreq * t)) / realToFrac (bandFreq * t) * realToFrac (cos (centerFreq * t))
  where
    !bandFreq = 2 * pi * bandSize
    !centerFreq = 2 * pi * bandCenter

{-
sampledConvolve modf profile w = sampleFrom $ \p -> modf (sample (sample profile p) p) (sample w p)

takeSamples :: 
takeSamples sampleRate w = map (sample w . (/sampleRate)) [0 .. 115]
  ,waveSamples = [map (doubleToSample . sample w . (/sampleRate)) [0 .. fromIntegral (numFrames - 1)]]
-}


-- | Discretize the output of a @'Double'@ producing waveform
discretize :: Waveform t Double -> Waveform t Discrete
discretize = fmap (Discrete . properFloor . (*discFactor))

tickTable :: Double -> Waveform Double a -> Waveform Tick a
tickTable tickrate w = sampleFrom $ \t -> sample w (fromIntegral t/tickrate)

tickTableMemo :: Double -> Waveform Double a -> Waveform Tick a
tickTableMemo tickrate w = sampleFrom $ \t -> if t < 0 then sample w (fromIntegral t/tickrate) else tab IntMap.! (fromIntegral t)
  where
    tab = IntMap.fromAscList . map (\k -> (fromIntegral k, sample w (fromIntegral k/tickrate))) $ [0..]

type Wavetable = Waveform Tick Discrete

data CompactWavetable = CompactWavetable {getWavetable :: Vector.Vector Int32, getZeroOffset :: !Int}

fromCompact :: CompactWavetable -> Wavetable -> Wavetable
fromCompact cwt w = sampleFrom $ \t -> case getWavetable cwt Vector.!? (fromIntegral t + getZeroOffset cwt) of
  Just d -> Discrete d
  Nothing -> sample w t

solidify :: Tick -> Wavetable -> CompactWavetable
solidify tickRadius w = CompactWavetable
  {getWavetable = Vector.generate (2 * fromIntegral tickRadius + 1) (unDiscrete . sample w . fromIntegral)
  ,getZeroOffset = fromIntegral tickRadius
  }

optimizeWavetable :: Wavetable -> Wavetable
optimizeWavetable w = fromCompact (solidify 200 w) w
