{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}
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
import Control.Monad.ST
import Control.Monad
import Boopadoop.Diagram
import Boopadoop.Rhythm
import Boopadoop.Interval
import Boopadoop.Discrete
import Data.List
import Data.Bits
import Data.Int
import Data.Complex
import Data.Foldable
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.Primitive.ByteArray as BA
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as MVector
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as BSB
import Debug.Trace
import System.IO.Unsafe
import Data.IORef

-- | A 'Waveform' is a function (of time) that we can later sample.
newtype Waveform t a = Waveform 
  {sample :: t -> a -- ^ 'sample' the 'Waveform' at a specified time
  }

instance Functor (Waveform t) where
  fmap f w = sampleFrom $ f . sample w

instance SummaryChar (Waveform t a) where
  sumUp _ = '~'

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
      waveSamples = map ((`quotRoundUp` (1 + (discFactor `quot` quantLevel))) . fromIntegral . unDiscrete . sample w . (/sampleRate)) [0 .. 115]
      quantLevel = 15 :: Int
      sampleRate = 6400

instance Show (Waveform Tick Discrete) where
  show w = intercalate "\n" . transpose $ map sampleToString waveSamples
    where
      sampleToString k = if k <= quantLevel && k >= -quantLevel
        then replicate (quantLevel - k) '.' ++ "x" ++ replicate (quantLevel + k) '.'
        else let m = "k = " ++ show k in m ++ replicate (quantLevel * 2 + 1 - length m) ' '
      waveSamples = map ((`quotRoundUp` (1 + (discFactor `quot` quantLevel))) . fromIntegral . unDiscrete . sample (skipTicks 1 w)) [0 .. 115]
      quantLevel = 15 :: Int

-- | A version of @'quot'@ that rounds away from zero instead of towards it.
quotRoundUp :: Int -> Int -> Int
quotRoundUp a b = if a `mod` b == 0 then a `quot` b else (signum a * signum b) + (a `quot` b)

-- | Build a 'Waveform' by sampling the given function.
sampleFrom :: (t -> a) -> Waveform t a
sampleFrom f = Waveform $ \t -> t `seq` f t

-- | Sample a 'Waveform' at specified time. @'sampleAt' = 'flip' 'sample'@
sampleAt :: t -> Waveform t a -> a
sampleAt = flip sample

-- | Pure sine wave of the given frequency
sinWave :: Floating a => a -> Waveform a a
sinWave f = sampleFrom $ \t -> let !freq = 2 * pi * f in sin (freq * t)

-- | Sine wave that is optimized to store only a small @'CompactWavetable'@.
-- The period is given in ticks because non-tick-multiple period sin waves will have aliasing under this optimization!
fastSin :: Tick -> Wavetable
fastSin per = exploitPeriodicity per $ tickTable (fromIntegral per) $ discretize $ sinWave 1

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

-- | Control rate of speed of one waveform with another
freqModulate :: Waveform t' p -> Waveform p a -> Waveform t' a
freqModulate phaseSelect w = sampleFrom $ \t -> sample w (sample phaseSelect t)

emuVCO :: Waveform Tick Double -> Waveform Double a -> [a]
emuVCO freqSelect w = go 0 0
  where
    go i sampPoint = let skip = sample freqSelect i in sample w sampPoint : go (i + 1) (sampPoint + skip)

recordStream :: [a] -> Waveform Tick a
recordStream xs = sampleFrom ((xs!!) . fromIntegral)

streamWavetable :: Wavetable -> [Discrete]
streamWavetable w = map (sample w) [0..]

memorizeStream :: Int -> [Discrete] -> Wavetable
memorizeStream len = fromCompact . CompactWavetable . Vector.fromList . map unDiscrete . take len

-- | Very slow
integrateWave :: Num a => Waveform Tick a -> Waveform Tick a
integrateWave w = sampleFrom $ \t -> sum . map (sample w) $ [0 .. t]

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

wavestreamToWAVE :: Tick -> Int -> [Discrete] -> WAVE
wavestreamToWAVE outTicks sampleRate ws = WAVE
  {waveHeader = WAVEHeader
    {waveNumChannels = 1
    ,waveFrameRate = sampleRate
    ,waveBitsPerSample = 32
    ,waveFrames = Just $ fromIntegral outTicks
    }
  ,waveSamples = [take outTicks $ map unDiscrete ws]
  }



-- | Triangle wave of the given frequency
triWave :: (Ord a,RealFrac a) => a -> Waveform a a
triWave f = sampleFrom $ \t -> let r = (t * f) - fromIntegral (floor (t * f)) in if r < 0.25
  then 4 * r
  else if r < 0.75
    then 2 - (4 * r)
    else -4 + (4 * r)

-- | Arbitrarily chosen standard tick rate, used in @'testWave'@
stdtr :: Num a => a
stdtr = 32000

-- | Output the first @len@ seconds of the given @'Wavetable'@ to a @.wav@ file at the given path for testing.
-- The volume is also attenuated by 50% to not blow out your eardrums.
-- Also pretty prints the wave.
testWave :: Double -> String -> Wavetable -> IO ()
testWave len fp w = print w >> pure w >>= putWAVEFile (fp ++ ".wav") . waveformToWAVE (floor $ len*stdtr) stdtr . amplitudeModulate (sampleFrom $ const 0.5)

-- | Output the first @len@ seconds of the given wave stream to a @.wav@ file at the given path for testing.
testWaveStream :: Double -> String -> [Discrete] -> IO ()
testWaveStream len fp = putWAVEFile (fp ++ ".wav") . wavestreamToWAVE (floor $ len*stdtr) stdtr


-- | Outputs a sound test of the given @'PitchFactorDiagram'@ as an interval above @'concertA'@ as a @'sinWave'@ to the file @diag.wav@ for testing.
testDiagram :: PitchFactorDiagram -> IO ()
testDiagram = putWAVEFile "diag.wav" . waveformToWAVE (3*32000) 32000 . tickTable 32000 . fmap doubleToDiscrete . buildTestTrack . realToFrac . diagramToRatio . normalizePFD
  where
    buildTestTrack p = sequenceNotes [((0,1),sinWave concertA),((1,2),sinWave (concertA * p)),((2,3), buildChord [1,p] concertA)]

-- | Converts a rhythm of @'DWave'@ notes to a combined @'DWave'@ according to the timing rules of @'Beat'@.
--sequenceToBeat :: Double -> Double -> Beat DWave -> DWave
--sequenceToBeat startAt totalLength (RoseBeat bs) = let dt = totalLength / genericLength bs in fst $ foldl (\(w,i) b -> (mergeWaves . (:[w]) . sequenceToBeat (i * dt) dt $ b,i+1)) (sampleFrom $ const 0,0) bs
--sequenceToBeat startAt totalLength Rest = sampleFrom $ const 0
--sequenceToBeat startAt totalLength (Beat w) = modulate muting (compactWave (startAt,startAt + totalLength)) $ timeShift startAt w

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

-- | Shift a wave in time such that the new zero is at the specified position
seekTo :: Num t => t -> Waveform t a -> Waveform t a
seekTo dt = sampleFrom . (. (+dt)) . sample

-- | Play several waves in a row with eqqual time each, using @'sequenceNotes'@.
--equalTime :: Double -> [DWave] -> DWave
--equalTime dt = sequenceNotes . foldl go []
  --where
    --go xs@(((_,t1),_):_) k = ((t1,t1 + dt),k):xs
    --go [] k = [((0,dt),k)]

-- | Modify the amplitude of a wave by a constant multiple
setVolume :: Num a => a -> Waveform t a -> Waveform t a
setVolume = amplitudeModulate . sampleFrom . const

-- | The empty wave that is always zero when sampled
emptyWave :: Num a => Waveform t a
emptyWave = sampleFrom $ const 0

-- | Convolve with explicit discrete filter kernel weights.
discreteConvolve :: (Num a, Num t) => Waveform t [(t,a)] -> Waveform t a -> Waveform t a
discreteConvolve profile w = sampleFrom $ \t -> sum . map (\(dt,amp) -> amp * sample w (t + dt)) $ sample profile t

-- | This operation is not convolution, but something kind of like it. Use for creative purposes? Should be fast!
-- @wackyNotConvolution modf profile w = sampleFrom $ \t -> sample (modulate modf (sample profile t) w) t@
wackyNotConvolution :: (a -> b -> c) -> Waveform t (Waveform t a) -> Waveform t b -> Waveform t c
wackyNotConvolution modf profile w = sampleFrom $ \t -> sample (modulate modf (sample profile t) w) t

-- | Perform a discrete convolution. The output waveform is @f(t) = \int_{t-tickRadius}^{t+tickRadius} (kernel(t))(x) * w(t+x) dx@
-- but is discretized such that @x@ is always a multiple of @skipRate@.
tickConvolution :: (Show a,Fractional a)
                => Tick -- ^ @tickRadius@
                -> Tick -- ^ @skipRate@
                -> Waveform Tick (Waveform Tick a) -- ^ The kernel of the convolution at each @'Tick'@
                -> Waveform Tick a -- ^ w(t)
                -> Waveform Tick a
tickConvolution tickRadius skipRate profile w = sampleFrom $ \t -> let !kern = sample profile t in sum . map (\dt -> (*0.01) . (*sample w (t + dt)) . sample kern $ dt) $ sampleDeltas
  where
    sampleDeltas = map (*skipRate) [-stepsPerSide.. stepsPerSide]
    stepsPerSide = tickRadius `div` skipRate
    !stepModifier = realToFrac . recip . fromIntegral $ stepsPerSide

--accuCount :: IORef Integer
--accuCount = unsafePerformIO $ newIORef 0

-- | This computes a solid slice of a convolution much faster than sampling each point individually by using a convolution cache window.
fastTickConvolutionFixedKern :: Double -> Tick -> Tick -> Wavetable -> Wavetable -> [Discrete]
fastTickConvolutionFixedKern tickRate tickStart tickRadius kern w = kern `seq` go (vi,0) tickStart
  where
    vi = runST $ do
      v <- BA.newByteArray (windowSize * 4)
      forM_ [-tickRadius .. (tickRadius - 1)] $ \dt -> BA.writeByteArray v (1 + tickRadius + dt) (unDiscrete $ sample w (tickStart + dt))
      BA.unsafeFreezeByteArray v
    -- Table, breakpoint, offset
    stepSt :: (BA.ByteArray, Int) -> Tick -> (Discrete, BA.ByteArray)
    stepSt (!t,!b) o = runST $ do
      v <- BA.unsafeThawByteArray t
      BA.writeByteArray v (b `mod` windowSize) (unDiscrete $ sample w (o + tickRadius))
      let 
        accu v tot (dt:dts) = do
          --let !() = unsafePerformIO $ readIORef accuCount >>= \x -> (if x `mod` 10000 == 0 then putStrLn ("accu: " ++ show x) else pure ()) >> writeIORef accuCount (x + 1)
          let !readIx = (b + tickRadius + dt + 1) `mod` windowSize
          s <- BA.readByteArray v readIx
          let !discs = Discrete s
          let !kernSamp = 0.01 * Discrete (BA.indexByteArray kernel dt)
          let !toAdd = (0.01 * Discrete (BA.indexByteArray kernel dt)) * Discrete s
          let !tots = tot + toAdd
          accu v tots dts
        accu _ tot [] = pure tot
      k <- accu v 0 sampleDeltas
      !v' <- BA.unsafeFreezeByteArray v
      pure (k,v')
    go (!t,!b) o = let (s,t') = stepSt (t,b) o in s `seq` (s : go (t',(b+1) `mod` windowSize) (o + 1))
    --accu :: BA.MutableByteArray s -> Int -> Tick -> Discrete -> Tick -> ST s Discrete
    windowSize = tickRadius * 2 + 1
    sampleDeltas = [-tickRadius .. tickRadius]
    !kernel = runST $ do
      v <- BA.newByteArray (windowSize * 4)
      forM_ [-tickRadius .. (tickRadius - 1)] $ \dt -> BA.writeByteArray v (1 + tickRadius + dt) (unDiscrete . (* doubleToDiscrete (recip tickRate)) $ sample kern dt)
      BA.unsafeFreezeByteArray v

-- | Same as @'tickConvolution'@ but for arbitarily valued waveforms. Works on @'DWave'@ for example.
sampledConvolution :: (RealFrac t, Fractional a) 
                   => t -- ^ @convolutionSampleRate@, controls sampling for @x@
                   -> t -- ^ @convolutionRadius@, continuous analogue of @tickRadius@
                   -> Waveform t (Waveform t a) -- ^ Kernel of convolution for each time
                   -> Waveform t a -> Waveform t a
sampledConvolution convolutionSampleRate convolutionRadius profile w = sampleFrom $ \t -> sum . map (\dt -> (*(realToFrac . recip $ convolutionSampleRate * convolutionRadius)) . (* sample w (t + dt)) . sample (sample profile t) $ dt) $ sampleDeltas
  where
    sampleDeltas = map ((/convolutionSampleRate) . realToFrac) [-samplesPerSide .. samplesPerSide]
    samplesPerSide = floor (convolutionRadius * convolutionSampleRate)
    sampleCount = 2 * samplesPerSide + 1


-- | Makes a filter which selects frequencies near @bandCenter@ with tuning parameter @bandSize@.
-- Try: @'optimizeFilter' 200 . 'tickTable' 'stdtr' $ 'bandpassFilter' 'concertA' 100@
bandpassFilter :: Fractional a 
               => Double -- ^ @bandCenter@
               -> Double -- ^ @bandSize@
               -> Waveform Double a
bandpassFilter bandCenter bandSize = sampleFrom $ \t -> if t == 0 then 1 else realToFrac $ (sin (bandFreq * t)) / (bandFreq * t) * (cos (centerFreq * t))
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

-- | Discretize the input to a @'Double'@ consuming waveform
tickTable :: Double -- ^ Sample rate. Each tick is @1/sampleRate@ seconds
          -> Waveform Double a -> Waveform Tick a
tickTable tickrate w = sampleFrom $ \t -> sample w (fromIntegral t/tickrate)

-- | Discretize the input to a @'Double'@ consuming waveform
tickTablePer :: Double -- ^ Sample period. Sampling rate is @1/period@
          -> Waveform Double a -> Waveform Tick a
tickTablePer per w = sampleFrom $ \t -> sample w (fromIntegral t * per)

-- | A domain- and codomain-discretized @'Waveform'@ suitable for writing to a WAVE file.
-- See @'waveformToWAVE'@.
type Wavetable = Waveform Tick Discrete

-- | A data structure for storing the results of a @'Wavetable'@ on some subset of its domain.
-- Used internally.
data CompactWavetable = CompactWavetable {getWavetable :: Vector.Vector Int32}

fromCompact :: CompactWavetable -> Wavetable
fromCompact cwt = sampleFrom $ \t -> case getWavetable cwt Vector.!? t of
  Just d -> Discrete d
  Nothing -> error "fromCompact Wavetable sampled outside size of compact table"


-- | Optimize a @'Wavetable'@ by storing its values in a particular range.
-- Uses @(tickEnd - tickStart + 1) * sizeOf (_ :: 'Discrete')@ bytes of memory to do this.
solidSlice :: Tick -> Tick -> Wavetable -> Wavetable
solidSlice tickStart tickEnd w = sampleFrom $ \t -> case getWavetable cwt Vector.!? (t-tickStart) of
  Just d -> Discrete d
  Nothing -> sample w t
  where
    cwt = CompactWavetable {getWavetable = Vector.generate (tickEnd - tickStart + 1) (unDiscrete . sample w . (+tickStart))}

-- | Optimize a filter by doing @'solidSlice'@ around @t=0@ since those values are sampled repeatedly in a filter
optimizeFilter :: Tick -> Wavetable -> Wavetable
optimizeFilter tickRadius = solidSlice (-tickRadius) tickRadius

-- | Take the Fourier Transform of a complex valued @'Tick'@ sampled waveform
fourierTransform :: Tick -> Double -> Waveform Tick (Complex Double) -> Waveform Double (Complex Double)
fourierTransform tickRadius fTickRate x = sampleFrom $ \f -> sum . map (\n -> sample x n / (fromIntegral tickRadius) * cis (2 * pi * f * (fromIntegral n / fTickRate))) $ [-tickRadius .. tickRadius]

-- | Take the Fourier Transform of a @'Wavetable'@
realDFT :: Tick -- ^ Radius of Fourier Transform window in @'Tick'@s. Try 200
        -> Double -- ^ Sampling rate to use for the Fourier transform. Try the sample sample rate as the @'Wavetable'@
        -> Wavetable -> Wavetable
realDFT tickRadius fTickRate x = discretize $ tickTable 1 $ fmap ((min 1) . magnitude) $ fourierTransform tickRadius fTickRate ((\x -> discreteToDouble x :+ 0) <$> solidSlice (-tickRadius) tickRadius x)

-- | Skip every @n@ ticks in the in the given @'Waveform'@.
-- @'sample' ('skipTicks' n w) k = 'sample' w (n*k)@
skipTicks :: Tick -- ^ @n@
          -> Waveform Tick a -> Waveform Tick a
skipTicks skipRate w = sampleFrom $ \t -> sample w (skipRate * t)

-- | Optimize a @'Wavetable'@ that we know to be periodic by storing it's values on one period.
-- Takes @period * sizeOf (_ :: 'Discrete')@ bytes of memory to do this.
--
-- Warning: Causes clicking on period boundaries if the period isn't exactly the given value in ticks.
exploitPeriodicity :: Tick -- ^ Period in @'Tick'@s of the @'Wavetable'@.
                   -> Wavetable -> Wavetable
exploitPeriodicity period x = sampleFrom $ \t -> case getWavetable cwt Vector.!? (t `mod` period) of
  Just d -> Discrete d
  Nothing -> sample x t
  where
    cwt = CompactWavetable {getWavetable = Vector.generate period (unDiscrete . sample x)}

-- | Attempts to do a fast fourier transform, but the units of the domain of the output are highly suspect.
-- May be unreliable, use with caution.
usingFFT :: Tick -> Wavetable -> Wavetable
usingFFT tickRadius w = sampleFrom $ \t -> if t < (fromIntegral $ length l)
  then(!! t) . fmap (doubleToDiscrete . magnitude) $ l
  else 0
  where
    l = fft (map ((\x -> discreteToDouble x :+ 0) . sample w) [-tickRadius .. tickRadius])

-- | Cooley-Tukey fft
fft :: [Complex Double] -> [Complex Double]
fft [] = []
fft [x] = [x]
fft xs = zipWith (+) ys ts ++ zipWith (-) ys ts
    where n = length xs
          ys = fft evens
          zs = fft odds 
          (evens, odds) = split xs
          split [] = ([], [])
          split [x] = ([x], [])
          split (x:y:xs) = (x:xt, y:yt) where (xt, yt) = split xs
          ts = zipWith (\z k -> exp' k n * z) zs [0..]
          exp' k n = cis $ -2 * pi * (fromIntegral k) / (fromIntegral n)
