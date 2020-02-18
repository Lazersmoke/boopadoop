{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
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

import qualified Data.WAVE as WAVE
import Control.Monad.ST
import Control.Monad
import Control.Applicative
import Boopadoop.Diagram
import Boopadoop.Rhythm
import Boopadoop.Interval
import Boopadoop.Discrete
import Data.List
import Data.Int
import Data.Complex
import qualified Data.Vector.Fixed as FV
import qualified Data.Primitive.ByteArray as BA
import qualified Data.Vector.Unboxed as Vector

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

instance Show (Waveform Tick Double) where
  show w = intercalate "\n" . transpose $ map sampleToString waveSamples
    where
      sampleToString k = if k <= quantLevel && k >= -quantLevel
        then replicate (quantLevel - k) '.' ++ "x" ++ replicate (quantLevel + k) '.'
        else let m = "k = " ++ show k in m ++ replicate (quantLevel * 2 + 1 - length m) ' '
      waveSamples = map (floor . (* realToFrac quantLevel) . sample (skipTicks 1 w)) [0 .. 115]
      quantLevel = 15 :: Int

-- | A version of @'quot'@ that rounds away from zero instead of towards it.
quotRoundUp :: Int -> Int -> Int
quotRoundUp a b = if a `mod` b == 0 then a `quot` b else (signum a * signum b) + (a `quot` b)

-- | Build a 'Waveform' by sampling the given function.
sampleFrom :: (t -> a) -> Waveform t a
sampleFrom f = Waveform $ \t -> {-t `seq`-} f t

-- | Sample a 'Waveform' at specified time. @'sampleAt' = 'flip' 'sample'@
sampleAt :: t -> Waveform t a -> a
sampleAt = flip sample

-- | Pure sine wave of the given frequency
--sinWave :: Floating a => a -> Waveform a a
sinWave :: Double -> DWave
sinWave f = if sinHack -- speedy fast sin hack from https://www.gamedev.net/forums/topic/621589-extremely-fast-sin-approximation/ due to nightcracker
  then sampleFrom $ \t -> let 
    x = pi * ((2 * f * t) - fromIntegral k) 
    x2 = x * x
    k = spookyFastTruncate (2 * f * t)
    in (if odd k then negate else id) (x * (c + x2 * (b + a * x2)))
  else sampleFrom $ \t -> sin (freq * t)
  where
    sinHack = True
    !freq = 2 * pi * f
    a = 0.00735246819687011731341356165096815
    b = -0.16528911397014738207016302002888890
    c = 0.99969198629596757779830113868360584

-- | Pure sine wave of the given frequency and initial phase
sinWaveWithPhase :: Floating a => a -> a -> Waveform a a
sinWaveWithPhase f phi0 = sampleFrom $ \t -> let !freq = 2 * pi * f in sin (freq * t + phi0)

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

-- | First argument is valued in sample time steps, so @1/stdtr@ is a good starting point.
emuVCO :: Waveform Tick Double -> Waveform Double a -> [a]
emuVCO freqSelect w = go 0 0
  where
    go !i sampPoint = let skip = sample freqSelect i in sample w sampPoint : go (i + 1) (sampPoint + skip)

emuVCO' :: [Double] -> Waveform Double a -> [a]
emuVCO' fs w = go fs 0
  where
    go freqSelect !sampPoint = let (skip:ss) = freqSelect in sample w sampPoint : go ss (sampPoint + skip)

niceVCO :: Double -> [Double] -> Waveform Double a -> [a]
niceVCO amp fs = emuVCO' (fmap (\f -> (amp ** f)/stdtr) fs)

oscAboveZero :: Double -> Double -> Double
oscAboveZero amp = oscAbout (amp/2) amp

oscAbout :: Double -> Double -> Double -> Double
oscAbout z0 amp z = (amp * z) + z0

-- | Piecewise linear reconstruction (interpolation) of a sampled signal
linearResample :: Double -> Waveform Tick Double -> Waveform Double Double
linearResample sampleRate w = sampleFrom $ \t -> do
  let t' = t * sampleRate
  let (low,high) = (floor t',ceiling t')
  (fromIntegral high - t') * sample w low + (t' - fromIntegral low) * sample w high

recordStream :: [a] -> Waveform Tick a
recordStream xs = sampleFrom ((xs!!) . fromIntegral)

-- | The output wavetables here have @sample (toConvolutionTableStream ds !! t) k = ds !! (t - k)@ for @k < windowSize@
toConvolutionTableStream :: Tick -> [Discrete] -> [Wavetable]
toConvolutionTableStream windowSize xs = fmap toWavetable . scanl stepSt (startTable xs) $ xs
  where
    toWavetable :: (BA.ByteArray,Int) -> Wavetable
    toWavetable (v,b) = sampleFrom $ \k -> Discrete $ BA.indexByteArray v ((b - k) `mod` windowSize)
    startTable :: [Discrete] -> (BA.ByteArray,Int)
    startTable ds = (BA.byteArrayFromListN windowSize (fmap unDiscrete $ take windowSize ds),0)
    -- Table, breakpoint
    stepSt :: (BA.ByteArray, Int) -> Discrete -> (BA.ByteArray, Int)
    stepSt (!t,!b) x = runST $ do
      v <- BA.unsafeThawByteArray t
      BA.writeByteArray v (b `mod` windowSize) (unDiscrete x)
      v' <- BA.unsafeFreezeByteArray v
      pure (v',(b+1) `mod` windowSize)

streamWavetable :: Waveform Tick a -> [a]
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
balanceChord notes = sampleFrom $ \t -> foldr (\x s -> s + factor * sample x t) 0 $ notes
  where
    factor = realToFrac . recip @Double . fromIntegral . length $ notes

-- | Play several waves on top of each other, without worrying about the volume. See 'balanceChord' for
-- a normalized version.
mergeWaves :: Num a => [Waveform t a] -> Waveform t a
mergeWaves notes = sampleFrom $ \t -> foldr (\x s -> s + sampleAt t x) 0 notes
  -- Average Frequency
  --,frequency = fmap (/(fromIntegral $ length notes)) . foldl (liftA2 (+)) (Just 0) . map frequency $ notes

-- | @'waveformToWAVE' outputLength@ gives a @'WAVE'@ file object by sampling the given @'DWave'@ at @44100Hz@.
-- May disbehave or clip based on behavior of @'doubleToSample'@ if the DWave takes values outside of @[-1,1]@.
waveformToWAVE :: Tick -> Int -> Wavetable -> WAVE.WAVE
waveformToWAVE outTicks sampleRate w = WAVE.WAVE
  {WAVE.waveHeader = WAVE.WAVEHeader
    {WAVE.waveNumChannels = 1
    ,WAVE.waveFrameRate = sampleRate
    ,WAVE.waveBitsPerSample = 32
    ,WAVE.waveFrames = Just $ fromIntegral outTicks
    }
  ,WAVE.waveSamples = [map (unDiscrete . sample w) [0 .. outTicks - 1]]
  }

wavestreamToWAVE :: Tick -> Int -> [Discrete] -> WAVE.WAVE
wavestreamToWAVE outTicks sampleRate ws = WAVE.WAVE
  {WAVE.waveHeader = WAVE.WAVEHeader
    {WAVE.waveNumChannels = 1
    ,WAVE.waveFrameRate = sampleRate
    ,WAVE.waveBitsPerSample = 32
    ,WAVE.waveFrames = Just $ fromIntegral outTicks
    }
  ,WAVE.waveSamples = [take outTicks $ map unDiscrete ws]
  }

finiteWavestreamToWAVE :: Int -> [Discrete] -> WAVE.WAVE
finiteWavestreamToWAVE sampleRate ws = WAVE.WAVE
  {WAVE.waveHeader = WAVE.WAVEHeader
    {WAVE.waveNumChannels = 1
    ,WAVE.waveFrameRate = sampleRate
    ,WAVE.waveBitsPerSample = 32
    ,WAVE.waveFrames = Just $ length ws --Nothing
    }
  ,WAVE.waveSamples = [map unDiscrete ws]
  }

-- | Triangle wave of the given frequency
triWave :: Double -> Waveform Double Double
triWave f = sampleFrom $ \t -> let r = t * f - fromIntegral (floor (t * f) :: Int) in if r < 0.25
  then 4 * r
  else if r < 0.75
    then 2 - (4 * r)
    else -4 + (4 * r)

sawWave :: Double -> Waveform Double Double
sawWave f = sampleFrom $ \t -> 2 * (t * f - fromIntegral (floor (t * f) :: Int)) - 1

ramp :: Double -> Double -> Double
ramp = rampFrom 0

rampFrom :: Double -> Double -> Double -> Double
rampFrom x0 time t = if t < time
  then x0 + (1 - x0) * (max t 0)/time
  else 1

waveTimbre :: Functor f => f DWave -> f Wavetable
waveTimbre = fmap (discretize . tickTable stdtr)

-- | Arbitrarily chosen standard tick rate, used in @'testWave'@
stdtr :: Num a => a
stdtr = 48000 --32000

-- | Output the first @len@ seconds of the given @'Wavetable'@ to a @.wav@ file at the given path for testing.
-- The volume is also attenuated by 50% to not blow out your eardrums.
-- Also pretty prints the wave.
testWave :: Double -> String -> Wavetable -> IO ()
testWave len fp w = print w >> pure w >>= WAVE.putWAVEFile (fp ++ ".wav") . waveformToWAVE (floor $ len*stdtr) stdtr . amplitudeModulate (sampleFrom $ const 0.5)

-- | Output the first @len@ seconds of the given wave stream to a @.wav@ file at the given path for testing.
testWaveStream :: Double -> String -> [Discrete] -> IO ()
testWaveStream len fp = WAVE.putWAVEFile (fp ++ ".wav") . wavestreamToWAVE (floor $ len*stdtr) stdtr


-- | Outputs a sound test of the given @'PitchFactorDiagram'@ as an interval above @'concertA'@ as a @'sinWave'@ to the file @diag.wav@ for testing.
--testDiagram :: PitchFactorDiagram -> IO ()
--testDiagram = putWAVEFile "diag.wav" . waveformToWAVE (3*32000) 32000 . tickTable 32000 . fmap doubleToDiscrete . buildTestTrack . realToFrac . diagramToRatio . normalizePFD
  --where
    --buildTestTrack p = sequenceNotes [((0,1),sinWave concertA),((1,2),sinWave (concertA * p)),((2,3), buildChord [1,p] concertA)]

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
buildChord :: [Double] -> Double -> Waveform Double Double
buildChord relPitches root = balanceChord $ map (triWave . (root *)) relPitches

-- | Builds a chord out of the given ratios relative to the root pitch, without normalizing the volume.
-- (Warning: may be loud)
buildChordNoBalance :: [Double] -> Double -> DWave
buildChordNoBalance relPitches root = mergeWaves $ map (triWave . (root *)) relPitches

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
--envelope :: Envelope Double Double -> DWave
--envelope e@(Envelope del att hol dec _ _) = susEnvelope e (del + att + hol + dec)

susEnvelope :: Envelope Tick Discrete -> Tick -> Wavetable
susEnvelope (Envelope del att hol dec sus rel) noteDuration = sampleFrom $ \t -> if t < del
  then 0
  else if t - del < att
    then doubleToDiscrete $ (t - del) `divTD` att
    else if t - del - att < hol
      then 1
      else if t - del - att - hol < dec
        then doubleToDiscrete $ 1 + ((t - del - att - hol) `divTD` dec) * (discreteToDouble sus - 1)
        else if t < noteDuration
          then sus
          else if t - noteDuration < rel
            then sus * doubleToDiscrete (1 - (t - noteDuration) `divTD` rel)
            else 0
  where
    divTD a b = fromIntegral a / fromIntegral b

suspendVelope :: Double -> Double -> Double -> Double -> Double -> DWave
suspendVelope del att hol dec sus = sampleFrom $ \t -> if t < del
  then 0
  else if t - del < att
    then (t - del) / att
    else if t - del - att < hol
      then 1
      else if t - del - att - hol < dec
        then 1 + (t - del - att - hol)/dec * (sus - 1)
        else sus

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

data Envelope a b = Envelope a a a a b a

discretizeEnvelope :: Double -> Envelope Double Double -> Envelope Tick Discrete
discretizeEnvelope tickRate (Envelope del att hol dec sus rel) = Envelope (dd del) (dd att) (dd hol) (dd dec) (doubleToDiscrete sus) (dd rel)
  where
    dd = floor . (*tickRate)

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
    -- !stepModifier = realToFrac . recip . fromIntegral $ stepsPerSide :: Double

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
        accu !tot (dt:dts) = do
          --let !() = unsafePerformIO $ readIORef accuCount >>= \x -> (if x `mod` 10000 == 0 then putStrLn ("accu: " ++ show x) else pure ()) >> writeIORef accuCount (x + 1)
          s <- BA.readByteArray v ((b + tickRadius + dt + 1) `mod` windowSize)
          let !toAdd = (0.01 * Discrete (BA.indexByteArray kernel dt)) * Discrete s
          accu (tot + toAdd) dts
        accu tot [] = pure tot
      k <- accu 0 sampleDeltas
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
    samplesPerSide = floor (convolutionRadius * convolutionSampleRate) :: Int
    --sampleCount = 2 * samplesPerSide + 1


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

type FilterParams = (Double,Double,Double)

ladderFilter :: Double -> [Double] -> [Double] -> [Double] -> [(Double,Double,Double,Double)]
ladderFilter sampleRate reso cutoffs inputs = scanl stepSt (1e-6,2e-6,0,0) $ zip3 reso cutoffs inputs
  where
    fmapTup f (!a,!b,!c,!d) = (f a,f b,f c,f d)
    zipTup f (!a,!b,!c,!d) (!a',!b',!c',!d') = (f a a',f b b',f c c',f d d')
    h = 1/sampleRate
    rkf (r,c,i) (x1,x2,x3,x4) = {-trace ("rkf(" ++ show r ++ ", " ++ show c ++ ", " ++ show i ++ ")") $-} let cut = 2 * pi * c in (cut * (i - r * x3 - x1),cut * (x1 - x2),cut * (x2 - x3),cut * (x3 - x4))
    stepSt x i = let
      k1 = fmapTup (h *) $ rkf i x
      k2 = fmapTup (h *) $ rkf i (zipTup (\k xi -> xi + k/2) k1 x)
      k3 = fmapTup (h *) $ rkf i (zipTup (\k xi -> xi + k/2) k2 x)
      k4 = fmapTup (h *) $ rkf i (zipTup (+) k3 x)
      in zipTup (\xi ki -> xi + ki/6) x $ zipTup (+) (zipTup (+) (fmapTup (*2) k2) (fmapTup (*2) k3)) (zipTup (+) k1 k4)

ladderFilter' :: Double -> [Double] -> [Double] -> [Double] -> [(Double,Double,Double,Double)]
ladderFilter' sampleRate reso cutoffs inputs = fmap (FV.convert :: FV.VecList 4 Double -> (Double,Double,Double,Double)) $ rkSolveStepSize (1/sampleRate) rkf (FV.mk4 1e-6 1e-6 1e-6 1e-6) (zip3 reso cutoffs inputs)
  where
    rkf (r,c,i) x = let cut = 2 * pi * c in FV.mk4 (cut * (i - r * (x FV.! 2) - (x FV.! 0))) (cut * (x FV.! 0 - x FV.! 1)) (cut * (x FV.! 1 - x FV.! 2)) (cut * (x FV.! 2 - x FV.! 3))

rkSolve :: Applicative f => (i -> f Double -> f Double) -> f Double -> [i] -> [f Double]
rkSolve rkf = scanl stepSt
  where
    stepSt x i = let
      k1 = rkf i x
      k2 = rkf i (liftA2 (\k xi -> xi + k/2) k1 x)
      k3 = rkf i (liftA2 (\k xi -> xi + k/2) k2 x)
      k4 = rkf i (liftA2 (+) k3 x)
      in liftA2 (\xi ki -> xi + ki/6) x $ liftA2 (+) (liftA2 (+) (fmap (*2) k2) (fmap (*2) k3)) (liftA2 (+) k1 k4)

rkSolveStepSize :: Applicative f => Double -> (i -> f Double -> f Double) -> f Double -> [i] -> [f Double]
rkSolveStepSize h rkf = rkSolve ((fmap (* h) .) . rkf)

scaledDrive :: Double -> Double
scaledDrive drv = (1 + drv) ** 5

lowPassFilter :: Double -> [Double] -> [Double] -> [Double] -> [Double]
lowPassFilter rate res cut = fmap (\(_,_,_,d) -> d) . ladderFilter rate res cut

synthFromFreqProfile :: (Double,Double) -> Double -> DWave -> DWave
synthFromFreqProfile (f0,f1) fSampRate prof = sampleFrom $ \t -> max (-1) $ min 1 $ sum . fmap ((/fromIntegral (length sampPoints)) . sampleAt t . getComponent) $ sampPoints
  where
    getComponent f = fmap (*sample prof f) $ sinWaveWithPhase f (pi/2)
    sampPoints = [f0, f0 + 1/fSampRate .. f1]
    --jitterFactor k = (fromIntegral (floor (((k - f0) / (f1 - f0)) * 81083) `mod` 115)) / 115
    --jitter k = k + (2/fSampRate) * jitterFactor k

saxProfile :: DWave
saxProfile = sampleFrom $ \f -> let x = f/fb in sqrt (n * x/(1 + x**7))
  where
    fb = 618
    n = 1.088910

synthFromDiscreteProfile :: [(Double,Double)] -> Wavetable
synthFromDiscreteProfile fs = {-solidSlice 0 (truncate $ stdtr/(1 :: Double)) .-} tickTable stdtr . discretize . mergeWaves . fmap (\(f,amp) -> fmap (*(normFactor * amp)) $ sinWaveWithPhase f f) $ fs
  where
    normFactor = 0.9/(sum $ fmap snd fs)

harmonicEquationToDiscreteProfile :: (Double -> Double) -> (Double -> Double) -> Double -> [(Double,Double)]
harmonicEquationToDiscreteProfile oddF evenF f0 = go (15 :: Int)
  where
    go 0 = []
    go k = (fromIntegral k * f0,(if even k then evenF else oddF) $ fromIntegral k) : go (k-1)

discSaxProfile :: [(Double,Double)]
discSaxProfile = f1 ++ f2
  where
  f1 = 
    [(588.6,0.0911)
    ,(1179,0.05463)
    ,(1767,0.03471)
    ,(2357,0.01109)
    ,(2946,0.01625)
    ] -- y=63/x ish
  f2 = 
    [(294.9,0.03032)
    ,(884,0.02445)
    ,(1474,0.01172)
    ,(2062,0.008224)
    ,(2651,0.00881)
    ,(3241,0.005567)
    ]

genSaxProfile' :: Double -> [(Double,Double)]
genSaxProfile' f0 =
  [(1 * f0,0.0474475510012406)
  ,(2 * f0,0.0141427241744339)
  ,(3 * f0,0.00838229600425522)
  ,(4 * f0,0.00452452580892121)
  ,(5 * f0,0.0129662632033567)
  ,(6 * f0,0.0102954061023286)
  ,(7 * f0,0.00246665413386754)
  ,(8 * f0,0.00309631105551752)
  ,(9 * f0,0.00313836031805197)
  ,(10 * f0,0.00549688733850934)
  ,(11 * f0,0.00826471159302181)
  ,(12 * f0,0.00280485189554740)
  ]
genSaxProfile :: Double -> [(Double,Double)]
genSaxProfile f0 = f1 ++ f2
  where
  f1 = 
    [(2 * f0,0.0911)
    ,(4 * f0,0.05463)
    ,(4 * f0,0.03471)
    ,(6 * f0,0.01109)
    ,(8 * f0,0.01625)
    ] -- y=63/x ish
  f2 = 
    [(f0,0.03032)
    ,(3 * f0,0.02445)
    ,(5 * f0,0.01172)
    ,(7 * f0,0.008224)
    ,(9 * f0,0.00881)
    ,(11 * f0,0.005567)
    ]
  --[(1023,0.14190)
  --,(2044,0.05463)
  --,(3064,0.03471)
  --,(5107,0.01625)
  --]

fakedSaxTimbre :: Double -> Wavetable
fakedSaxTimbre = synthFromDiscreteProfile . genSaxProfile'

chordSinTimbre :: ChordVoicing -> Double -> Wavetable
chordSinTimbre c r = discretize . tickTable stdtr . balanceChord . fmap (sinWave . flip intervalOf r) $ listVoices c

{-
sampledConvolve modf profile w = sampleFrom $ \p -> modf (sample (sample profile p) p) (sample w p)

takeSamples :: 
takeSamples sampleRate w = map (sample w . (/sampleRate)) [0 .. 115]
  ,waveSamples = [map (doubleToSample . sample w . (/sampleRate)) [0 .. fromIntegral (numFrames - 1)]]
-}


-- | Discretize the output of a @'Double'@ producing waveform
discretize :: Functor f => f Double -> f Discrete
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
realDFT tickRadius fTickRate w = discretize $ tickTable 1 $ fmap ((min 1) . magnitude) $ fourierTransform tickRadius fTickRate ((\x -> discreteToDouble x :+ 0) <$> solidSlice (-tickRadius) tickRadius w)

-- | Skip every @n@ ticks in the in the given @'Waveform'@.
-- @'sample' ('skipTicks' n w) k = 'sample' w (n*k)@
skipTicks :: Tick -- ^ @n@
          -> Waveform Tick a -> Waveform Tick a
skipTicks skipRate w = sampleFrom $ \t -> sample w (skipRate * t)

skipStream :: Tick -> [Discrete] -> [Discrete]
skipStream skipRate (x:xs) = x : skipStream skipRate (drop skipRate xs)
skipStream _ [] = []

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
  then (!! t) . fmap (doubleToDiscrete . magnitude) $ l
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
          split (x:y:xss) = (x:xt, y:yt) where (xt, yt) = split xss
          ts = zipWith (\z k -> exp' k n * z) zs [0..]
          exp' :: Int -> Int -> Complex Double
          exp' k a = cis $ -2 * pi * (fromIntegral k) / (fromIntegral a)
