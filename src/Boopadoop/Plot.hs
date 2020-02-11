{-# LANGUAGE BangPatterns #-}
module Boopadoop.Plot where

import Boopadoop
import Boopadoop.Ideate
import qualified Data.WAVE as WAVE

listenUnboundedWavestream :: [Discrete] -> IO ()
listenUnboundedWavestream = WAVE.putWAVEFile "listen.wav" . finiteWavestreamToWAVE stdtr

listenWavestream' :: Double -> [Discrete] -> IO ()
listenWavestream' t w = WAVE.putWAVEFile "listen.wav" (wavestreamToWAVE (floor $ stdtr * t) stdtr w)

listenWavestream :: [Discrete] -> IO ()
listenWavestream = listenWavestream' 5

listenWavetable :: Wavetable -> IO ()
listenWavetable = listenWavestream . streamWavetable

listenChord :: ChordVoicing -> IO ()
listenChord = listenWavetable . tickTable stdtr . discretize . balanceChord . map (sinWave . (*concertA) . diagramToRatio) . listVoices

listenArpChord :: ChordVoicing -> IO ()
listenArpChord = listenWavetable . compose (stdtr * 3) . fmap (tickTable stdtr . discretize . sinWave . (*concertA) . diagramToRatio) . arpegiate

listenChords :: Beat ChordVoicing -> IO ()
listenChords = listenWavetable . mediumFO . compose (stdtr * 7) . fmap (tickTable stdtr . discretize . balanceChord . map (sinWave . (*concertA) . diagramToRatio) . listVoices)
  where
    mediumFO :: Wavetable -> Wavetable
    mediumFO = amplitudeModulate (tickTable stdtr . discretize . fmap abs $ triWave 3)

listenBeats :: Beat Wavetable -> IO ()
listenBeats b = listenWavetable $ compose (7 * stdtr) b

listenSinesKey :: Double -> Double -> Beat PitchFactorDiagram -> IO ()
listenSinesKey k l = listenWavestream' l . streamWavetable . compose (floor $ l * stdtr) . fmap (tickTable stdtr . discretize . sinWave) . toConcreteKey k

listenTimedSinesKey :: Double -> Timed (Maybe PitchFactorDiagram) -> IO ()
listenTimedSinesKey k = listenTimedTimbreKey k (waveTimbre sinWave)

listenTimedTimbreKey :: Double -> (Double -> Wavetable) -> Timed (Maybe PitchFactorDiagram) -> IO ()
listenTimedTimbreKey k timbre = listenUnboundedWavestream . composeTimed tempo . niceEnvelope tempo . fmap (fmap (timbre . flip intervalOf k))
  where
    tempo = floor $ stdtr / (4 :: Double)

listenTimeStreamTimbreKey :: Double -> (Double -> Wavetable) -> TimeStream (Maybe PitchFactorDiagram) -> IO ()
listenTimeStreamTimbreKey k timbre = listenUnboundedWavestream . timeStreamToValueStream (fromIntegral tempo) . fmap (maybe emptyWave id . fmap (timbre . flip intervalOf k))
  where
    tempo :: Int
    tempo = floor $ stdtr / (4 :: Double)

listenTimeStreamFollow :: Double -> (Double -> Wavetable) -> TimeStream (Maybe PitchFactorDiagram) -> IO ()
listenTimeStreamFollow k timbre ts = listenUnboundedWavestream . meshWavestreams . fmap timbre . followValue' stdtr 0.5 . stepCompose (8/stdtr) . (fmap . fmap) (flip intervalOf k) $ ts

listenTimeStream :: TimeStream Wavetable -> IO ()
listenTimeStream = listenWavestream . timeStreamToValueStream stdtr

listenTimbre :: (Double -> Wavetable) -> IO ()
listenTimbre tim = listenTimeStreamTimbreKey (intervalOf (invertPFD octave) concertA) tim $ solFeck "v0''''''0''''''0''''''0......0......0......0"

niceEnvelope :: Tick -> Timed (Maybe Wavetable) -> Timed Wavetable
niceEnvelope tempo = overTimings (\k -> maybe emptyWave (amplitudeModulate (env k)))
  where
    env k = susEnvelope de (tempo * k - floor ((0.01 :: Double) * stdtr))
    de = (discretizeEnvelope stdtr $ Envelope 0.001 0.01 0.07 0.01 0.5 0.01)

--niceEnvelopets :: Tick -> TimeStream (Maybe Wavetable) -> TimeStream Wavetable
--niceEnvelopets tempo = overTimingsTimeStream (\r -> maybe emptyWave (amplitudeModulate (env r)))
  --where
    --env k = susEnvelope de (fromRational k - 0.01)
    --de = (discretizeEnvelope stdtr $ Envelope 0.001 0.01 0.07 0.01 0.5 0.01)

listenSolfeck :: String -> IO ()
listenSolfeck = listenSolfeckTimbre (waveTimbre sinWave)

listenSolfeckTimbre :: (Double -> Wavetable) -> String -> IO ()
listenSolfeckTimbre tim = listenTimeStreamTimbreKey (intervalOf (invertPFD octave) concertA) tim . solFeck

analyzeWavestream :: Double -> [Discrete] -> IO ()
analyzeWavestream t w = listenWavestream' t w *> dumpFiniteWavestreamToScatter (take (floor $ stdtr * t) $ w)

dumpFiniteWavestreamToScatter :: Show a => [a] -> IO ()
dumpFiniteWavestreamToScatter = writeFile "scatter" . unlines . outS (0 :: Int)
  where
    outS !t (x:xs) = entryFor t x : outS (t + 1) xs
    outS _ [] = []
    entryFor t x = show t ++ " " ++ show x
