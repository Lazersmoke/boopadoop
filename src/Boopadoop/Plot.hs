{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
module Boopadoop.Plot where

import Boopadoop
import Boopadoop.Ideate
import qualified Data.WAVE as WAVE
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BSB
import Data.IORef
import Control.Concurrent

foreign import ccall "cstuff.cpp PlayAudioStream" c_PlayAudioStream :: FunPtr AudioSourceCallback -> FunPtr StartCoordCallback -> IO ()
foreign import ccall "cstuff.cpp &sinWaveLDC" sinWaveLDC :: FunPtr AudioSourceCallback

type AudioSourceCallback = CUInt -> Ptr Float -> Ptr () -> IO (Ptr ())
foreign import ccall "wrapper" mkAudioSourceCallback :: AudioSourceCallback -> IO (FunPtr AudioSourceCallback)

type StartCoordCallback = CULong -> IO ()
foreign import ccall "wrapper" mkSCCB :: StartCoordCallback -> IO (FunPtr StartCoordCallback)

wavestreamAudioCallback :: IORef Bool -> [Discrete] -> AudioSourceCallback
wavestreamAudioCallback killSwitch ws size ptr _flagsPtr = do
  let (out,rest) = splitAt (fromIntegral size) ws
  doDie <- readIORef killSwitch
  --putStrLn $ "doDie = " ++ show doDie
  --putStrLn $ "null out = " ++ show (null out)
  if null out || doDie
    then pure nullPtr
    else do
      --_ <- forkIO $ let x = forceStreamEval (fromIntegral size) rest `seq` () in x `seq` pure x
      let (fp,_,l) = BSI.toForeignPtr . BSL.toStrict . BSB.toLazyByteString . foldl mappend mempty . concatMap packFloat $ out
      --putStrLn "About to memcpy!"
      _ <- withForeignPtr fp $ \xx -> BSI.memcpy (castPtr ptr) (castPtr xx) l *> pure 0
      --putStrLn "About to cast!"
      pure . castFunPtrToPtr =<< mkAudioSourceCallback (wavestreamAudioCallback killSwitch rest)
  where
    packFloat = replicate numChannels . BSB.floatLE . realToFrac . discreteToDouble
    numChannels = 2
{-
wavestreamAudioSource :: MVar BSL.ByteString -> AudioSourceCallback
wavestreamAudioSource outBytes size ptr _flagsPtr = do
  --putStrLn "Data callback called!"
  --putStrLn $ "Data pointer:" ++ show ptr
  --putStrLn $ "Flags pointer:" ++ show flagsPtr
  samplesToBuffer <- modifyMVar outBytes $ \k -> pure $ let (out,rest) = BSL.splitAt (fromIntegral size) k in {-trace ("Now buffering: " ++ show (BSL.length out)) $ -}if BSL.null out then (rest,Nothing) else (rest,Just out)
  ret <- case samplesToBuffer of
    Nothing -> pure 1
    Just out -> let (fp,_,l) = BSI.toForeignPtr (BSL.toStrict out) in withForeignPtr fp $ \xx -> BSI.memcpy (castPtr ptr) (castPtr xx) (4 * l) *> pure 0
  --putStrLn "Done putting data"
  pure ret
-}
forceStreamEval :: Int -> [Discrete] -> [Discrete]
forceStreamEval _ [] = []
forceStreamEval 0 xs = xs
forceStreamEval !i (x:xs) = x `seq` (x : forceStreamEval (i-1) xs)

playWavestream :: MVar CULong -> IORef Bool -> [Discrete] -> IO ()
playWavestream startCoord ks ws = do
  cb <- mkAudioSourceCallback (wavestreamAudioCallback ks ws)
  sccb <- mkSCCB $ putMVar startCoord
  c_PlayAudioStream cb sccb *> freeHaskellFunPtr cb *> freeHaskellFunPtr sccb

explainNotes :: TimeStream String -> IO ()
explainNotes (TimeStream t x xs) = do
  _ <- forkIO $ putStrLn x
  threadDelay (floor $ t * 0.8 * 1000000)
  explainNotes xs

chunkSamples :: [Discrete] -> Int -> [BSS.ByteString]
chunkSamples ws blockSize = let (out,rest) = splitAt blockSize ws in if null out then [] else let x = BSL.toStrict (BSB.toLazyByteString (foldl mappend mempty . concatMap packFloat $ out)) in x `seq` (x : chunkSamples rest blockSize)
  where
    packFloat = replicate numChannels . BSB.floatLE . realToFrac . discreteToDouble
    numChannels = 2

wavestreamToLazyByteString :: [Discrete] -> BSL.ByteString
wavestreamToLazyByteString xs = BSL.fromChunks $ chunkSamples xs stdtr

listenUnboundedWavestream :: [Discrete] -> IO ()
listenUnboundedWavestream = WAVE.putWAVEFile "listen.wav" . finiteWavestreamToWAVE stdtr

listenWavestream' :: Double -> [Discrete] -> IO ()
listenWavestream' t w = WAVE.putWAVEFile "listen.wav" (wavestreamToWAVE (floor $ stdtr * t) stdtr w)

listenWavestream :: [Discrete] -> IO ()
listenWavestream = listenWavestream' 5

listenWavetable :: Wavetable -> IO ()
listenWavetable = listenWavestream . streamWavetable

listenChord :: ChordVoicing PitchFactorDiagram -> IO ()
listenChord = listenWavetable . tickTable stdtr . discretize . balanceChord . map (sinWave . ($ concertA) . intervalOf) . getVoiceList

{-
listenArpChord :: ChordVoicing PitchFactorDiagram -> IO ()
listenArpChord = listenWavetable . compose (stdtr * 3) . fmap (tickTable stdtr . discretize . sinWave . (*concertA) . diagramToRatio) . arpegiate

listenChords :: TimeStream (ChordVoicing PitchFactorDiagram) -> IO ()
listenChords = listenWavetable . mediumFO . compose (stdtr * 7) . fmap (tickTable stdtr . discretize . balanceChord . map (sinWave . (*concertA) . diagramToRatio) . getVoiceList)
  where
    mediumFO :: Wavetable -> Wavetable
    mediumFO = amplitudeModulate (tickTable stdtr . discretize . fmap abs $ triWave 3)

-}
listenTimedSinesKey :: Double -> Timed (Maybe (Octaved PitchFactorDiagram)) -> IO ()
listenTimedSinesKey k = listenTimedTimbreKey k (waveTimbre sinWave)

listenTimedTimbreKey :: Double -> (Double -> Wavetable) -> Timed (Maybe (Octaved PitchFactorDiagram)) -> IO ()
listenTimedTimbreKey k timbre = listenUnboundedWavestream . composeTimed tempo . niceEnvelope tempo . fmap (fmap (timbre . flip intervalOf k))
  where
    tempo = floor $ stdtr / (4 :: Double)

listenTimeStreamTimbreKey :: Double -> (Double -> Wavetable) -> TimeStream (Maybe (Octaved PitchFactorDiagram)) -> IO ()
listenTimeStreamTimbreKey k timbre x = listenUnboundedWavestream $ makeWavestreamTimeStreamTimbreKey k timbre x

makeWavestreamTimeStreamTimbreKey :: Double -> (Double -> Wavetable) -> TimeStream (Maybe (Octaved PitchFactorDiagram)) -> [Discrete]
makeWavestreamTimeStreamTimbreKey k timbre = timeStreamToValueStream (fromIntegral tempo) . fmap (maybe emptyWave id . fmap (timbre . flip intervalOf k))
  where
    tempo :: Int
    tempo = floor stdtr

listenTimeStreamFollow :: Double -> (Double -> Wavetable) -> TimeStream (Maybe (Octaved PitchFactorDiagram)) -> IO ()
listenTimeStreamFollow k timbre ts = listenUnboundedWavestream . meshWavestreams . fmap timbre . followValue' stdtr 0.5 . stepCompose (8/stdtr) . (fmap . fmap) (flip intervalOf k) $ ts

listenTimeStream :: TimeStream Wavetable -> IO ()
listenTimeStream = listenWavestream . timeStreamToValueStream stdtr

listenTimbre :: (Double -> Wavetable) -> IO ()
listenTimbre tim = listenTimeStreamTimbreKey (intervalOf (shiftOctave (-1) unison) concertA) tim $ solFeckPFD "v0''''''0''''''0''''''0......0......0......0"

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
listenSolfeckTimbre tim = listenTimeStreamTimbreKey (intervalOf (shiftOctave (-1) unison) concertA) tim . solFeckPFD

analyzeWavestream :: Double -> [Discrete] -> IO ()
analyzeWavestream t w = listenWavestream' t w *> dumpFiniteWavestreamToScatter (take (floor $ stdtr * t) $ w)

dumpFiniteWavestreamToScatter :: Show a => [a] -> IO ()
dumpFiniteWavestreamToScatter = writeFile "scatter" . unlines . outS (0 :: Int)
  where
    outS !t (x:xs) = entryFor t x : outS (t + 1) xs
    outS _ [] = []
    entryFor t x = show t ++ " " ++ show x
