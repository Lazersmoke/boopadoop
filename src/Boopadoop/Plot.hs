{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications #-}
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
import qualified Data.ByteString.Char8 as BSC
import Data.IORef
import Data.Word
import Data.Function
import Control.Concurrent
import Numeric

import qualified System.Hardware.Serialport as Serial

foreign import ccall "cstuff.cpp PlayAudioStream" c_PlayAudioStream :: FunPtr AudioSourceCallback -> FunPtr StartCoordCallback -> IO ()
foreign import ccall "cstuff.cpp &sinWaveLDC" sinWaveLDC :: FunPtr AudioSourceCallback
foreign import ccall "cstuff.cpp hask_sleep" c_sleep :: CUInt -> IO ()

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
      _ <- withForeignPtr fp $ \xx -> BSI.memcpy (castPtr ptr) (castPtr xx) l *> pure (0 :: Int)
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

playWavestreamBlockUntilStart :: [Discrete] -> IO ()
playWavestreamBlockUntilStart ws = do
  sc <- newEmptyMVar
  ks <- newIORef False
  _ <- forkIO $ playWavestream sc ks ws
  () <- takeMVar sc
  pure ()

playWavestream :: MVar () -> IORef Bool -> [Discrete] -> IO ()
playWavestream startCoord ks ws = do
  cb <- mkAudioSourceCallback (wavestreamAudioCallback ks ws)
  sccb <- mkSCCB $ (\_ -> forkIO (putMVar startCoord ()) *> pure ())
  c_PlayAudioStream cb sccb *> freeHaskellFunPtr cb *> freeHaskellFunPtr sccb

explainNotes :: IORef Bool -> TimeStream String -> IO ()
explainNotes _ EndStream = pure ()
explainNotes ks (TimeStream t x xs) = readIORef ks >>= \doDie -> if doDie
  then pure ()
  else do
    putStrLn x
    threadDelay (floor $ t * 100000)
    explainNotes ks xs

chunkSamples :: [Discrete] -> Int -> [BSS.ByteString]
chunkSamples ws blockSize = let (out,rest) = splitAt blockSize ws in if null out then [] else let x = BSL.toStrict (BSB.toLazyByteString (foldl mappend mempty . concatMap packFloat $ out)) in x `seq` (x : chunkSamples rest blockSize)
  where
    packFloat = replicate numChannels . BSB.floatLE . realToFrac . discreteToDouble
    numChannels = 2

wavestreamToLazyByteString :: [Discrete] -> BSL.ByteString
wavestreamToLazyByteString xs = BSL.fromChunks $ chunkSamples xs stdtr

listenUnboundedWavestream :: [Discrete] -> IO ()
listenUnboundedWavestream ws = WAVE.putWAVEFile "listen.wav" . wavestreamToWAVE (length ws) stdtr $ ws

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
listenTimeStreamTimbreKey :: Double -> (Double -> Wavetable) -> TimeStream (Maybe (Octaved PitchFactorDiagram)) -> IO ()
listenTimeStreamTimbreKey k timbre x = listenUnboundedWavestream $ makeWavestreamTimeStreamTimbreKey k timbre x

makeWavestreamTimeStreamTimbreKey :: Double -> (Double -> Wavetable) -> TimeStream (Maybe (Octaved PitchFactorDiagram)) -> [Discrete]
makeWavestreamTimeStreamTimbreKey k timbre = timeStreamToValueStream (fromIntegral tempo) . fmap (maybe emptyWave id . fmap (timbre . flip intervalOf k))
  where
    tempo :: Int
    tempo = floor @Double (stdtr / 1.5)

listenTimeStreamFollow :: Double -> (Double -> Wavetable) -> TimeStream (Maybe (Octaved PitchFactorDiagram)) -> IO ()
listenTimeStreamFollow k timbre ts = listenUnboundedWavestream . meshWavestreams . fmap timbre . followValue' stdtr 0.5 . stepCompose (8/stdtr) . (fmap . fmap) (flip intervalOf k) $ ts

listenTimeStream :: TimeStream Wavetable -> IO ()
listenTimeStream = listenWavestream . timeStreamToValueStream stdtr

listenTimbre :: (Double -> Wavetable) -> IO ()
listenTimbre tim = listenTimeStreamTimbreKey (intervalOf (shiftOctave (-1) unison) concertA) tim $ solFeckPFD "v0''''''0''''''0''''''0......0......0......0"

listenSolfeck :: String -> IO ()
listenSolfeck = listenSolfeckTimbre (fmap (discretize . tickTable stdtr) sinWave)

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

solFeckToLilyPond :: Octaved TwelveTone -> String -> IO ()
solFeckToLilyPond cRel = writeFile "lilyout.ly" . (\s -> "{ " ++ s ++ " }") . toLilyPond . (fmap . fmap) (<> cRel) . stretchTimeStream (1/8) . solFeck

toLilyPond :: TimeStream (Maybe (Octaved TwelveTone)) -> String
toLilyPond (TimeStream t mx xs) = case mx of
  Just x -> let
      theOctave = replicate (max 0 $ 1 + getOctave x) '\''
      decodeTT = case getPitchClass x of
        MkTwelveTone 0 -> "c"
        MkTwelveTone 1 -> "cis"
        MkTwelveTone 2 -> "d"
        MkTwelveTone 3 -> "dis"
        MkTwelveTone 4 -> "e"
        MkTwelveTone 5 -> "f"
        MkTwelveTone 6 -> "fis"
        MkTwelveTone 7 -> "g"
        MkTwelveTone 8 -> "gis"
        MkTwelveTone 9 -> "a"
        MkTwelveTone 10 -> "ais"
        MkTwelveTone 11 -> "b"
        _ -> error "Bad Twelve Tone in lilypond"
    in decodeTT ++ theOctave ++ theTime ++ (if doDot then "." else "") ++ " " ++ toLilyPond xs
  Nothing -> "r" ++ theTime ++ " " ++ toLilyPond xs
  where
   theTime = show @Int . floor @Double . (2^^) . (if doDot then (+(1 :: Int)) else id) . round $ logTime
   doDot = odd @Int . round $ 2 * logTime
   logTime = logBase (2 :: Double) . realToFrac $ recip t
toLilyPond EndStream = ""

testStream :: TimeStream BSS.ByteString
testStream = TimeStream 0 (mkPkt 0x01 "000000") (TimeStream 0 (mkPkt 0x00 "01") (TimeStream 1 (mkPkt 0x01 "ff0000") (TimeStream 0.01 (mkPkt 0x01 "00ff00") (TimeStream 1 (mkPkt 0x01 "0000ff") (TimeStream 0 (mkPkt 0x00 "00") EndStream)))))

ttArdPacket :: Maybe TwelveTone -> BSS.ByteString
ttArdPacket Nothing = mkPkt 0x01 "000000"
ttArdPacket (Just tt) = mkPkt 0x01 $ case getTTNum tt of
  -- Primary
  0 -> "AA3939"
  4 -> "FFAAAA"
  7 -> "D46A6A"
  -- Secondary
  2 -> "AA6C39"
  5 -> "FFD1AA"
  9 -> "D49A6A"
  -- Also secondary
  3 -> "226666"
  8 -> "669999"
  10 -> "407F7F"
  -- Compl
  1 -> "2D882D"
  6 -> "88CC88"
  11 -> "55AA55"

solFeckArd :: String -> TimeStream BSS.ByteString
solFeckArd = stretchTimeStream 0.25 . TimeStream 0 (mkPkt 0x00 "01" {-"01"-}) . fmap (ttArdPacket . fmap getPitchClass) . solFeck

sendArduinoTimeStream :: TimeStream BSS.ByteString -> IO ()
sendArduinoTimeStream ts = Serial.withSerial "COM3" Serial.defaultSerialSettings $ \ser -> do
  threadDelay 3000000
  Serial.send ser (BSS.pack [0x23,0x01,0x00,0x00,0x00])
  --forkIO $ Serial.recv sp 1 >>= print
  go (TimeStream 3 (BSS.empty) ts) ser
  Serial.send ser (BSS.pack [0x23,0x00,0x00])
  putStrLn "Done!"
  threadDelay 3000000
  where
    go (TimeStream t x xs) sp = do
      Serial.send sp x
      Serial.send sp (BSS.pack (concat [[0x23,0x06],timeToBytes t]))
      --Serial.flush sp
      putStrLn $ "Sent " ++ show x ++ " with time " ++ show (timeToBytes t)
      threadDelay (floor $ t * 1000000)
      go xs sp
    go EndStream _ = pure ()
    timeToBytes time = let millis = floor (time * 1000) in [fromIntegral (millis `div` 0xff),fromIntegral (millis `mod` 0xff)]

playArdSolfeck :: String -> IO ()
playArdSolfeck sol = Serial.withSerial "COM3" Serial.defaultSerialSettings $ \ser -> do
  threadDelay 3000000
  Serial.send ser (BSS.pack [0x23,0x01,0x00,0x00,0x00])
  --forkIO $ Serial.recv sp 1 >>= print
  let ws = makeWavestreamTimeStreamTimbreKey (intervalOf (shiftOctave (-1) unison) concertA) eqTimbre . (fmap . fmap) ttPFD $ solFeck sol
  playWavestreamBlockUntilStart ws
  go (solFeckArd sol) ser
  Serial.send ser (BSS.pack [0x23,0x00,0x00])
  putStrLn "Done!"
  threadDelay 3000000
  where
    go (TimeStream t x xs) sp = do
      Serial.send sp x
      Serial.send sp (BSS.pack (concat [[0x23,0x06],timeToBytes t]))
      --Serial.flush sp
      putStrLn $ "Sent " ++ show x ++ " with time " ++ show (timeToBytes t)
      threadDelay (floor $ t * 1000000)
      go xs sp
    go _ _ = pure ()
    timeToBytes time = let millis = floor (time * 1000) in [fromIntegral (millis `div` 0xff),fromIntegral (millis `mod` 0xff)]

eqTimbre :: Double -> Wavetable
eqTimbre = synthFromDiscreteProfile . harmonicEquationToDiscreteProfile (\x -> 0.1894 / (x ** 1.02)) (\x -> 0.0321 / (x ** 0.5669))
{-
    recAck sp l fc = do
      threadDelay 100000
      fc' <- Serial.recv sp 1 >>= \x -> if BSS.null x then pure (fc + 1) else putStrLn "Got ack!" *> Serial.flush sp *> putMVar l () *> takeMVar l *> pure 0
      if fc' > 10
        then putMVar l () *> takeMVar l *> recAck sp l 0
        else recAck sp l fc'
-}

mkPkt :: Word8 -> String -> BSS.ByteString
mkPkt pktId datas = BSS.pack $ 0x23 : pktId : go datas
  where
    go b@(_:_:xs) = readHex' (take 2 b) : go xs
    go _ = []

doArduinoThing :: IO ()
doArduinoThing = Serial.withSerial "COM3" Serial.defaultSerialSettings $ \ser -> threadDelay 3000000 *> do
  lock <- newMVar ()
  _ <- forkIO $ fix $ \f -> do
    threadDelay 100000
    () <- takeMVar lock
    Serial.recv ser 100 >>= \x -> if BSS.null x then pure () else pure () --putStrLn ("\n<Arduino> " ++ BSC.unpack x ++ "\n")
    putMVar lock ()
    f
  doPrompt ser lock
  where
    doPrompt sp l = do
      putStr "> "
      go sp l
    go sp l = do
      putMVar l ()
      c <- getLine
      () <- takeMVar l
      case c of
        ('m':xs) -> do
          _ <- Serial.send sp $ BSS.pack [0x23,0x00,readHex' (take 2 xs)]
          Serial.flush sp
          doPrompt sp l
        ('c':xs) -> do
          _ <- Serial.send sp $ mkPkt 0x01 (take 6 xs)
          Serial.flush sp
          doPrompt sp l
        ('u':xs) -> do
          _ <- Serial.send sp $ BSS.pack [0x23,0x02,readHex' (take 2 xs),readHex' (take 2 (drop 2 xs))]
          Serial.flush sp
          doPrompt sp l
        ('p':xs) -> do
          _ <- Serial.send sp $ BSS.pack [0x23,0x03,readHex' (take 2 xs),readHex' (take 2 (drop 2 xs)),readHex' (take 2 (drop 4 xs)),readHex' (take 2 (drop 6 xs))]
          Serial.flush sp
          doPrompt sp l
        ('f':xs) -> do
          _ <- Serial.send sp $ BSS.pack [0x23,0x04,readHex' (take 2 xs),readHex' (take 2 (drop 2 xs)),readHex' (take 2 (drop 4 xs))]
          Serial.flush sp
          doPrompt sp l
        ('s':xs) -> do
          _ <- Serial.send sp $ BSS.pack [0x23,0x05,readHex' (take 2 xs)]
          Serial.flush sp
          doPrompt sp l
        ('d':xs) -> do
          _ <- Serial.send sp $ BSS.pack [0x23,0x06,readHex' (take 2 xs),readHex' (take 2 (drop 2 xs))]
          Serial.flush sp
          doPrompt sp l
        "q" -> putMVar l () *> threadDelay 3000000
        "\n" -> go sp l
        x -> do
          putStrLn $ "What is " ++ show x ++ "?"
          doPrompt sp l

readHex' :: String -> Word8
readHex' x = case readHex x of {[] -> 0x00; ((a,_):_) -> a}
