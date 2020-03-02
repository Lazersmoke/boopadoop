{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
module Boopadoop.Plot where

import Boopadoop
import Boopadoop.Ideate
import qualified Data.WAVE as WAVE
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BSB
import Data.IORef
import Data.Word
import Data.Function
import Control.Concurrent
import Numeric

import qualified System.Hardware.Serialport as Serial

data AudioClient

foreign import ccall "cstuff.cpp c_PlayAudioStream" c_PlayAudioStream :: Ptr AudioClient -> FunPtr AudioSourceCallback -> FunPtr StartCoordCallback -> IO ()
foreign import ccall "cstuff.cpp c_InitAudio" c_InitAudio :: IO (Ptr AudioClient)
foreign import ccall "cstuff.cpp c_ReleaseAudio" c_ReleaseAudio :: Ptr AudioClient -> IO ()

type AudioSourceCallback = CUInt -> Ptr BiFloat -> Ptr CUInt -> IO ()
foreign import ccall "wrapper" mkAudioSourceCallback :: AudioSourceCallback -> IO (FunPtr AudioSourceCallback)

type StartCoordCallback = CULong -> IO ()
foreign import ccall "wrapper" mkSCCB :: StartCoordCallback -> IO (FunPtr StartCoordCallback)

{-
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
-}

writeToOutputBuffer :: Storable a => [a] -> MVar (OutputBuffer a) -> IORef Bool -> IO ()
writeToOutputBuffer [] _ _ = pure ()
writeToOutputBuffer fs mob ks = do
  --fs <- readMVar mob >>= \obr -> pure $ forceStreamEval (outBufferSize obr) fs'
  doDie <- readIORef ks
  if doDie
    then pure ()
    else do
      ob <- takeMVar mob
      let bufSpace = spaceInBuffer ob
      if bufSpace > 0
        then do
          let (fsWrite,rest) = splitAt bufSpace fs
          pokeArray (advancePtr (outBufferPtr ob) (outBufferWriteOffset ob)) fsWrite
          putMVar mob ob {outBufferWriteOffset = outBufferWriteOffset ob + length fsWrite}
          writeToOutputBuffer rest mob ks
        else putMVar mob ob *> putStrLn "Blocking on write!" *> threadDelay 10000 *> writeToOutputBuffer fs mob ks

yeetFromOutputBuffer :: Storable a => Int -> MVar (OutputBuffer a) -> Ptr a -> IO ()
yeetFromOutputBuffer 0 _ _ = pure ()
yeetFromOutputBuffer reqSamps mob ptr = do
  ob <- takeMVar mob
  let sampsAvail = outBufferWriteOffset ob
  if sampsAvail < reqSamps
    then if reqSamps > outBufferSize ob
      then reallocArray (outBufferPtr ob) reqSamps >>= \obp' -> putMVar mob ob {outBufferPtr = obp', outBufferSize = reqSamps} *> putStrLn ("Resized out buffer to " ++ show reqSamps ++ "!") *> yeetFromOutputBuffer reqSamps mob ptr
      else putMVar mob ob *> putStrLn "Bytes weren't ready to be read from output buffer in time!" *> threadDelay 10000 *> yeetFromOutputBuffer reqSamps mob ptr
    else do
      let newOutInd = sampsAvail - reqSamps
      copyArray ptr (outBufferPtr ob) reqSamps
      moveArray (outBufferPtr ob) (advancePtr (outBufferPtr ob) reqSamps) newOutInd -- TODO: Ring buffer so this is fast again
      putMVar mob ob {outBufferWriteOffset = newOutInd}

spaceInBuffer :: OutputBuffer a -> Int
spaceInBuffer ob = outBufferSize ob - outBufferWriteOffset ob

bufferedAudioCallback :: MVar (OutputBuffer BiFloat) -> IORef Bool -> AudioSourceCallback
bufferedAudioCallback mob ks bytes ptr flagsPtr = do
  doDie <- readIORef ks
  if doDie
    then poke flagsPtr 0x02 -- kill flag
    else yeetFromOutputBuffer (fromIntegral bytes) mob ptr

mkOutputBuffer :: Storable a => Int -> IO (OutputBuffer a)
mkOutputBuffer initSize = do
  p <- mallocArray initSize
  pure $ OutputBuffer {outBufferPtr = p, outBufferWriteOffset = 0, outBufferSize = initSize}

data BiFloat = BiFloat !Float !Float deriving (Eq,Show)
instance Storable BiFloat where
  sizeOf _ = sizeOf @Float undefined + sizeOf @Float undefined
  alignment _ = alignment @Float undefined
  peek ptr = BiFloat <$> peek (castPtr ptr) <*> peekElemOff (castPtr ptr) 1
  poke ptr (BiFloat a b) = poke (castPtr ptr) a *> pokeElemOff (castPtr ptr) 1 b

data OutputBuffer a = OutputBuffer
  {outBufferPtr :: Ptr a
  ,outBufferWriteOffset :: Int
  ,outBufferSize :: Int
  }
{-
data WaveStreamIO a = ConsIO a (IO (WaveStreamIO a))

pullFromIOStream :: Int -> WaveStreamIO a -> IO ([a],WaveStreamIO a)
pullFromIOStream 0 s = pure ([],s)
pullFromIOStream !k (ConsIO a s') = (\(xs,s) -> (a:xs,s)) <$> pullFromIOStream (k - 1) s'
-}

forceStreamEval :: Int -> [a] -> [a]
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
  mob <- newMVar =<< mkOutputBuffer 0
  _ <- forkIO $ writeToOutputBuffer (fmap packFloat ws) mob ks
  cb <- mkAudioSourceCallback (bufferedAudioCallback mob ks)
  sccb <- mkSCCB $ (\_ -> forkIO (putMVar startCoord ()) *> pure ())
  ac <- c_InitAudio
  c_PlayAudioStream ac cb sccb *> freeHaskellFunPtr cb *> freeHaskellFunPtr sccb
  c_ReleaseAudio ac
  where
    packFloat = (\x -> BiFloat x x) . realToFrac . discreteToDouble

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
listenUnboundedWavestream ws = WAVE.putWAVEFile "out/listen.wav" . wavestreamToWAVE (length ws) stdtr $ ws

listenWavestream' :: Double -> [Discrete] -> IO ()
listenWavestream' t w = WAVE.putWAVEFile "out/listen.wav" (wavestreamToWAVE (floor $ stdtr * t) stdtr w)

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
    tempo = floor @Double stdtr

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
solFeckToLilyPond cRel = writeFile "out/lilyout.ly" . (\s -> "{ " ++ s ++ " }") . toLilyPond . (fmap . fmap) (<> cRel) . stretchTimeStream (1/8) . solFeck

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
  _ -> "000000"

solFeckArd :: String -> TimeStream BSS.ByteString
solFeckArd = stretchTimeStream 0.25 . TimeStream 0 (mkPkt 0x00 "01" {-"01"-}) . fmap (ttArdPacket . fmap getPitchClass) . solFeck

sendArduinoTimeStream :: TimeStream BSS.ByteString -> IO ()
sendArduinoTimeStream ts = Serial.withSerial "COM3" Serial.defaultSerialSettings $ \ser -> do
  threadDelay 3000000
  _ <- Serial.send ser (BSS.pack [0x23,0x01,0x00,0x00,0x00])
  --forkIO $ Serial.recv sp 1 >>= print
  go (TimeStream 3 (BSS.empty) ts) ser
  _ <- Serial.send ser (BSS.pack [0x23,0x00,0x00])
  putStrLn "Done!"
  threadDelay 3000000
  where
    go (TimeStream t x xs) sp = do
      _ <- Serial.send sp x
      _ <- Serial.send sp (BSS.pack (concat [[0x23,0x06],timeToBytes t]))
      --Serial.flush sp
      putStrLn $ "Sent " ++ show x ++ " with time " ++ show (timeToBytes t :: [Word8])
      threadDelay (floor $ t * 1000000)
      go xs sp
    go EndStream _ = pure ()
    timeToBytes time = let millis = floor (time * 1000) :: Int in [fromIntegral (millis `div` 0xff),fromIntegral (millis `mod` 0xff)]

playArdSolfeck :: String -> IO ()
playArdSolfeck sol = Serial.withSerial "COM3" Serial.defaultSerialSettings $ \ser -> do
  threadDelay 3000000
  _ <- Serial.send ser (BSS.pack [0x23,0x01,0x00,0x00,0x00])
  --forkIO $ Serial.recv sp 1 >>= print
  let ws = makeWavestreamTimeStreamTimbreKey (intervalOf (shiftOctave (-1) unison) concertA) eqTimbre . (fmap . fmap) ttPFD $ solFeck sol
  playWavestreamBlockUntilStart ws
  go (solFeckArd sol) ser
  _ <- Serial.send ser (BSS.pack [0x23,0x00,0x00])
  putStrLn "Done!"
  threadDelay 3000000
  where
    go (TimeStream t x xs) sp = do
      _ <- Serial.send sp x
      _ <- Serial.send sp (BSS.pack (concat [[0x23,0x06],timeToBytes t]))
      --Serial.flush sp
      putStrLn $ "Sent " ++ show x ++ " with time " ++ show (timeToBytes t :: [Word8])
      threadDelay (floor $ t * 1000000)
      go xs sp
    go _ _ = pure ()
    timeToBytes time = let millis = floor (time * 1000) :: Word16 in [fromIntegral (millis `div` 0xff),fromIntegral (millis `mod` 0xff)]

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
