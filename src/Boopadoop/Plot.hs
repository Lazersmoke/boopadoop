{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveFunctor #-}
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


dependOnParameters :: MVar a -> (a -> b -> c) -> [b] -> IO (IOLacedStream c)
dependOnParameters _ _ [] = error "empty stream depend on paramters"
dependOnParameters ma f (b:bs) = do
  a <- readMVar ma
  pure $ IOLacedStream (f a b) (dependOnParameters ma f bs)

streamToList :: IOLacedStream a -> IO [a]
streamToList (IOLacedStream a s) = (a:) <$> (s >>= streamToList)

listToStream :: [a] -> IOLacedStream a
listToStream (x:xs) = IOLacedStream x (pure $ listToStream xs)

example :: IOLacedStream Double -> IO (IOLacedStream BiFloat)
example ios = do
  fs <- streamToList ios
  let xs = emuVCO' fs (fmap packFloat . discretize $ sinWave 440)
  pure $ listToStream xs

delayStream :: IOLacedStream Char
delayStream = IOLacedStream 'a' (threadDelay 1000000 *> pure (IOLacedStream 'b' (pure delayStream)))

data IOLacedStream a = IOLacedStream a (IO (IOLacedStream a)) deriving Functor

fastForwardIOLaced :: Int -> IOLacedStream a -> IO ([a],IOLacedStream a)
fastForwardIOLaced 0 s = pure ([],s)
fastForwardIOLaced !k (IOLacedStream a s) = s >>= \next -> fastForwardIOLaced (k - 1) next >>= \(xs,s') -> pure (a:xs,s')


writeIOLaced :: Storable a => IOLacedStream a -> MVar (OutputBuffer a) -> IORef Bool -> IO ()
writeIOLaced ios mob ks = do
  doDie <- readIORef ks
  if doDie
    then pure ()
    else do
      ob <- takeMVar mob
      let bufSpace = spaceInBuffer ob
      if bufSpace > 0
        then do
          (fsWrite,rest) <- fastForwardIOLaced bufSpace ios
          pokeArray (advancePtr (outBufferPtr ob) (outBufferWriteOffset ob)) fsWrite
          putMVar mob ob {outBufferWriteOffset = outBufferWriteOffset ob + length fsWrite}
          writeIOLaced rest mob ks
        else putMVar mob ob *> threadDelay 10000 *> writeIOLaced ios mob ks


-- | Continuously keep the buffer supplied with values from the list, blocking until all values have been written
writeToOutputBuffer :: Storable a => [a] -> MVar (OutputBuffer a) -> IORef Bool -> IO ()
writeToOutputBuffer [] _ _ = pure ()
writeToOutputBuffer fs mob ks = do
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
        else putMVar mob ob *> threadDelay 10000 *> writeToOutputBuffer fs mob ks

-- | Copy exactly the specified number of values from the output buffer to the pointer, blocking until that many values are available.
-- If the requested amount is bigger than the buffer size, resize the buffer so more values can be written at once.
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

-- | Empty, writable space remaining in a buffer
spaceInBuffer :: OutputBuffer a -> Int
spaceInBuffer ob = outBufferSize ob - outBufferWriteOffset ob

-- | 'AudioSourceCallback' using 'yeetFromOutputBuffer'
bufferedAudioCallback :: MVar (OutputBuffer BiFloat) -> IORef Bool -> AudioSourceCallback
bufferedAudioCallback mob ks bytes ptr flagsPtr = do
  doDie <- readIORef ks
  if doDie
    then poke flagsPtr 0x02 -- kill flag
    else yeetFromOutputBuffer (fromIntegral bytes) mob ptr

-- | Create an output buffer with an initial size.
-- Doing @'mkOutputBuffer' 0@ is reasonable in conjunction with 'yeetFromOutputBuffer' because it will be resized as needed.
mkOutputBuffer :: Storable a => Int -> IO (OutputBuffer a)
mkOutputBuffer initSize = do
  p <- mallocArray initSize
  pure $ OutputBuffer {outBufferPtr = p, outBufferWriteOffset = 0, outBufferSize = initSize}

-- | Two float values packed together (left and right channel)
data BiFloat = BiFloat !Float !Float deriving (Eq,Show)

-- | Right next to each other, in order
instance Storable BiFloat where
  sizeOf _ = sizeOf @Float undefined + sizeOf @Float undefined
  alignment _ = alignment @Float undefined
  peek ptr = BiFloat <$> peek (castPtr ptr) <*> peekElemOff (castPtr ptr) 1
  poke ptr (BiFloat a b) = poke (castPtr ptr) a *> pokeElemOff (castPtr ptr) 1 b

-- | A storable array with a write index.
-- Roll your own thread safety using @'Mvar'@
data OutputBuffer a = OutputBuffer
  {outBufferPtr :: Ptr a
  ,outBufferWriteOffset :: Int
  ,outBufferSize :: Int
  }

forceStreamEval :: Int -> [a] -> [a]
forceStreamEval _ [] = []
forceStreamEval 0 xs = xs
forceStreamEval !i (x:xs) = x `seq` (x : forceStreamEval (i-1) xs)

-- | Play the wavestream, blocking until the playback starts, then returning
playWavestreamBlockUntilStart :: [Discrete] -> IO ()
playWavestreamBlockUntilStart ws = do
  sc <- newEmptyMVar
  ks <- newIORef False
  _ <- forkIO $ playWavestream sc ks (playByWriting ws)
  () <- takeMVar sc
  pure ()

-- | Play the wavestream, blocking until the stream ends.
-- Will fill the supplied @'MVar'@ as soon as playback begins.
-- Setting the @'IORef'@ to @'True'@ will tell the playback to terminate early (useful with infinite streams)
playWavestream :: MVar () -> IORef Bool -> (MVar (OutputBuffer BiFloat) -> IORef Bool -> IO ()) -> IO ()
playWavestream startCoord ks fillAction = do
  mob <- newMVar =<< mkOutputBuffer 0
  _ <- forkIO $ fillAction mob ks
  cb <- mkAudioSourceCallback (bufferedAudioCallback mob ks)
  sccb <- mkSCCB $ (\_ -> forkIO (putMVar startCoord ()) *> pure ())
  ac <- c_InitAudio
  c_PlayAudioStream ac cb sccb *> freeHaskellFunPtr cb *> freeHaskellFunPtr sccb
  c_ReleaseAudio ac

playByWriting :: [Discrete] -> MVar (OutputBuffer BiFloat) -> IORef Bool -> IO ()
playByWriting ws mob ks = writeToOutputBuffer (fmap packFloat ws) mob ks

playByReading :: IOLacedStream BiFloat -> MVar (OutputBuffer BiFloat) -> IORef Bool -> IO ()
playByReading ilsf mob ks = writeIOLaced ilsf mob ks

packFloat :: Discrete -> BiFloat
packFloat = (\x -> BiFloat x x) . realToFrac . discreteToDouble

-- | Log the strings in time. Imprecision inherited from @'threadDelay'@
explainNotes :: IORef Bool -> TimeStream String -> IO ()
explainNotes _ EndStream = pure ()
explainNotes ks (TimeStream t x xs) = readIORef ks >>= \doDie -> if doDie
  then pure ()
  else do
    putStrLn x
    threadDelay (floor $ t * 100000)
    explainNotes ks xs

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

listenTimeStreamTimbreKey :: Double -> (Double -> Wavetable) -> TimeStream (Maybe (Octaved PitchFactorDiagram)) -> IO ()
listenTimeStreamTimbreKey k timbre x = listenUnboundedWavestream $ makeWavestreamTimeStreamTimbreKey k timbre x

makeWavestreamTimeStreamTimbreKey :: Double -> (Double -> Wavetable) -> TimeStream (Maybe (Octaved PitchFactorDiagram)) -> [Discrete]
makeWavestreamTimeStreamTimbreKey k timbre = timeStreamToValueStream stdtr . fmap (maybe emptyWave id . fmap (timbre . flip intervalOf k))

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

-- | Make an arduino serial packet
mkPkt :: Word8 -> String -> BSS.ByteString
mkPkt pktId datas = BSS.pack $ 0x23 : pktId : go datas
  where
    go b@(_:_:xs) = readHex' (take 2 b) : go xs
    go _ = []

-- | Interact with an arduino via text prompt
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

-- | Read a singular byte off of a hex string
readHex' :: String -> Word8
readHex' x = case readHex x of {[] -> 0x00; ((a,_):_) -> a}
