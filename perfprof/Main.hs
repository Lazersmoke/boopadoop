{-# LANGUAGE BangPatterns #-}
module Main where

import Boopadoop.Example
import Boopadoop
import Boopadoop.Plot
import Boopadoop.Ideate
import Boopadoop.Rack

import System.Random
import Control.Concurrent.MVar
import Control.Concurrent
import Data.IORef
import Debug.Trace

main :: IO ()
main = do
  notes <- genMusic
  let ws = trace "got ws" $ makeWavestreamTimeStreamTimbreKey (intervalOf (shiftOctave (-1) unison) concertA) eqTimbre . fmap (Just . ttPFD . snd . getExplained) $ notes
  ks <- newIORef False
  pt <- newMVar ()
  wt <- newMVar ()
  startCoord <- newEmptyMVar
  _ <- forkIO $ readMVar startCoord *> putStrLn "Start Now!!!!!!!!"
  _ <- forkIO $ readMVar startCoord *> explainNotes (fmap getExplanation notes)
  playThread <- forkIO $ (takeMVar pt *> threadDelay 100 *> playWavestream startCoord ks ws *> putMVar pt ())
  writeOutThread <- forkIO $ (takeMVar wt *> listenWavestream' 15 ws *> putMVar wt ())
  putStrLn "Now playing! Press enter to stop"
  _ <- getLine
  putStr "Waiting for sound to stop playing..."
  writeIORef ks True
  _ <- takeMVar pt
  putStrLn " Check!"
  putStr "Waiting for listen.wav to be written..."
  _ <- takeMVar wt
  putStrLn " Check!"
  putStrLn "Goodbye."

genMusic :: IO (TimeStream (WithExplanation (PlayingContext TwelveTone,Octaved TwelveTone)))
genMusic = getStdGen >>= \rg -> pure $ followLeads timingRuleSet ruleSet (ttPC rg) (limitTimeStream 10 . trace "got jazz chords" . perfectDescJazzFifths $ chordOf $ fmap twelveTone [0,4,7])

chords :: TimeStream (Chord PitchFactorDiagram)
chords = TimeStream 2 (chordOf [unison,majorThird,perfectFifth,majorSeventh]) (TimeStream 1 (chordOf [perfectFourth,majorSixth,unison]) (TimeStream 1 (chordOf [perfectFifth,majorSeventh,majorSecond,perfectFourth]) chords))

perfectDescJazzFifths :: Chord TwelveTone -> TimeStream (Chord TwelveTone)
perfectDescJazzFifths startChord = TimeStream 1 startChord (perfectDescJazzFifths $ chordMap (<> twelveTone 5) startChord)

ruleSet :: [CompositionRule TwelveTone (Octaved TwelveTone)]
ruleSet = replicate 5 skipStep ++ replicate 30 keepRangeBounds ++ replicate 12 stepToCenter ++ replicate 8 leadingTone ++ replicate 8 continueStepping ++ replicate 6 (arpLC True) ++ replicate 6 (arpLC False)

timingRuleSet :: [CompositionRule a Rational]
timingRuleSet =
  [repeatLastTiming
  ,repeatLastTiming
  ,repeatLastTiming
  ,repeatLastTiming
  ,repeatLastTiming
  ,repeatLastTiming
  ,repeatLastTiming
  ,repeatLastTiming
  ,repeatLastTiming
  ,repeatLastTiming
  ,repeatLastTiming
  ,repeatLastTiming
  ,repeatLastTiming
  ,repeatLastTiming
  ,repeatLastTiming
  ,repeatLastTiming
  ,halfSpeed
  ,halfSpeed
  ,halfSpeed
  ,halfSpeed
  ,halfSpeed
  ,twiceAsFast
  ,twiceAsFast
  ,twiceAsFast
  ,twiceAsFast
  ,twiceAsFast
  ,unitTiming
  ,boundSpeedAbove (1/16)
  ,boundSpeedAbove (1/16)
  ,boundSpeedAbove (1/16)
  ,boundSpeedAbove (1/16)
  ,boundSpeedAbove (1/16)
  ,boundSpeedAbove (1/16)
  ,boundSpeedBelow 2
  ,boundSpeedBelow 2
  ,boundSpeedBelow 2
  ,boundSpeedBelow 2
  ,boundSpeedBelow 2
  ,boundSpeedBelow 2
  ,boundSpeedBelow 2
  ,boundSpeedBelow 2
  ]

eqTimbre :: Double -> Wavetable
eqTimbre = synthFromDiscreteProfile . harmonicEquationToDiscreteProfile (\x -> 0.1894 / (x ** 1.02)) (\x -> 0.0321 / (x ** 0.5669))


--listenTimeStreamTimbreKey :: Double -> (Double -> Wavetable) -> TimeStream (Maybe PitchFactorDiagram) -> IO ()
--playFile "listen.wav"
--windowsLoadLibrary "plugin.dll"
--dumpFiniteWavestreamToScatter . take 4000 . streamWavetable . discretize $ tickTable stdtr $ sinWave 440
--analyzeWavestream 1 . discretize . take stdtr . streamWavetable . tickTable stdtr . synthFromFreqProfile (400,480) (1/10) $ saxProfile

--main = listenWavestream . composeWithEnvelope ukeEnvelope (stdtr * 14) . fmap playUke $ mapleLeaf
--main = listenWavestream . composeWithEnvelope ukeEnvelope (stdtr * 14) . fmap (fmap (*0.8) . tickTable stdtr . discretize . sinWave) . toConcreteKey concertA $ theCanon
