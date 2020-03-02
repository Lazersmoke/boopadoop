{-# LANGUAGE BangPatterns #-}
module Main where

import Boopadoop.Example
import Boopadoop
import Boopadoop.Plot
import Boopadoop.Ideate
import Boopadoop.Rack

import System.Random
import System.IO
import Control.Concurrent.MVar
import Control.Concurrent
import Data.IORef
import Debug.Trace

main :: IO ()
main = main''

main'' :: IO ()
main'' = do
  putStr "Enter SolFeck or press enter to listent to generated music!\n> "
  hFlush stdout
  sf <- getLine
  if sf == "q" then pure () else do
    if null sf
      then main'
      else do
        let sf' = solFeck sf
        print sf'
        let ns = stretchTimeStream (1/16) sf'
        let ws = makeWavestreamTimeStreamTimbreKey (intervalOf (shiftOctave (-1) unison) concertA) eqTimbre . (fmap . fmap) ttPFD $ ns
        wt <- newMVar ()
        _writeOutThread <- forkIO $ (takeMVar wt *> writeFile "lilyout.ly" ("{ " ++ toLilyPond ns ++ " }") *> listenWavestream' 90 ws *> putMVar wt ())

        startCoord <- newEmptyMVar
        ks <- newIORef False
        pt <- newMVar ()
        _playThread <- forkIO $ (takeMVar pt *> playWavestream startCoord ks ws *> putMVar pt ())
        do
          putStrLn "Now playing! Press enter to stop"
          _ <- getLine
          putStr "Waiting for sound to stop playing..."
          hFlush stdout
          writeIORef ks True
        _ <- takeMVar pt
        _ <- takeMVar wt
        putStrLn "Goodbye."


main' :: IO ()
main' = do
  notes <- genMusic
  let sideOutTime = 10
  let noteStream = limitTimeStream (fromIntegral sideOutTime) . fmap (Just . snd . getExplained) $ notes
  let ws = makeWavestreamTimeStreamTimbreKey (intervalOf (shiftOctave (-1) unison) concertA) eqTimbre . fmap (Just . ttPFD . snd . getExplained) $ notes
  ks <- newIORef False
  pt <- newMVar ()
  wt <- newMVar ()
  startCoord <- newEmptyMVar
  _ <- forkIO $ readMVar startCoord *> putStrLn "Start Now!!!!!!!!"
  --_ <- forkIO $ readMVar startCoord *> explainNotes ks (fmap getExplanation notes)
  _playThread <- forkIO $ (takeMVar pt *> threadDelay 100 *> playWavestream startCoord ks ws *> putMVar pt ())
  _writeOutThread <- forkIO $ (takeMVar wt *> writeFile "lilyout.ly" ("{ " ++ toLilyPond noteStream ++ " }") *> listenWavestream' (fromIntegral sideOutTime) ws *> putMVar wt ())
  putStrLn "Now playing! Press enter to stop"
  _ <- getLine
  putStr "Waiting for sound to stop playing..."
  hFlush stdout
  writeIORef ks True
  _ <- takeMVar pt
  putStrLn " Check!"
  putStr "Waiting for listen.wav to be written..."
  hFlush stdout
  _ <- takeMVar wt
  putStrLn " Check!"
  putStrLn "Goodbye."

genMusic :: IO (TimeStream (WithExplanation (PlayingContext TwelveTone,Octaved TwelveTone)))
genMusic = getStdGen >>= \rg -> pure $ stepChangeContext (ttPC rg) (fmap (\chrd -> \pc -> pc {leadChord = Just chrd}) $ perfectDescJazzFifths (solFeckChrd "047b")) (ruledSeededPlaying EndStream phraseRuleSet ruleSet)
--genMusic = getStdGen >>= \rg -> pure $ followLeads timingRuleSet ruleSet (ttPC rg) (perfectDescJazzFifths $ chordOf $ fmap twelveTone [0,4,7,11])

chords :: TimeStream (Chord PitchFactorDiagram)
chords = TimeStream 2 (chordOf [unison,majorThird,perfectFifth,majorSeventh]) (TimeStream 1 (chordOf [perfectFourth,majorSixth,unison]) (TimeStream 1 (chordOf [perfectFifth,majorSeventh,majorSecond,perfectFourth]) chords))

perfectDescJazzFifths :: Chord TwelveTone -> TimeStream (Chord TwelveTone)
perfectDescJazzFifths startChord = TimeStream 10 startChord (perfectDescJazzFifths $ chordMap (<> twelveTone 5) startChord)

ruleSet :: [CompositionRule TwelveTone (Octaved TwelveTone)]
ruleSet = 
  replicate 5 skipStep ++ 
  replicate 30 keepRangeBounds ++ 
  replicate 10 stepToCenter ++ 
  replicate 15 leadingTone ++ 
  replicate 8 continueStepping ++ 
  replicate 15 (arpLC True) ++ 
  replicate 15 (arpLC False) ++
  replicate 8 hackyEightToSevenResolution ++
  replicate 8 diatonicResolutionDownToRoot

timingRuleSet :: [CompositionRule a Rational]
timingRuleSet = replicate 20 repeatLastTiming ++ replicate 4 halfSpeed ++ replicate 4 twiceAsFast ++ replicate 20 (boundSpeedAbove (1/16)) ++ replicate 20 (boundSpeedBelow 2)

phraseRuleSet :: [CompositionRule a (TimeStream ())]
phraseRuleSet = replicate 7 quarterNotes ++ replicate 15 eightNotes ++ replicate 5 sixteenths ++ replicate 2 halfNote ++ replicate 1 wholeNote

--eqTimbre :: Double -> Wavetable
--eqTimbre = synthFromDiscreteProfile . harmonicEquationToDiscreteProfile (\x -> 0.1894 / (x ** 1.02)) (\x -> 0.0321 / (x ** 0.5669))

--listenTimeStreamTimbreKey :: Double -> (Double -> Wavetable) -> TimeStream (Maybe PitchFactorDiagram) -> IO ()
--playFile "listen.wav"
--windowsLoadLibrary "plugin.dll"
--dumpFiniteWavestreamToScatter . take 4000 . streamWavetable . discretize $ tickTable stdtr $ sinWave 440
--analyzeWavestream 1 . discretize . take stdtr . streamWavetable . tickTable stdtr . synthFromFreqProfile (400,480) (1/10) $ saxProfile

--main = listenWavestream . composeWithEnvelope ukeEnvelope (stdtr * 14) . fmap playUke $ mapleLeaf
--main = listenWavestream . composeWithEnvelope ukeEnvelope (stdtr * 14) . fmap (fmap (*0.8) . tickTable stdtr . discretize . sinWave) . toConcreteKey concertA $ theCanon
