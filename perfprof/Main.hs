module Main where

import Boopadoop.Example
import Boopadoop
import Boopadoop.Plot
import Boopadoop.Ideate
import Boopadoop.Rack

import System.Random

{-
import Sound.ALUT

playFile :: FilePath -> IO ()
playFile fileName = do
   -- Create an AL buffer from the given sound file.
   buf <- createBuffer (File fileName)

   -- Generate a single source, attach the buffer to it and start playing.
   source <- genObjectName
   buffer source $= Just buf
   play [source]

   -- Normally nothing should go wrong above, but one never knows...
   errs <- get alErrors
   unless (null errs) $ do
      hPutStrLn stderr (concat (intersperse "," [ d | ALError _ d <- errs ]))
      exitFailure

   -- Check every 0.1 seconds if the sound is still playing.
   let waitWhilePlaying = do
          sleep 0.1
          state <- get (sourceState source)
          when (state == Playing) $
             waitWhilePlaying
   waitWhilePlaying
-}
main :: IO ()
main = getStdGen >>= \rg -> {-listenTimeStreamFollow-} listenTimeStreamTimbreKey (intervalOf (invertPFD octave) concertA) eqTimbre $ limitTimeStream 100 $ fmap Just $ composeAlong (ruledPlaying timingRuleSet ruleSet) (defaultPlayingContext rg)

ruleSet :: [CompositionRule PitchFactorDiagram]
ruleSet =
  [skipStep
  ,skipStep
  ,skipStep
  ,skipStep
  ,skipStep
  ,keepRangeBounds
  ,keepRangeBounds
  ,keepRangeBounds
  ,keepRangeBounds
  ,keepRangeBounds
  ,keepRangeBounds
  ,keepRangeBounds
  ,keepRangeBounds
  ,keepRangeBounds
  ,keepRangeBounds
  ,keepRangeBounds
  ,keepRangeBounds
  ,keepRangeBounds
  ,keepRangeBounds
  ,stepToCenter
  ,continueStepping
  ,continueStepping
  ,continueStepping
  ,continueStepping
  ,continueStepping
  ,continueStepping
  ,leadingTone
  ,leadingTone
  ,leadingTone
  ,leadingTone
  ,leadingTone
  ,leadingTone
  ,leadingTone
  ,stepToTarget
  ,arpLC True
  ,arpLC True
  ,arpLC True
  ,arpLC False
  ,arpLC False
  ,arpLC False
  ]

timingRuleSet :: [CompositionRule Rational]
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
  ,halfSpeed
  ,halfSpeed
  ,halfSpeed
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
