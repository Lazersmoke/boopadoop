module Main where

import Boopadoop.Example
import Boopadoop
import Boopadoop.Plot
import Boopadoop.Ideate
import Boopadoop.Rack

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
main = pure ()
  --playFile "listen.wav"
--dumpFiniteWavestreamToScatter . take 4000 . streamWavetable . discretize $ tickTable stdtr $ sinWave 440
--analyzeWavestream 1 . discretize . take stdtr . streamWavetable . tickTable stdtr . synthFromFreqProfile (400,480) (1/10) $ saxProfile

--main = listenWavestream . composeWithEnvelope ukeEnvelope (stdtr * 14) . fmap playUke $ mapleLeaf
--main = listenWavestream . composeWithEnvelope ukeEnvelope (stdtr * 14) . fmap (fmap (*0.8) . tickTable stdtr . discretize . sinWave) . toConcreteKey concertA $ theCanon
