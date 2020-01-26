module Main where

import Boopadoop
import Boopadoop.Example
import Boopadoop.Ideate

main :: IO ()
main = listenWavestream vcvSound
--main = listenWavestream . composeWithEnvelope ukeEnvelope (stdtr * 14) . fmap playUke $ mapleLeaf
--main = listenWavestream . composeWithEnvelope ukeEnvelope (stdtr * 14) . fmap (fmap (*0.8) . tickTable stdtr . discretize . sinWave) . toConcreteKey concertA $ theCanon
