module Main where

import Boopadoop
import Boopadoop.Example
import Boopadoop.Ideate

main :: IO ()
main = listenWavestream . composeWithEnvelope ukeEnvelope (stdtr * 14) . fmap playUke $ mapleLeaf
