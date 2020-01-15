{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Boopadoop.Example where

import Boopadoop
import Boopadoop.Ideate
import Debug.Trace

theFilter :: Wavetable
theFilter = optimizeFilter (160) $ tickTable stdtr $ bandpassFilter concertA 100

fastFilteredTicks :: [Discrete]
fastFilteredTicks = fastTickConvolutionFixedKern stdtr 0 16 theFilter $ unfilteredTicks

-- An artificially really expensive and slow to calculate source wave
unfilteredTicks :: Wavetable
unfilteredTicks = sampleFrom $ \t -> (if t `mod` 1000 == 0 then trace ("Got unfilteredTicks at Tick " ++ show t) else id) $ {-([0..] !! 10000) `seq`-} sampleAt t $ modulate (+) (setVolume 0.5 $ fastSin (floor $ stdtr/(concertA :: Double))) (setVolume 0.5 $ fastSin (floor $ stdtr / (18 / 13 * concertA :: Double))) 

testFourier :: Wavetable
testFourier = realDFT 100 44000 $ fWave

fWave :: Wavetable
fWave = solidSlice (-2000) (2000) . discretize . tickTable 44000 . sinWave $ concertA

swung :: Wavetable
swung = compose 16000 $ fmap sel swingIt
  where
    sel 0 = fastSin . floor $ stdtr/(concertA :: Double)
    sel _ = fastSin . floor $ (stdtr/concertA :: Double) / 2

soundPFD :: PitchFactorDiagram -> IO ()
soundPFD pfd = testWave (1.95) "soundPFD" . discretize . tickTable stdtr . sinWave . (*concertA) $ diagramToRatio pfd

--confuse :: Wavetable
--confuse = compose (2*stdtr) $ equalTime [Beat $ stdSin perfectFifth, Beat $ stdSin (consonantHalfway perfectFifth octave), Beat $ stdSin octave]

-- In key of Bm
metallicaOneSoloArps :: Beat PitchFactorDiagram
metallicaOneSoloArps = equalTime . fmap (repeatBeat 8) $ 
  [arpegiateLike [2,1,0] $ rebaseChord perfectFourth minorChord -- Em
  ,arpegiateLike [2,0,1] $ rebaseChord minorSecond (invChrd 1 majorChord) -- C
  ,arpegiateLike [2,1,0] $ minorChord -- Bm
  ,arpegiateLike [2,1,0] $ rebaseChord (invertPFD majorThird) (invChrd 1 majorChord) -- G
  ,arpegiateLike [2,1,0] $ rebaseChord (Factors [-4,1,1]) minorChord -- Am
  ,arpegiateLike [2,1,0] $ rebaseChord (addPFD (invertPFD perfectFifth) minorSecond) (invChrd 1 minorChord) -- F
  ]

metallicaTest :: GlobalMuSyncMagic -> Wavetable
metallicaTest = fmap (fmap getValue) . composeMuSync (stdtr * 7) . fmap (fmap (fmap discretize . tickTable stdtr) . fmap (modulate (\e s -> fmap (*e) s) env) . sinMuSync) . toConcreteKey (intervalOf (addPFD (invertPFD octave) majorSecond) concertA) $ metallicaOneSoloArps
  where
    env = envelope 0 0 0.05 0.01 0.5 0.01
