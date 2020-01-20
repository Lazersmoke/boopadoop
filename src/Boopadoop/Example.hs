{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Boopadoop.Example where

import Boopadoop
import Boopadoop.Ideate
import Debug.Trace
import qualified Data.WAVE as WAVE

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

metallicaSlurred :: [Discrete]
metallicaSlurred = streamSlurs (stdtr * 7) . fmap ((3000,) . fmap (*0.4) . discretize . tickTable stdtr . amplitudeModulate env . sinWave) . toConcreteKey (intervalOf (invertPFD minorSeventh) concertA) $ metallicaOneSoloArps
  where
    env = envelope 0 0.01 0.07 0.01 0.5 0.005

listenWavestream :: [Discrete] -> IO ()
listenWavestream w = WAVE.putWAVEFile "listenWavestream.wav" (wavestreamToWAVE (stdtr * 7) stdtr w)

listenWavetable :: Wavetable -> IO ()
listenWavetable w = print w >> WAVE.putWAVEFile "listenWavetable.wav" (waveformToWAVE (stdtr * 2) stdtr w)

listenChord :: Chord -> IO ()
listenChord = (\w -> print w >> WAVE.putWAVEFile "listenChord.wav" (waveformToWAVE (stdtr * 10) stdtr w)) . tickTable stdtr . discretize . balanceChord . map (sinWave . (*concertA) . diagramToRatio) . chordPitches

listenArpChord :: Chord -> IO ()
listenArpChord = (\w -> print w >> WAVE.putWAVEFile "listenChord.wav" (waveformToWAVE (stdtr * 3) stdtr w)) . compose (stdtr * 3) . fmap (tickTable stdtr . discretize . sinWave . (*concertA) . diagramToRatio) . arpegiate

fphExamp :: Beat Chord
fphExamp = RoseBeat $
  [(2,Beat $ closedFPH 1 $ rebaseChord unison majorChord)
  ,(1,Beat $ closedFPH 0 $ rebaseChord (invertPFD perfectFourth) majorChord) -- V
  ,(1,Beat $ closedFPH 2 $ rebaseChord unison majorChord)
  ,(2,Beat $ closedFPH 1 $ rebaseChord (invertPFD perfectFifth) majorChord) -- IV
  ,(2,Beat $ closedFPH 2 $ rebaseChord unison majorChord)
  ,(1,Beat $ closedFPH 0 $ rebaseChord (invertPFD perfectFourth) majorChord) -- V
  ,(1,Beat $ closedFPH 2 $ rebaseChord (invertPFD minorThird) minorChord) -- vi
  ,(1,Beat $ closedFPH 0 $ rebaseChord (invertPFD perfectFifth) majorChord) -- IV
  ,(1,Beat $ closedFPH 2 $ rebaseChord (invertPFD perfectFourth) majorChord) -- V
  ,(4,Beat $ addPitch (invertPFD octave) $ voiceChord [1,2,0] $ rebaseChord unison majorChord) -- I
  ]

fromClosedFPH :: [(Int,Int,PitchFactorDiagram,Chord)] -> Beat Chord
fromClosedFPH = RoseBeat . map (\(t,ov,p,c) -> (t,Beat $ closedFPH ov $ rebaseChord p c))

fphExamp' :: Beat Chord
fphExamp' = fromClosedFPH
  [(2,1,unison,majorChord)
  ,(1,0,invertPFD perfectFourth,majorChord)
  ,(1,2,unison,majorChord)
  ,(2,1,invertPFD perfectFifth,majorChord)
  ,(2,2,unison,majorChord)
  ,(1,0,invertPFD perfectFourth,majorChord)
  ,(1,2,invertPFD minorThird,minorChord)
  ,(1,0,invertPFD perfectFifth,majorChord)
  ,(1,2,invertPFD perfectFourth,majorChord)
  ,(4,1,unison,majorChord)
  ]

listenChords :: Beat Chord -> IO ()
listenChords = (\w -> print w >> WAVE.putWAVEFile "listenChords.wav" (waveformToWAVE (stdtr * 7) stdtr w)) . mediumFO . compose (stdtr * 7) . fmap (tickTable stdtr . discretize . balanceChord . map (sinWave . (*concertA) . diagramToRatio) . chordPitches)

mediumFO :: Wavetable -> Wavetable
mediumFO = amplitudeModulate (tickTable stdtr . discretize . fmap abs $ triWave 3)

testStream :: [Discrete]
testStream = streamSlurs 150 $ RoseBeat
  [(1,Beat (10,sampleFrom $ const 0.1))
  ,(1,Beat (20,sampleFrom $ const 0.2))
  ,(1,Beat (30,sampleFrom $ const 0.3))
  ]
