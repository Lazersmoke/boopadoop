{-# LANGUAGE BangPatterns #-}
module Boopadoop.Example where

import Boopadoop
import Data.Complex

testProg :: DWave
testProg = sequenceNotes
  [((1,2),buildChord [4/4,5/4,6/4] (concertA * semi ** 0))
  ,((2,3),buildChord [5/5,6/5,7/5] (concertA * semi ** 0))
  ,((3,4),buildChord [6/6,7/6,8/6] (concertA * semi ** 0))
  ,((4,5),buildChord [semi ** (-5), semi ** 0, semi ** 4] (concertA * semi ** 7))
  ]

testDoot :: DWave
testDoot = amplitudeModulate (envelope 0.5 0.15 0 0.25 0.1 3) $ triWave concertA

wackRatio :: DWave
wackRatio = sequenceNotes
  [((0,2),sinWave concertA)
  ,((2,3),buildChord [4/4,5/4,6/4] concertA)
  ,((3,4),sinWave concertA)
  ,((4,5),sinWave (concertA * 7 / 4))
  ,((5,6),buildChord [4/4,7/4] concertA)
  ,((6,7),buildChord [4/4,5/4,7/4] concertA)
  ,((7,8),buildChord [4/4,5/4,6/4,7/4] concertA)
  ]

tripleOscEmulation :: Double -> DWave
tripleOscEmulation f = balanceChord
  [sinWave f
  ,sinWave $ intervalOf (scalePFD (-1) octave) f
  ,sinWave $ intervalOf (scalePFD (-2) octave) f
  ]

tiptoeEnvelope :: DWave
tiptoeEnvelope = envelope 0 0.052 0 0.393 0.0418 0.432

tiptoeEmulation :: Double -> DWave
tiptoeEmulation f = amplitudeModulate tiptoeEnvelope $ phaseModulate 0.005 (setVolume 0.2 $ sinWave f) (setVolume 0.38 $ triWave f)
{- LMMS Envelope XML:
<elvol lshp="0" latt="0" x100="0" rel="0.432" lpdel="0" lspd_denominator="4" ctlenvamt="0" lspd_numerator="4" lamt="0" lspd_syncmode="0" hold="0" amt="1" sustain="0.101" lspd="0.0418" att="0.052" pdel="0" userwavefile="" dec="0.393"/>
-}


downBeat :: Beat DWave
downBeat = RoseBeat [Beat (sinWave concertA),Rest,Beat (sinWave $ intervalOf perfectFifth concertA)]

filteredCont :: DWave
filteredCont = sampledConvolution 1600 0.02 (sampleFrom $ const $ bandpassFilter concertA 1) $ unfilteredCont

filteredDisc :: Waveform Double Discrete
filteredDisc = sampledConvolution 1600 0.02 (sampleFrom $ const $ bandpassFilter concertA 1) $ unfiltered

bandFilterIt :: DWave -> DWave
bandFilterIt = sampledConvolution 1600 0.05 (sampleFrom $ const $ bandpassFilter concertA 100)

unfilteredCont :: Waveform Double Double
unfilteredCont = sampleFrom $ \t -> let !w = (sample (sinWave concertA) t + sample (sinWave (18 / 13 * concertA)) t)* 0.5 in w

unfiltered :: Waveform Double Discrete
unfiltered = sampleFrom $ \t -> let !w = (sample (discretize $ sinWave concertA) t * 0.5 + sample (discretize $ sinWave (18 / 13 * concertA)) t * 0.5) in w

filteredTicks :: Wavetable
filteredTicks = tickConvolution (160) 10 (sampleFrom $ const theFilter) $ unfilteredTicks
  where
    !theFilter = optimizeFilter (160) $ tickTable stdtr $ bandpassFilter concertA 100

unfilteredTicks :: Wavetable
unfilteredTicks = solidSlice (0-160) (2*stdtr + 160) $ modulate (+) (setVolume 0.5 $ fastSin concertA stdtr) (setVolume 0.5 $ fastSin (18 / 13 * concertA) stdtr) 

testFourier :: Wavetable
testFourier = realDFT 100 44000 $ fWave

fWave :: Wavetable
fWave = solidSlice (-2000) (2000) . discretize . tickTable 44000 . sinWave $ concertA

outputTicks :: Wavetable
outputTicks = tickTable stdtr $ balanceChord [discretize $ sinWave concertA,discretize $ sampleFrom $ const 0]

outputGoal :: Waveform Double Discrete
outputGoal = balanceChord [discretize $ sinWave concertA,discretize $ sampleFrom $ const 0]



