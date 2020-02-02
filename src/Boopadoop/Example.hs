{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
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

metallicaSlurred :: [Discrete]
metallicaSlurred = streamSlurs (stdtr * 7) . fmap ((3000,) . fmap (*0.4) . discretize . tickTable stdtr . amplitudeModulate env . sinWave) . toConcreteKey (intervalOf (invertPFD minorSeventh) concertA) $ metallicaOneSoloArps
  where
    env = envelope 0 0.01 0.07 0.01 0.5 0.005

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

testStream :: [Discrete]
testStream = streamSlurs 150 $ RoseBeat
  [(1,Beat (10,sampleFrom $ const 0.1))
  ,(1,Beat (20,sampleFrom $ const 0.2))
  ,(1,Beat (30,sampleFrom $ const 0.3))
  ]

ukeTuning :: Int -> Double
ukeTuning n = intervalOf (scalePFD (fromIntegral n) perfectFourth) lowG
  where
    lowG = intervalOf (addPFD (invertPFD octave) minorSeventh) concertA

ukeSoundTable :: Int -> Int -> Wavetable
ukeSoundTable s f = discretize . tickTable stdtr . sinWave $ (ukeTuning (s - 1)) * semi ** fromIntegral f

mapleLeaf :: Beat UkeTab
mapleLeaf = RoseBeat $ concat [m1,m2,m3,m4,m9,m10]
  where
    (a,b,c) = (pickUke 2 8, pickUke 3 7, UkeTab Nothing (Just 3) Nothing (Just 5))
    (a',b') = (pickUke 2 7, pickUke 3 5)
    m1 =
      [(1,Beat ukeRest)
      ,(1,Beat a)
      ,(1,Beat c)
      ,(1,Beat a)
      ,(1,Beat b)
      ,(2,Beat c)

      ,(1,Beat a')
      ,(1,Beat c)
      ,(1,Beat a')
      ,(1,Beat b')
      ,(5,Beat c)
      ]
    m2 = init m1 ++ [(3,Beat c),(1,Beat ukeRest),(1,Beat c)]
    m3 =
      [(1,Beat ukeRest)
      ,(1,Beat a)
      ,(1,Beat $ pickUke 3 6)
      ,(1,Beat $ pickUke 4 6)
      ,(1,Beat ukeRest)
      ,(1,Beat c)
      ,(1,Beat ukeRest)
      ,(1,Beat c)
      ]
    m4 = (iterate init m3 !! 2) ++ [(2,Beat ukeRest)]
    hn = Beat $ pickUke 4 10
    m9 = replicate 3 (2,hn) ++ 
      [(1,hn)
      ,(2,hn)
      ,(1,Beat $ pickUke 4 5)
      ,(1,Beat $ pickUke 4 7)
      ,(1,Beat $ pickUke 3 7)
      ,(1,Beat $ pickUke 4 5)
      ,(2,Beat $ UkeTab Nothing (Just 8) Nothing (Just 7))
      ,(1,Beat $ UkeTab Nothing (Just 4) (Just 3) Nothing)
      ,(1,Beat $ pickUke 4 0)
      ,(1,Beat $ UkeTab Nothing (Just 4) Nothing (Just 1))
      ,(1,Beat $ pickUke 3 3)
      ,(1,Beat $ pickUke 4 0)
      ,(2,Beat $ UkeTab Nothing (Just 3) Nothing (Just 2))
      ,(1,Beat $ pickUke 3 3)
      ,(1,Beat $ UkeTab Nothing (Just 3) Nothing (Just 2))
      ,(1,Beat $ pickUke 3 3)
      ,(1,Beat $ UkeTab Nothing (Just 3) Nothing (Just 0))
      ,(1,Beat $ UkeTab Nothing (Just 3) (Just 3) Nothing)
      ]
    m10 = ((1,Beat ukeRest) : (3,hn) : tail m9) ++ [(2,Beat ukeRest)]

data UkeTab = UkeTab (Maybe Int) (Maybe Int) (Maybe Int) (Maybe Int)

ukeRest :: UkeTab
ukeRest = UkeTab Nothing Nothing Nothing Nothing

pickUke :: Int -> Int -> UkeTab
pickUke 1 f = UkeTab (Just f) Nothing Nothing Nothing
pickUke 2 f = UkeTab Nothing (Just f) Nothing Nothing
pickUke 3 f = UkeTab Nothing Nothing (Just f) Nothing
pickUke 4 f = UkeTab Nothing Nothing Nothing (Just f)
pickUke s _ = error $ "Uke has only 4 strings, not " ++ show s

displayUkeTab :: Char -> UkeTab -> String
displayUkeTab ch (UkeTab a b c d) = [k a,k b,k c,k d]
  where
    k (Just p) = "0123456789abcdef" !! p
    k Nothing = ch

writeUkeTab :: Beat UkeTab -> String
writeUkeTab = unlines . beatList . flattenTimes (fmap toUkeHold) . fmap (displayUkeTab '-')

ukeHF :: Beat UkeTab -> Beat UkeTab
ukeHF _ = Beat ukeRest

toUkeHold :: String -> String
toUkeHold = map (\c -> if c == '-' then '-' else '|')

playUke :: UkeTab -> Wavetable
playUke (UkeTab a b c d) = mergeWaves . zipWith mkSnd [1,2,3,4] $ [a,b,c,d]
  where
    mkSnd s (Just f) = setVolume 0.25 $ ukeSoundTable s f
    mkSnd _ Nothing = emptyWave

ukeEnvelope :: Envelope Tick Discrete
ukeEnvelope = discretizeEnvelope stdtr $ Envelope 0 0.01 0.05 0.01 0.5 0.01

theCanon :: Beat PitchFactorDiagram
theCanon = equalTime . fmap (repeatBeat 2 . arpegiateLike [2,0,1,2,1,0,1,0,2,0,1,2]) $ 
  [rebaseChord octave majorChord -- I
  ,rebaseChord perfectFifth majorChord -- V
  ,rebaseChord majorSixth minorChord -- vi
  ,invChrd 1 majorChord -- I6
  ,rebaseChord perfectFourth majorChord -- IV
  ,rebaseChord octave majorChord -- I
  ,rebaseChord perfectFourth majorChord -- IV
  ,rebaseChord perfectFifth majorChord -- V
  ]
exampleModulator :: Waveform Tick Double
exampleModulator = tickTable stdtr . modulate (\x -> oscAbout (1/stdtr) (x/(3*stdtr))) (sinWave 0.5) $ sinWave 0.25

threeVCO :: [Discrete]
threeVCO = discretize $ niceVCO 3 (niceVCO 30 veryLFO lfo) $ sinWave concertA
  where
    veryLFO = streamWavetable $ tickTable stdtr $ sampleFrom $ rampFrom 0.25 5
    lfo = {-streamWavetable $ tickTable stdtr $-} sinWave 20

lazyFeedbackTest :: [Discrete]
lazyFeedbackTest = seed ++ (emuVCO' (fmap (/stdtr) $ zipWith (+) (slowSin 0.5 0.15) $ zipWith (*) (slowSin 0.1 0.5) $ fmap discreteToDouble lazyFeedbackTest) $ discretize $ sinWave concertA)
  where
    seed = [0.1,0.2,0.5]
    slowSin a f = fmap (* a) $ streamWavetable $ tickTable stdtr $ sinWave f

vcvSound :: [Discrete]
vcvSound = discretize $ lowPassFilter stdtr (fmap (\x -> (x ** 2) * 10) $ repeat 0.234) cuts $ streamWavetable $ tickTable stdtr $ amplitudeModulate env $ sawWave 261.63
  where
    env = suspendVelope 0 0.1 0 0.93757 0.5
    cuts = streamWavetable . tickTable stdtr . fmap (\x -> 261.6256 * (2 ** (x + freqParam))) $ env
    freqParam = 1

blueBossaSolFeck :: String
blueBossaSolFeck = "4-^4--...-.------.-.---^4--.=8x--...-.------.-.---^2--.=8~a2--...-.------.-x-.i-!`-=8~05-.`--.-.`---..=8"

blackWidowSolFeck :: String
blackWidowSolFeck = "5--7--8-7-5-7--8--7--5--8--a--b-a-8-5--7--8-7-5-8--a--b-a-8-a--0--1-0-a-8--a--b-a-8-a--0--1-0-a-8--a--b-a-8-a--b--a--8--b--1--2-1-b-8--a--b-a-8-b--1--2-1-b-1--3--4-3-1-7=6=6"

warmupSolFeck :: String
warmupSolFeck = "0```'!,,'```'!,,'```'!,,'```'!,,'```'!,,'```'!,,'```'!,,'=3"


blueBossaRiff :: Beat PitchFactorDiagram
blueBossaRiff = RoseBeat
  [(1,fmap fromMajorScale $ RoseBeat walk)
  ,(1,fmap fromMajorScale . fmap (subtract 1) $ RoseBeat $ drop 1 $ walk)
  ,(1,fmap (addPFD $ invertPFD majorSecond) $ fmap fromMajorScale $ RoseBeat otherWalk)
  ]
  where
    otherWalk =
      [(3,Beat (7 + 1))
      ,(1,Beat (7 + 0))
      ,(1,Beat 6)
      ,(2,Beat 5)
      ,(7,Beat 4)
      ,(2,Beat 3)
      ,(2,Beat 3)
      ,(1,Beat 2)
      ,(2,Beat 5)
      ,(1,Beat 2)
      ,(10,Beat 4)
      ]
    walk =
      [(2,Beat 2)
      ,(3,Beat (7 + 2))
      ,(1,Beat (7 + 1))
      ,(1,Beat (7 + 0))
      ,(2,Beat 6)
      ,(7,Beat 5)
      ,(2,Beat 4)
      ,(4,Beat 3)
      ,(3,Beat (7 + 2))
      ,(9,Beat (7 + 1))
      ]
