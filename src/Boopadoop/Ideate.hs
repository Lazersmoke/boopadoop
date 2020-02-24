{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
module Boopadoop.Ideate where

import Boopadoop
import qualified Data.Set as Set
import Control.Applicative
import Data.List
import Data.Ord
import Data.Maybe
import Data.Functor.Identity
import System.Random

import Debug.Trace

data Motion = MoveInterval PitchFactorDiagram

type GlobalMuSyncMagic = Double

globalMuSyncZero :: GlobalMuSyncMagic
globalMuSyncZero = 0

data MuSync a = MuSync {getValue :: !a, getSynchronization :: !GlobalMuSyncMagic} deriving Functor

type ToSync a = GlobalMuSyncMagic -> a

sinMuSync :: Double -> ToSync (Waveform Double (MuSync Double))
sinMuSync f = withSync $ \t phi0 -> MuSync {getValue = sin ((2 * pi * f) * t + phi0),getSynchronization = 2 * pi * f * t + phi0}

withSync :: (t -> GlobalMuSyncMagic -> MuSync a) -> ToSync (Waveform t (MuSync a))
withSync f g = sampleFrom $ \t -> f t g

--withSyncZero :: Waveform (MuSync t) a -> Waveform t a
--withSyncZero = microSync globalMuSyncZero

syncTo :: Waveform (MuSync t) a -> ToSync (Waveform t a)
syncTo w g = sampleFrom $ \t -> sample w (MuSync {getValue = t,getSynchronization = g})

--stdSin :: Double -> ToSync (Waveform Tick (MuSync Discrete))
--stdSin = fmap (fmap discretize) . sinMuSync

{-
composeMuSync :: Tick -> Beat (ToSync (Waveform Tick (MuSync a))) -> ToSync (Waveform Tick (MuSync a))
composeMuSync _ (Beat w) = w
composeMuSync time (RoseBeat bs) = withSync (\t g -> (if t `mod` 1000 == 0 then trace ("t=" ++ show t) else id) $ sampleIn bs t g)
  where
    sampleIn ((k,w):xs) t g
      | k * curBeatTicks >= t = sample (composeMuSync (k * curBeatTicks) w g) t
      | otherwise = sampleIn xs (t - (k * curBeatTicks)) (getSynchronization $ sample (composeMuSync (k * curBeatTicks) w g) (k * curBeatTicks))
    sampleIn [] t g = error ("Sampled past end of `composeMuSync` wavetable by " ++ show t ++ " ticks with curBeatTicks=" ++ show curBeatTicks ++ " and RoseBeat is " ++ show (fmap ($ globalMuSyncZero) $ RoseBeat bs) ++ " with g=" ++ show g)
    curSubdivs = sum . fmap fst $ bs
    curBeatTicks = time `quotRoundUp` curSubdivs

compose :: Tick -> Beat (Waveform Tick a) -> Waveform Tick a
compose _ (Beat w) = w
compose time (RoseBeat bs) = sampleFrom (\t -> sampleIn t bs t)
  where
    sampleIn ot ((k,w):xs) t
      | k * curBeatTicks >= t = sample (compose (k * curBeatTicks) w) t
      | otherwise = sampleIn ot xs (t - (k * curBeatTicks))
    sampleIn ot [] t = error ("Sampled past end of `compose` wavetable by " ++ show t ++ " ticks with curBeatTicks=" ++ show curBeatTicks ++ " and RoseBeat is " ++ show bs ++ " with ot=" ++ show ot)
    curSubdivs = sum . fmap fst $ bs
    curBeatTicks = time `quotRoundUp` curSubdivs
    -- Not needed, since `sampleIn` already doesn't sample past the end or otherwise out of bounds.
    --compactified = fmap (\(k,w) -> modulate muting (compactWave (0,k * curBeatTicks)) w) $ xs
-}

timeStreamToValueStream :: Rational -> TimeStream (Waveform Tick a) -> [a]
timeStreamToValueStream ticksPerCount = go (0 :: Tick)
  where
    go :: Tick -> TimeStream (Waveform Tick a) -> [a]
    go !tickNum (TimeStream t x xs) = if fromIntegral tickNum < t * ticksPerCount
      then sample x tickNum : go (tickNum + 1) (TimeStream t x xs)
      else go 0 xs
    go _ EndStream = []

timeStreamToValueStreamChunked :: Rational -> TimeStream (Waveform Tick a) -> [[a]]
timeStreamToValueStreamChunked ticksPerCount (TimeStream t x xs) = timeStreamToValueStream ticksPerCount (TimeStream t x EndStream) : timeStreamToValueStreamChunked ticksPerCount xs
timeStreamToValueStreamChunked _ EndStream = []

composeTimed :: Num a => Tick -> Timed (Waveform Tick a) -> [a]
composeTimed beatTime (Timed timed) = go 0 timed
  where
    go !t b@((k,w):xs) = if t < beatTime * k
      then sample w t : go (t + 1) b
      else go 0 xs
    go _ [] = []

composeSlurred :: Tick -> [(Tick,Wavetable)] -> Beat (Tick,Wavetable) -> Waveform Tick (Discrete,[(Tick,Wavetable)])
composeSlurred _ slurs (Beat expw) = (sampleFrom $ \t -> addSlurs t (expw:slurs))
composeSlurred time slurs (RoseBeat bs) = sampleFrom $ \t -> sampleIn slurs bs t
  where
    sampleIn :: [(Tick,Wavetable)] -> [(Int,Beat (Tick,Wavetable))] -> Tick -> (Discrete,[(Tick,Wavetable)])
    sampleIn sls ((k,b):xs) t
      | t <= k * curBeatTicks = sample (composeSlurred (k * curBeatTicks) sls b) t
      | otherwise = let (_,sls') = sample (composeSlurred (k * curBeatTicks) sls b) (k * curBeatTicks) in sampleIn sls' xs (t - k * curBeatTicks)
    sampleIn sls [] t = error ("Sampled past end of `composeSlurred` wavetable by " ++ show t ++ " ticks with curBeatTicks=" ++ show curBeatTicks ++ " and sls=" ++ show sls)
    curSubdivs = sum . fmap fst $ bs
    curBeatTicks = time `quotRoundUp` curSubdivs

streamSlurs :: Tick -> Beat (Tick,Wavetable) -> [Discrete]
streamSlurs _time (Beat expw) = slurOut expw
streamSlurs time (RoseBeat bs) = snd . foldl (\(k',xs) (k,w) -> (k+k',holdAndZip (k' * curBeatTicks) (+) xs (streamSlurs (k * curBeatTicks) w))) (0,repeat 0) $ bs
  where
    curSubdivs = sum . fmap fst $ bs
    curBeatTicks = time `quotRoundUp` curSubdivs

envelopeSlurs :: Tick -> Beat (Envelope Tick Discrete,Wavetable) -> [Discrete]
envelopeSlurs time (Beat expw) = envSlur time expw
envelopeSlurs time (RoseBeat bs) = snd . foldl (\(k',xs) (k,w) -> (k+k',holdAndZip (k' * curBeatTicks) (+) xs (envelopeSlurs (k * curBeatTicks) w))) (0,repeat 0) $ bs
  where
    curSubdivs = sum . fmap fst $ bs
    curBeatTicks = time `quotRoundUp` curSubdivs

envSlur :: Tick -> (Envelope Tick Discrete,Wavetable) -> [Discrete]
envSlur noteDuration (e@(Envelope _ _ _ _ _ rel),w) = go 0
  where
    env = susEnvelope e noteDuration
    w' = amplitudeModulate env w
    go :: Tick -> [Discrete]
    go tNow = if tNow < rel + noteDuration
      then sample w' tNow : go (tNow + 1)
      else []

slurOut :: (Tick,Wavetable) -> [Discrete]
slurOut (tExp,w) = go 0
  where
    go :: Tick -> [Discrete]
    go tNow = if tNow < tExp
      then sample w tNow : go (tNow + 1)
      else []

holdAndZip :: Int -> (a -> b -> a) -> [a] -> [b] -> [a]
--holdAndZip k as = zipWith (+) (replicate k 0 ++ as)
holdAndZip !k f (a:as) bs = if k > 0
  then a : holdAndZip (k - 1) f as bs
  else zipWithNoTrunc f as bs
holdAndZip _ _ [] _ = error "holdAndZip on empty list"

zipWithNoTrunc :: (a -> b -> a) -> [a] -> [b] -> [a]
zipWithNoTrunc f (a:as) (b:bs) = f a b : zipWithNoTrunc f as bs
zipWithNoTrunc _ as [] = as
zipWithNoTrunc _ [] _ = error "zipWithNoTrunc out of as"

freshSlurs :: Tick -> Beat (Tick,Wavetable) -> Wavetable
freshSlurs time b = fmap fst $ composeSlurred time [] b

composeWithSlurs :: Tick -> Tick -> Beat Wavetable -> [Discrete]
composeWithSlurs slurTimeout time = streamSlurs time . fmap (slurTimeout,)

composeWithEnvelope :: Envelope Tick Discrete -> Tick -> Beat Wavetable -> [Discrete]
composeWithEnvelope env time = envelopeSlurs time . fmap (env,)


-- At a given tick, mix the given slurs. Remove any expired slurs.
addSlurs :: Tick -> [(Tick,Wavetable)] -> (Discrete,[(Tick,Wavetable)])
addSlurs tNow ((st,sw):ss) = let {(x,ss') = addSlurs tNow ss ; st' = st - tNow }in if st' < 0
  then (x,ss') --yeet st
  else (sample sw tNow + x,(st',sw):ss') -- keep st
addSlurs _ [] = (0,[])

arpegiate :: Chord a -> TimeStream a
arpegiate c = arpegiateLike [0 .. countVoices c - 1] c

arpegiateLike :: [Int] -> Chord a -> TimeStream a
arpegiateLike ixs c = equalTime . fmap (getVoiceList c!!) $ ixs

volumeFollowsPitch :: [PitchFactorDiagram] -> [Discrete]
volumeFollowsPitch = go 0.5 mempty
  where
    go v lastPitch (x:xs) = let deltaPitch = ratioBetween lastPitch x in if deltaPitch == 1
      then v : go v x xs
      else if deltaPitch > 1
        then let v' = (\y -> doubleToDiscrete y*(1 - v) + v) . min 1 . logBase 2 $ deltaPitch in v' : go v' x xs
        else let v' = (\y -> (1 - v) + doubleToDiscrete y*v) . min 1 . logBase 2 . recip $ deltaPitch in v' : go v' x xs
    go _ _ [] = []

-- For constant pitch: keep same volume
-- For ascending pitch: increase volume, slope goes like slope of pitch
--

-- Music is made of beats, pitches, timbre
--data Ideation = Ideate {
  --deltaMusic :: Music -> Music
  --}


--buildMedley :: Ideation a => Tree a

{-
sampleMelody :: Beat PitchFactorDiagram
sampleMelody = RoseBeat
  [Beat unison
  ,Beat majorThird
  ,Beat perfectFifth
  ,Beat perfectFifth
  ]
  where
    fastBit = RoseBeat [Beat perfectFifth]
-}

toConcreteKey :: Functor f => Double -> f (Octaved PitchFactorDiagram) -> f Double
toConcreteKey root = fmap (($ root) . intervalOf)

fromTabWithTuning :: [Double] -> [Int] -> [Double]
fromTabWithTuning = zipWith (\t f -> t * (semi ** fromIntegral f))


makeChoice :: String -> [k] -> (k -> a) -> (Int -> a)
makeChoice _ xs f i = f $ xs !! i

data LeadChord a = LeadChord {chordTones :: Set.Set a, scaleTones :: Set.Set a}

nonChordTones :: Ord a => LeadChord a -> Set.Set a
nonChordTones lc = scaleTones lc `Set.difference` chordTones lc

{-
data RiffSemaphore = PickedRiff PitchFactorDiagram (Beat PitchFactorDiagram) deriving Show

consecrateSemaphore :: RiffSemaphore -> Beat PitchFactorDiagram
consecrateSemaphore (PickedRiff k bs) = fmap (addPFD k) bs

walkMajorUp :: Int -> [Int] -> Timed PitchFactorDiagram
walkMajorUp p0 ts = withTimings ts . fmap fromMajorScale $ [p0, p0 + 1 ..]

walkMajorDown :: Int -> [Int] -> Timed PitchFactorDiagram
walkMajorDown p0 ts = withTimings ts . fmap fromMajorScale $ [p0, p0 - 1 ..]
-}

withTimings :: [Int] -> [a] -> Timed a
withTimings ts = Timed . zip ts

solFeckChrd :: String -> Chord TwelveTone
solFeckChrd = chordOf . catMaybes . fmap ttFromSolfeck

solFeckPFD :: String -> TimeStream (Maybe (Octaved PitchFactorDiagram))
solFeckPFD = fmap (fmap ttPFD) . solFeck

solFeck :: String -> TimeStream (Maybe (Octaved TwelveTone))
solFeck = reverseTimeStream . (\(_,_,_,notes) -> notes) . foldl step st0
  where
    extendPreviousNoteBy n (TimeStream t x xs) = TimeStream (t + n) x xs
    extendPreviousNoteBy _ EndStream = EndStream
    duplicatePreviousNote (TimeStream t x xs) = TimeStream 1 x $ TimeStream t x xs
    duplicatePreviousNote EndStream = EndStream
    debugSolFeck = False
    st0 = (0 :: Int,mempty,mempty,EndStream)
    step (mode,key,lastNote,notes) symb = (\(a,b,c,d) -> if debugSolFeck then (trace ("solFeck " ++ [symb] ++ " in key " ++ show key ++ " with lastNote " ++ show lastNote ++ " return:" ++ show d) (a,b,c,d)) else (a,b,c,d)) $ case mode of
      2 -> case ttFromSolfeck symb of
        Just p -> (0,p,lastNote,notes)
        Nothing -> error $ "Unknown solFeck key signature " ++ [symb]
      1 -> (0,key,lastNote,extendPreviousNoteBy (fromIntegral (read [symb] :: Int)) notes)
      0 -> case ttFromSolfeck symb of
        Just ttPitch' -> let n' = closestInstanceOfPitch ttPitch' lastNote in (0,key,n', TimeStream 1 (Just n') notes)
        Nothing -> case symb of
          '^' -> (0,key,shiftOctave 1 lastNote,notes)
          'v' -> (0,key,shiftOctave (-1) lastNote,notes)
          '-' -> (0,key,lastNote,extendPreviousNoteBy 1 notes)
          '=' -> (1,key,lastNote,notes)
          '~' -> (2,key,lastNote,notes)
          ' ' -> (0,key,lastNote,TimeStream 1 Nothing notes)
          'x' -> (0,key,lastNote,duplicatePreviousNote notes)
          'i' -> let n' = ttScaleUp key (ttScaleUp key (ttScaleUp key lastNote)) in (0,key,n',TimeStream 1 (Just n') notes)
          '!' -> let n' = ttScaleDown key (ttScaleDown key (ttScaleDown key lastNote)) in (0,key,n',TimeStream 1 (Just n') notes)
          '.' -> let n' = ttScaleDown key lastNote in (0,key,n',TimeStream 1 (Just n') notes)
          ',' -> let n' = ttScaleDown key (ttScaleDown key lastNote) in (0,key,n',TimeStream 1 (Just n') notes)
          '\'' -> let n' = ttScaleUp key lastNote in (0,key,n',TimeStream 1 (Just n') notes)
          '`' -> let n' = ttScaleUp key (ttScaleUp key lastNote) in (0,key,n',TimeStream 1 (Just n') notes)
          x -> error $ "Unknown solFeck symbol '" ++ [x] ++ "'"
      _ -> error $ "Unknown solFeck mode " ++ show mode ++ " with symbol '" ++ [symb] ++ "'"

diaIndex :: Char -> Maybe Int
diaIndex = (`elemIndex` "0123456789ab")

closestInstanceOfPitch :: TwelveTone -> Octaved TwelveTone -> Octaved TwelveTone
closestInstanceOfPitch p' cur = let delta = ttRelPitch p' cur in if delta >= 6
  then inOctave (-1) (twelveTone delta) <> cur -- Go down; e.g. 4 from 7 to 4 with delta = 3
  else inOctave 0 (twelveTone delta) <> cur -- Go up; e.g. 5 from 2 to 5 with delta = 9

getRealDelta :: Char -> Int -> Maybe Int
getRealDelta symb cur = fmap (\i -> let delta = i - (cur `mod` 12) in ((delta + 5) `mod` 12) - 5) $ diaIndex symb

{-
nextStepDownMajor :: PitchFactorDiagram -> PitchFactorDiagram -> PitchFactorDiagram
nextStepDownMajor key x = case foldl (\s p -> case s of {Nothing -> if p < nx then Just p else Nothing; Just s' -> Just s'}) Nothing (fmap (fromMajorScale) [8,7..(-1)]) of
  Nothing -> error $ "nextStepDownMajor(" ++ show x ++ ")"
  Just x' -> addPFD (addPFD (invertPFD nx) x') x
  where
    nx = normalizePFD (addPFD (invertPFD key) x)

nextStepUpMajor :: PitchFactorDiagram -> PitchFactorDiagram -> PitchFactorDiagram
nextStepUpMajor key x = case foldl (\s p -> case s of {Nothing -> if p > nx then Just p else Nothing; Just s' -> Just s'}) Nothing (fmap (fromMajorScale) [0,1..9]) of
  Nothing -> error $ "nextStepUpMajor(" ++ show x ++ ")"
  Just x' -> addPFD (addPFD (invertPFD nx) x') x
  where
    nx = normalizePFD (addPFD (invertPFD key) x)
-}

--aliasToScale :: PitchFactorDiagram -> PitchFactorDiagram -> Int
--aliasToScale key x = round (diagramToSemi @Double (addPFD (invertPFD key) x))

nextNoteFromScale :: (Ord a,Floating a) => [PitchFactorDiagram] -> a -> PitchFactorDiagram
nextNoteFromScale pitches targetPitch = fst . minimumBy (comparing snd) $ zip pitches (sqr targetPitch $ fmap diagramToRatio pitches)
  where
    sqr y0 = fmap (\y -> (y - y0)*(y - y0))

unfoldTimeStream :: (b -> (Rational,a,b)) -> b -> TimeStream a
unfoldTimeStream f !z = let (r,a,b) = f z in TimeStream r a (unfoldTimeStream f b)

unfoldConsumingStream :: [a] -> (a -> (Rational,b)) -> TimeStream b
unfoldConsumingStream (x:xs) f = let (r,b) = f x in TimeStream r b (unfoldConsumingStream xs f)
unfoldConsumingStream [] _ = EndStream

-- | Compose a TimeStream as a step function of samples
stepCompose :: Rational -> TimeStream a -> [a]
stepCompose beatsPerSample (TimeStream t x xs) = x : stepCompose beatsPerSample (if t > 0 then TimeStream (t - beatsPerSample) x xs else xs)
stepCompose _ EndStream = []

-- | Compose a TimeStream while ensuring smoothness of the output by following the time stream value with a diffeq instead of using it as a step function
followValue :: Double -> Double -> [Double] -> [Double]
followValue _ _ [] = error "followValue: no initial value"
followValue sampleRate lambda (x0:toFollow) = fmap runIdentity $ rkSolveStepSize (1/sampleRate) rkf (Identity x0) toFollow
  where
    rkf val (Identity x) = Identity $ lambda * (val - x)

followValue' :: Double -> Double -> [Maybe Double] -> [Double]
followValue' sampleRate lambda (Just x0:toFollow) = fmap runIdentity $ rkSolveStepSize (1/sampleRate) rkf (Identity x0) toFollow
  where
    rkf (Just val) (Identity x) = Identity $ lambda * (val - x)
    rkf Nothing _ = Identity 0
followValue' _ _ _ = error "followValue': no initial value"

meshWavestreams :: [Wavetable] -> [Discrete]
meshWavestreams = go 0
  where
    go !t (w:ws) = sample w t : go (t + 1) ws
    go _ _ = []

data PlayingContext a = PlayingContext
  {leadChord :: Maybe (Chord a)
  ,effectiveKey :: Maybe (Chord a)
  ,effectiveRoot :: Maybe a
  ,allowNCT :: Bool
  ,tensionPhase :: Maybe Double -- ^ In [0,1] phase of tension
  ,pitchTarget :: Maybe (Octaved a)
  ,playingHistory :: TimeStream (Octaved a) -- ^ Looks backward into history
  ,rangeLowerBound :: Maybe (Octaved a)
  ,rangeUpperBound :: Maybe (Octaved a)
  ,randomGen :: !StdGen
  }

defaultPlayingContext :: StdGen -> PlayingContext PitchFactorDiagram
defaultPlayingContext rg = PlayingContext
  {leadChord = Just $ chordOf [unison,majorThird,perfectFifth,majorSeventh]
  ,effectiveKey = Just $ chordOf majorScale
  ,effectiveRoot = Nothing
  ,allowNCT = False
  ,tensionPhase = Nothing
  ,pitchTarget = Nothing
  ,playingHistory = TimeStream 1 (inOctave 0 unison) (TimeStream 1 (inOctave (-1) unison) EndStream)
  ,rangeLowerBound = Just $ inOctave (-1) unison
  ,rangeUpperBound = Just $ inOctave 1 unison
  ,randomGen = rg
  }

ttPC :: StdGen -> PlayingContext TwelveTone
ttPC rg = PlayingContext
  {leadChord = Just $ chordOf $ map twelveTone [0,4,7,11]
  ,effectiveKey = Just $ chordOf [twelveTone 0]
  ,effectiveRoot = Just $ twelveTone 0
  ,allowNCT = False
  ,tensionPhase = Nothing
  ,pitchTarget = Nothing
  ,playingHistory = TimeStream 1 mempty EndStream
  ,rangeLowerBound = Just . inOctave (-1) $ mempty
  ,rangeUpperBound = Just . inOctave 2 $ mempty
  ,randomGen = rg
  }

data WithExplanation a = WithExplanation {getExplanation :: String, getExplained :: a}

instance Show a => Show (WithExplanation a) where
  show (WithExplanation why a) = "Use " ++ show a ++ " because " ++ why

followLeads :: Show (Octaved a) => [CompositionRule a Rational] -> [CompositionRule a (Octaved a)] -> PlayingContext a -> TimeStream (Chord a) -> TimeStream (WithExplanation (PlayingContext a,Octaved a))
followLeads trs prs pc0 (TimeStream t lc lcs) = let (WithExplanation _why (pc',_pfdLast),ts) = branchAfterTimeStream (t * 15) (composeAlong (ruledPlaying trs prs) (pc0 {leadChord = Just lc})) (trace "recursing" $ followLeads trs prs pc' lcs) in ts
followLeads _ _ _ EndStream = EndStream

composeAlong :: Show (Octaved a) => (PlayingContext a -> (WithExplanation (Octaved a),WithExplanation Rational,PlayingContext a)) -> PlayingContext a -> TimeStream (WithExplanation (PlayingContext a,Octaved a))
composeAlong f pc = TimeStream t (WithExplanation ("Pitch " ++ show x ++ " because " ++ whyX ++ "; Timing " ++ show t ++ " because " ++ whyT) (pc',x)) (composeAlong f pc')
  where
    (WithExplanation whyX !x,WithExplanation whyT !t,!pc') = f pc

ruledPlaying :: [CompositionRule a Rational] -> [CompositionRule a (Octaved a)] -> PlayingContext a -> (WithExplanation (Octaved a),WithExplanation Rational,PlayingContext a)
ruledPlaying trules rules ctx = (WithExplanation whatHappened nextPFD,WithExplanation whatHappenedT nextTiming,ctx {playingHistory = TimeStream nextTiming nextPFD $ playingHistory ctx,randomGen = randomGen''})
  where
    possibleNextTimings = catMaybes $ (\r -> triggerApply r ctx) <$> trules
    (WithExplanation whatHappenedT nextTiming) = if null possibleNextTimings then error "Writer's block!" else possibleNextTimings !! nextTimingIndex
    (nextTimingIndex,randomGen'') = randomR (0,length possibleNextTimings - 1) randomGen'

    possibleNextPFDs = catMaybes $ (\r -> triggerApply r ctx) <$> rules
    (WithExplanation whatHappened nextPFD) = if null possibleNextPFDs then error "Writer's block! (Pitches)" else possibleNextPFDs !! nextPFDIndex
    (nextPFDIndex,randomGen') = randomR (0,length possibleNextPFDs - 1) (randomGen ctx)

selectRule :: PlayingContext a -> [CompositionRule a b] -> (WithExplanation b,StdGen)
selectRule ctx rules = let {goodRules = catMaybes $ (\r -> triggerApply r ctx) <$> rules; (i,r') = randomR (0,length goodRules - 1) (randomGen ctx)} in (goodRules !! i,r')

ruledSeededPlaying :: Show (Octaved a) => TimeStream () -> [CompositionRule a (TimeStream ())] -> [CompositionRule a (Octaved a)] -> PlayingContext a -> TimeStream (WithExplanation (PlayingContext a,Octaved a))
ruledSeededPlaying rhythmSeed rhythmRules pitchRules pc0 = let (WithExplanation _whyLastPitch (pc',_pitchLast),ts) = branchAfterEndStream (addPhrasingExpl $ go pc0 {randomGen = r'} phrase) (ruledSeededPlaying EndStream rhythmRules pitchRules pc') in ts
  where
    go !pc (TimeStream t () xs) = let (WithExplanation whyPitch p,r'') = selectRule pc pitchRules in let pcAfter = pc {playingHistory = TimeStream t p $ playingHistory pc, randomGen = r''} in TimeStream t (WithExplanation ("Pitch " ++ show p ++ " because " ++ whyPitch) (pcAfter,p)) (go pcAfter xs)
    go !pc EndStream = EndStream
    addPhrasingExpl = fmap (\(WithExplanation exp x) -> WithExplanation (exp ++ "\n  with phrasing: " ++ whyPhrase ++ "\n") x)
    (WithExplanation whyPhrase phrase,r') = case rhythmSeed of
      EndStream -> selectRule pc0 rhythmRules
      ts -> (WithExplanation "Seeded" ts,randomGen pc0)

stepChangeContext :: PlayingContext a -> TimeStream (PlayingContext a -> PlayingContext a) -> (PlayingContext a -> TimeStream (WithExplanation (PlayingContext a,b))) -> TimeStream (WithExplanation (PlayingContext a,b))
stepChangeContext !pc0 (TimeStream t change changes) f = let (WithExplanation _whyLastPitch (pc',_pitchLast), ts) = branchAfterTimeStream t (f (change pc0)) (stepChangeContext pc' changes f) in ts

newtype CompositionRule b a = CompositionRule {triggerApply :: PlayingContext b -> Maybe (WithExplanation a)}

--ttRuleToPFDRule :: CompositionRule TwelveTone -> CompositionRule PitchFactorDiagram
--ttRuleToPFDRule =

playRoot :: CompositionRule TwelveTone (Octaved TwelveTone)
playRoot = CompositionRule $ \ctx -> effectiveRoot ctx >>= \r -> lastNoteCtx ctx >>= \l -> Just (WithExplanation "Play Root" $ closestInstanceOfPitch r l)

skipStep :: CompositionRule TwelveTone (Octaved TwelveTone)
skipStep = CompositionRule $ \ctx -> effectiveRoot ctx >>= \r -> lastNoteCtx ctx >>= \l -> lastInterval (playingHistory ctx) >>= \(x'',x') -> let del = ttInterval x'' x' in if del > 2
  then pure $ WithExplanation "skipped up, so step down now" (ttScaleUp r l)
  else if del < (-2)
    then pure $ WithExplanation "skipped down, so step up now" (ttScaleDown r l)
    else Nothing

arpLC :: Bool -> CompositionRule TwelveTone (Octaved TwelveTone)
arpLC upwards = CompositionRule $ \ctx -> leadChord ctx >>= \lc -> lastNoteCtx ctx >>= \l -> if not (Set.member (getPitchClass l) $ getVoices lc) then Nothing else pure $ if upwards
  then WithExplanation ("arpeggiate upward on lead chord (" ++ show lc ++ ")") (chordCeil l lc)
  else WithExplanation ("arpeggiate downward on lead chord (" ++ show lc ++ ")") (chordFloor l lc)

leadingTone :: CompositionRule TwelveTone (Octaved TwelveTone)
leadingTone = CompositionRule $ \ctx -> effectiveRoot ctx >>= \r -> lastNoteCtx ctx >>= \l -> if ttRelPitch r l == 11
  then pure $ WithExplanation "Leading tone!" (ttScaleUp r l)
  else Nothing

stepToTarget :: (Show (Octaved a),Ord a) => CompositionRule a (Octaved a)
stepToTarget = CompositionRule $ \ctx -> pitchTarget ctx >>= \pt -> lastNoteCtx ctx >>= \l -> effectiveKey ctx >>= \ec -> pure $ if l > pt
  then WithExplanation ("Step down towards target (" ++ show pt ++ ")") (chordFloor l ec)
  else WithExplanation ("Step up towards target (" ++ show pt ++ ")") (chordCeil l ec)

keepRangeBounds :: Ord a => CompositionRule a (Octaved a)
keepRangeBounds = CompositionRule $ \ctx -> do
  lb <- rangeLowerBound ctx
  ub <- rangeUpperBound ctx
  l <- lastNoteCtx ctx
  if l > ub
    then ((WithExplanation "Soft walk toward upper bound") . chordFloor l <$> effectiveKey ctx) <|> Just (WithExplanation "Hard clamp to upper bound" ub)
    else if l < lb
      then ((WithExplanation "Soft walk toward lower bound") . chordCeil l <$> effectiveKey ctx) <|> Just (WithExplanation "Hard clamp to lower bound" lb)
      else Nothing

stepToCenter :: (Ord a,Monoid (Octaved a),Show (Octaved a)) => CompositionRule a (Octaved a)
stepToCenter = CompositionRule $ \ctx -> lastNoteCtx ctx >>= \l -> effectiveKey ctx >>= \ec -> if l == mempty then Nothing else pure $ if l > mempty
  then WithExplanation "Step down towards center" (chordFloor l ec)
  else WithExplanation "Step up towards center" (chordCeil l ec)

continueStepping :: CompositionRule TwelveTone (Octaved TwelveTone)
continueStepping = CompositionRule $ \ctx -> effectiveKey ctx >>= \k -> lastNoteCtx ctx >>= \l -> lastInterval (playingHistory ctx) >>= \(x'',x') -> case ttInterval x'' x' of
  1 -> pure $ WithExplanation "Continue stepping upward" (chordCeil l k)
  2 -> pure $ WithExplanation "Continue stepping upward" (chordCeil l k)
  -1 -> pure $ WithExplanation "Continue stepping downward" (chordFloor l k)
  -2 -> pure $ WithExplanation "Continue stepping downward" (chordFloor l k)
  _ -> Nothing

diatonicResolutionDownToRoot :: CompositionRule TwelveTone (Octaved TwelveTone)
diatonicResolutionDownToRoot = CompositionRule $ \ctx -> effectiveRoot ctx >>= \r -> lastNoteCtx ctx >>= \l -> if ttRelPitch r l `elem` [1,2]
  then pure $ WithExplanation "Down to root!" (ttScaleDown r l)
  else Nothing

hackyEightToSevenResolution :: CompositionRule TwelveTone (Octaved TwelveTone)
hackyEightToSevenResolution = CompositionRule $ \ctx -> effectiveRoot ctx >>= \r -> lastNoteCtx ctx >>= \l -> if ttRelPitch r l == 8
  then pure $ WithExplanation "Down to fifth!" (ttScaleDown r l)
  else Nothing 

{-
allNoteRules :: [CompositionRule TwelveTone]
allNoteRules =
  [skipStep
  ,keepRangeBounds
  ,stepToCenter
  ,leadingTone
  ,diatonicResolutionDownToRoot
  ,hackyEightToSevenResolution
  ,stepToTarget
  ,arpLC True
  ,arpLC False
  ,continueStepping
  ]
-}

repeatLastTiming :: CompositionRule a Rational
repeatLastTiming = CompositionRule $ (fmap . fmap) (WithExplanation "Repeat last timing") lastTimeCtx

unitTiming :: CompositionRule a Rational
unitTiming = CompositionRule $ const (Just (WithExplanation "Unit timing" 1))

twiceAsFast :: CompositionRule a Rational
twiceAsFast = CompositionRule $ \ctx -> lastTimeCtx ctx >>= \lt -> Just (WithExplanation "Twice as fast!" (lt / 2))

halfSpeed :: CompositionRule a Rational
halfSpeed = CompositionRule $ \ctx -> lastTimeCtx ctx >>= \lt -> Just (WithExplanation "Twice as fast!" (lt / 2))

boundSpeedAbove :: Rational -> CompositionRule a Rational
boundSpeedAbove tmin = CompositionRule $ \ctx -> lastTimeCtx ctx >>= \lt -> if lt < tmin then Just (WithExplanation "Hard lower bound" tmin) else Nothing

boundSpeedBelow :: Rational -> CompositionRule a Rational
boundSpeedBelow tmax = CompositionRule $ \ctx -> lastTimeCtx ctx >>= \lt -> if lt > tmax then Just (WithExplanation "Hard upper bound" tmax) else Nothing

allTimingRules :: [CompositionRule a Rational]
allTimingRules =
  [repeatLastTiming
  ,halfSpeed
  ,twiceAsFast
  ,unitTiming
  ,boundSpeedAbove (1/32)
  ,boundSpeedBelow 2
  ]

wholeNote :: CompositionRule a (TimeStream ())
wholeNote = CompositionRule $ \ctx -> Just (WithExplanation "Whole note" (TimeStream 1 () EndStream))

halfNote :: CompositionRule a (TimeStream ())
halfNote = CompositionRule $ \ctx -> Just (WithExplanation "Half note" (TimeStream 0.5 () EndStream))

quarterNotes :: CompositionRule a (TimeStream ())
quarterNotes = CompositionRule $ \ctx -> Just (WithExplanation "Quarter note" (TimeStream 0.25 () EndStream))

eightNotes :: CompositionRule a (TimeStream ())
eightNotes = CompositionRule $ \ctx -> Just (WithExplanation "Eights note" (TimeStream 0.125 () (TimeStream 0.125 () EndStream)))

sixteenths :: CompositionRule a (TimeStream ())
sixteenths = CompositionRule $ \ctx -> Just (WithExplanation "Sixteenths note" (TimeStream 0.0625 () (TimeStream 0.0625 () (TimeStream 0.0625 () (TimeStream 0.0625 () EndStream)))))

lastInterval :: TimeStream a -> Maybe (a,a)
lastInterval (TimeStream _ x (TimeStream _ y _)) = Just (y,x)
lastInterval _ = Nothing

--absDiffPFD :: PitchFactorDiagram -> PitchFactorDiagram -> PitchFactorDiagram
--absDiffPFD x y = makePFDGoUp $ addPFD x (invertPFD y)

--classifyLI :: PlayingContext PitchFactorDiagram -> IntervalClassification
--classifyLI = classifyPFD . lastInterval . playingHistory

effectiveChord :: PlayingContext a -> Maybe (Chord a)
effectiveChord ctx = case leadChord ctx of
  Just lc -> Just lc
  Nothing -> case effectiveKey ctx of
    Just ek -> Just ek
    Nothing -> Nothing

lastNoteCtx :: PlayingContext a -> Maybe (Octaved a)
lastNoteCtx context = case playingHistory context of
  (TimeStream _ x _) -> Just x
  EndStream -> Nothing

lastTimeCtx :: PlayingContext a -> Maybe Rational
lastTimeCtx context = case playingHistory context of
  (TimeStream t _ _) -> Just t
  EndStream -> Nothing

--stuff' :: Finite 3 -> Double
--stuff' = makeChoice "num" (ListCons 1 (ListCons 2 (ListCons (3 :: Double) ListNil))) $ \t -> 2 * t
-- Variations monad
-- stuff = do
--   a <- Variations "num" [1,2,3]
--   pure $ 2 * a
--
--
--  >>> variate stuff "num" 0
--  2
{-
stuff :: Variations ('ListCons ('S ('S ('S 'Z))) 'ListNil) ('ListCons Int 'ListNil) Int
stuff = RequireChoice (ListCons 1 $ ListCons 2 $ ListCons 3 $ ListNil) $ \t -> VariationsPure $ 2 * t

variate :: Variations ('ListCons k ks) ('ListCons t ts) a -> SFin k -> Variations ks ts a
variate (RequireChoice (ListCons x _) vf) SO = vf x
variate (RequireChoice (ListCons _ xs) vf) (SS n) = variate (RequireChoice xs vf) n
variate (RequireChoice ListNil _) _ = error "Unreachable because the index is uninhabited"

unVariate :: Variations 'ListNil 'ListNil a -> a
unVariate (VariationsPure a) = a

data Fin = S Fin | Z

data SFin (k :: Fin) where
  SO :: SFin ('S 'Z)
  SS :: SFin n -> SFin ('S n)

instance Num (SFin k) where
  fromInteger k = if k > 1
    then SS (fromInteger (k - 1))
    else SO

data Variations (ks :: List n Fin) (ts :: List n *) (a :: *) where
  VariationsPure :: a -> Variations 'ListNil 'ListNil a
  RequireChoice :: List len t -> (t -> Variations ks ts a) -> Variations ('ListCons len ks) ('ListCons t ts) a

instance Functor (Variations ks ts) where
  fmap f (VariationsPure x) = VariationsPure $ f x
  fmap f (RequireChoice cs v) = RequireChoice cs $ fmap (fmap f) v

--instance Applicative (Variations ts) where
  --pure = VariationsPure
  --(<*>) :: Variations ts (a -> b) -> Variations ts a -> Variations ts b
  --VariationsPure f <*> vx = fmap f vx
  --RequireChoice f <*> vx = fmap f vx

--instance Monad (Variations ts) where
-}
