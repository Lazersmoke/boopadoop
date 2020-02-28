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

-- | Rasterize a 'TimeStream' into a stream of samples with the given number of ticks per count in the TimeStream.
timeStreamToValueStream :: Rational -> TimeStream (Waveform Tick a) -> [a]
timeStreamToValueStream ticksPerCount = go (0 :: Tick)
  where
    go :: Tick -> TimeStream (Waveform Tick a) -> [a]
    go !tickNum (TimeStream t x xs) = if fromIntegral tickNum < t * ticksPerCount
      then sample x tickNum : go (tickNum + 1) (TimeStream t x xs)
      else go 0 xs
    go _ EndStream = []

-- | Like 'timeStreamToValueStream', but chunks it by putting each step of the 'TimeStream' in its own list.
timeStreamToValueStreamChunked :: Rational -> TimeStream (Waveform Tick a) -> [[a]]
timeStreamToValueStreamChunked ticksPerCount (TimeStream t x xs) = timeStreamToValueStream ticksPerCount (TimeStream t x EndStream) : timeStreamToValueStreamChunked ticksPerCount xs
timeStreamToValueStreamChunked _ EndStream = []

-- | Arpegiate a chord by playing chord tones in the order specified
-- @
--  'arpegiateLike' [0,2,1,2,1,0] (solFeckChrd "047") ==> 'solFeck' "074740"
-- @
arpegiateLike :: [Int] -> Chord a -> TimeStream a
arpegiateLike ixs c = equalTime . fmap (getVoiceList c!!) $ ixs

-- | Arpegiate like @[0,1,2,..]@
arpegiate :: Chord a -> TimeStream a
arpegiate c = arpegiateLike [0 .. countVoices c - 1] c

-- | Naive implementation of making a stream of volumes that roughly follows the pitch of the input stream
volumeFollowsPitch :: [PitchFactorDiagram] -> [Discrete]
volumeFollowsPitch = go 0.5 mempty
  where
    go v lastPitch (x:xs) = let deltaPitch = ratioBetween lastPitch x in if deltaPitch == 1
      then v : go v x xs
      else if deltaPitch > 1
        then let v' = (\y -> doubleToDiscrete y*(1 - v) + v) . min 1 . logBase 2 $ deltaPitch in v' : go v' x xs
        else let v' = (\y -> (1 - v) + doubleToDiscrete y*v) . min 1 . logBase 2 . recip $ deltaPitch in v' : go v' x xs
    go _ _ [] = []

toConcreteKey :: Functor f => Double -> f (Octaved PitchFactorDiagram) -> f Double
toConcreteKey root = fmap (($ root) . intervalOf)

fromTabWithTuning :: [Double] -> [Int] -> [Double]
fromTabWithTuning = zipWith (\t f -> t * (semi ** fromIntegral f))

makeChoice :: String -> [k] -> (k -> a) -> (Int -> a)
makeChoice _ xs f i = f $ xs !! i

-- | Interpret a string of SolFeck pitch characters as a 'Chord' (harmonically instead of melodically)
solFeckChrd :: String -> Chord TwelveTone
solFeckChrd = chordOf . catMaybes . fmap ttFromSolfeck

-- | Same as 'solFeck' but outputs the 'PitchFactorDiagram's of the notes instead
solFeckPFD :: String -> TimeStream (Maybe (Octaved PitchFactorDiagram))
solFeckPFD = fmap (fmap ttPFD) . solFeck

-- | Interpret a string written in the SolFeck language as a sequence of twelve-toned notes and rests
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

-- | Get the smallest effective chord from the playing context. This is the lead chord if avabailable, otherwise the effective key.
effectiveChord :: PlayingContext a -> Maybe (Chord a)
effectiveChord ctx = case leadChord ctx of
  Just lc -> Just lc
  Nothing -> effectiveKey ctx

-- | Get the last pitch from the playing context history
lastNoteCtx :: PlayingContext a -> Maybe (Octaved a)
lastNoteCtx context = case playingHistory context of
  (TimeStream _ x _) -> Just x
  EndStream -> Nothing

-- | Get the last time interval from the playing context history
lastTimeCtx :: PlayingContext a -> Maybe Rational
lastTimeCtx context = case playingHistory context of
  (TimeStream t _ _) -> Just t
  EndStream -> Nothing
