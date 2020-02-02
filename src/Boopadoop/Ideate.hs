{-# LANGUAGE DeriveFunctor #-}
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
import Data.List
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

composeTimed :: Num a => Tick -> Timed (Waveform Tick a) -> [a]
composeTimed beatTime (Timed timed) = go 0 timed
  where
    go t b@((k,w):xs) = if t < beatTime * k
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

arpegiate :: Chord -> Beat PitchFactorDiagram
arpegiate c = arpegiateLike [0 .. Set.size (getNotes c) - 1] c

arpegiateLike :: [Int] -> Chord -> Beat PitchFactorDiagram
arpegiateLike ixs c = equalTime . fmap (Beat . (chordPitches c!!)) $ ixs

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

toConcreteKey :: Functor f => Double -> f PitchFactorDiagram -> f Double
toConcreteKey root = fmap (($ root) . intervalOf)

fromTabWithTuning :: [Double] -> [Int] -> [Double]
fromTabWithTuning = zipWith (\t f -> t * (semi ** fromIntegral f))


makeChoice :: String -> [k] -> (k -> a) -> (Int -> a)
makeChoice _ xs f i = f $ xs !! i

data LeadChord = LeadChord {chordTones :: Set.Set PitchFactorDiagram, scaleTones :: Set.Set PitchFactorDiagram}

nonChordTones :: LeadChord -> Set.Set PitchFactorDiagram
nonChordTones lc = scaleTones lc `Set.difference` chordTones lc

data RiffSemaphore = PickedRiff PitchFactorDiagram (Beat PitchFactorDiagram) deriving Show

consecrateSemaphore :: RiffSemaphore -> Beat PitchFactorDiagram
consecrateSemaphore (PickedRiff k bs) = fmap (addPFD k) bs

walkMajorUp :: Int -> [Int] -> Timed PitchFactorDiagram
walkMajorUp p0 ts = withTimings ts . fmap fromMajorScale $ [p0, p0 + 1 ..]

walkMajorDown :: Int -> [Int] -> Timed PitchFactorDiagram
walkMajorDown p0 ts = withTimings ts . fmap fromMajorScale $ [p0, p0 - 1 ..]

withTimings :: [Int] -> [a] -> Timed a
withTimings ts = Timed . zip ts

solFeck :: String -> Timed (Maybe PitchFactorDiagram)
solFeck = Timed . (\(_,_,_,notes) -> notes) . foldl step st0
  where
    debugSolFeck = False
    st0 = (0 :: Int,unison,unison,[])
    step (mode,key,lastNote,notes) symb = (\(a,b,c,d) -> if debugSolFeck then (trace ("solFeck " ++ [symb] ++ " in key " ++ show key ++ "with lastNote " ++ show lastNote ++ " return:" ++ show (Timed d)) (a,b,c,d)) else (a,b,c,d)) $ case mode of
      2 -> case fmap fromDiatonic $ symb `elemIndex` "0123456789ab" of
        Just p -> (0,p,lastNote,notes)
        Nothing -> error $ "Unknown solFeck key signature " ++ [symb]
      1 -> (0,key,lastNote,init notes ++ [(\(k,p) -> (k+(read [symb] :: Int),p)) (last notes)])
      0 -> case getRealDelta symb (aliasToScale key lastNote) of --getRealDelta symb (aliasToScale key lastNote) of
        Just del -> let n' = addPFD key (fromDiatonic $ aliasToScale key lastNote + del) in (0,key,n', notes ++ [(1,Just n')])
        Nothing -> case symb of
          '^' -> (0,key,addPFD octave lastNote,notes)
          'v' -> (0,key,addPFD (invertPFD octave) lastNote,notes)
          '-' -> (0,key,lastNote,init notes ++ [(\(k,p) -> (k+1,p)) (last notes)])
          '=' -> (1,key,lastNote,notes)
          '~' -> (2,key,lastNote,notes)
          ' ' -> (0,key,lastNote,notes ++ [(1,Nothing)])
          'x' -> (0,key,lastNote,notes ++ [(\(_,p) -> (1,p)) (last notes)])
          'i' -> let n' = nextStepUpMajor key (nextStepUpMajor key (nextStepUpMajor key lastNote)) in (0,key,n',notes ++ [(1,Just n')])
          '!' -> let n' = nextStepDownMajor key (nextStepDownMajor key (nextStepDownMajor key lastNote)) in (0,key,n',notes ++ [(1,Just n')])
          '.' -> let n' = nextStepDownMajor key lastNote in (0,key,n',notes ++ [(1,Just n')])
          ',' -> let n' = nextStepDownMajor key (nextStepDownMajor key lastNote) in (0,key,n',notes ++ [(1,Just n')])
          '\'' -> let n' = nextStepUpMajor key lastNote in (0,key,n',notes ++ [(1,Just n')])
          '`' -> let n' = nextStepUpMajor key (nextStepUpMajor key lastNote) in (0,key,n',notes ++ [(1,Just n')])
          x -> error $ "Unknown solFeck symbol '" ++ [x] ++ "'"
      _ -> error $ "Unknown solFeck mode " ++ show mode ++ " with symbol '" ++ [symb] ++ "'"

diaIndex :: Char -> Maybe Int
diaIndex = (`elemIndex` "0123456789ab")

getRealDelta :: Char -> Int -> Maybe Int
getRealDelta symb cur = fmap (\i -> let delta = i - (cur `mod` 12) in ((delta + 5) `mod` 12) - 5) $ diaIndex symb

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

aliasToScale :: PitchFactorDiagram -> PitchFactorDiagram -> Int
aliasToScale key x = round (diagramToSemi @Double (addPFD (invertPFD key) x))

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
