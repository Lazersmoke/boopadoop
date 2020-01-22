{-# LANGUAGE DeriveFunctor #-}
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

toConcreteKey :: Double -> Beat PitchFactorDiagram -> Beat Double
toConcreteKey root = fmap (($ root) . intervalOf)

fromTabWithTuning :: [Double] -> [Int] -> [Double]
fromTabWithTuning = zipWith (\t f -> t * (semi ** fromIntegral f))


makeChoice :: String -> [k] -> (k -> a) -> (Int -> a)
makeChoice _ xs f i = f $ xs !! i

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
