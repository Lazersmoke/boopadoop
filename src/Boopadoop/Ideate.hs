module Boopadoop.Ideate where

import Boopadoop
import Data.Tree
import qualified Data.Set as Set
import Debug.Trace

data Motion = MoveInterval PitchFactorDiagram

compose :: Tick -> Beat Wavetable -> Wavetable
compose _ (Beat w) = w
compose time (RoseBeat xs) = sampleFrom (\t -> sampleIn t xs t)
  where
    sampleIn ot ((k,w):xs) t
      | k * curBeatTicks >= t = sample (compose (k * curBeatTicks) w) t
      | otherwise = sampleIn ot xs (t - (k * curBeatTicks))
    sampleIn ot [] t = error ("Sampled past end of `compose` wavetable by " ++ show t ++ " ticks with curBeatTicks=" ++ show curBeatTicks ++ " and RoseBeat is " ++ show xs ++ " with ot=" ++ show ot)
    curSubdivs = sum . fmap fst $ xs
    curBeatTicks = time `quotRoundUp` curSubdivs
    -- Not needed, since `sampleIn` already doesn't sample past the end or otherwise out of bounds.
    --compactified = fmap (\(k,w) -> modulate muting (compactWave (0,k * curBeatTicks)) w) $ xs

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
