-- | Some common musical intervals written as @'PitchFactorDiagram'@s
module Boopadoop.Interval where

import Boopadoop.Diagram

fromMajorScale :: Int -> PitchFactorDiagram
fromMajorScale = fromPitchList [unison,majorSecond,majorThird,perfectFourth,perfectFifth,majorSixth,majorSeventh]

fromDiatonic :: Int -> PitchFactorDiagram
fromDiatonic = fromPitchList [unison,minorSecond,majorSecond,minorThird,majorThird,perfectFourth,tritone,perfectFifth,minorSixth,majorSixth,minorSeventh,majorSeventh]

fromPitchList :: [PitchFactorDiagram] -> Int -> PitchFactorDiagram
fromPitchList ps k = addPFD (scalePFD (fromIntegral o) octave) (ps!! i)
  where
    (o,i) = k `divMod` length ps

-- | The non interval, ratio is 1. Identity of `addPFD`.
unison :: PitchFactorDiagram
unison = Factors []

-- | Interval of one octave, ratio is 2.
octave :: PitchFactorDiagram
octave = Factors [1]

-- | Interval of a perfect fourth 4:3
perfectFourth :: PitchFactorDiagram
perfectFourth = normalizePFD . invertPFD $ perfectFifth

-- | Interval of a perfect fifth 3:2
perfectFifth :: PitchFactorDiagram
perfectFifth = normalizePFD $ Factors [0,1]

-- | Interval of a major third 5:4
majorThird :: PitchFactorDiagram
majorThird = normalizePFD $ Factors [0,0,1]

-- | Interval of a minor third 6:5
minorThird :: PitchFactorDiagram
minorThird = normalizePFD $ addPFD perfectFifth (invertPFD majorThird)

-- | Interval of a minor sixth 8:5
minorSixth :: PitchFactorDiagram
minorSixth = normalizePFD . invertPFD $ majorThird

-- | Interval of a major sixth 5:3
majorSixth :: PitchFactorDiagram
majorSixth = normalizePFD . invertPFD $ minorThird

-- | Interval 7:4
harmonicSeven :: PitchFactorDiagram
harmonicSeven = normalizePFD $ Factors [0,0,0,1]

-- | Interval of a major seventh 16:9
minorSeventh :: PitchFactorDiagram
minorSeventh = normalizePFD $ Factors [0,-2]

-- | Interval of a major seventh 15:8
majorSeventh :: PitchFactorDiagram
majorSeventh = normalizePFD $ Factors [0,1,1]

-- | Interval of a minor second 16:15
minorSecond :: PitchFactorDiagram
minorSecond = normalizePFD . invertPFD $ majorSeventh

-- | Interval of a major second 9:8
majorSecond :: PitchFactorDiagram
majorSecond = normalizePFD $ Factors [0,2]

-- | Interval 25:16
mystery25 :: PitchFactorDiagram
mystery25 = normalizePFD $ Factors [0,0,2]

tritone :: PitchFactorDiagram
tritone = countPFDFuzzy $ sqrt 2

majorScale :: [PitchFactorDiagram]
majorScale = fmap fromMajorScale [0..11]

-- | Interval 199:200. Should be mostly consonant to your ear but has non-small PFD:
-- @
--  [-3,0,-2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
-- @
counterExample :: PitchFactorDiagram
counterExample = Factors $ [-3,0,-2] ++ take 42 (repeat 0) ++ [1]

majorChord :: Chord
majorChord = chordOf [unison,majorThird,perfectFifth]

minorChord :: Chord
minorChord = chordOf [unison,minorThird,perfectFifth]

fphRanges :: [[PitchFactorDiagram]]
fphRanges = fmap (fmap fromDiatonic) [bass,tenor,alto,soprano]
  where
    soprano = [0..19]
    alto = [-5..12]
    tenor = [-12..7]
    bass = [-19..0]
