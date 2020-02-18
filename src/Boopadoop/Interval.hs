-- | Some common musical intervals written as @'PitchFactorDiagram'@s
module Boopadoop.Interval where

import Data.Ratio
import Boopadoop.Diagram

fromMajorScale :: Int -> PitchFactorDiagram
fromMajorScale = fromPitchList majorScale

fromDiatonic :: Int -> PitchFactorDiagram
fromDiatonic = fromPitchList [unison,minorSecond,majorSecond,minorThird,majorThird,perfectFourth,tritone,perfectFifth,minorSixth,majorSixth,minorSeventh,majorSeventh]

fromPitchList :: [PitchClass] -> Int -> PitchFactorDiagram
fromPitchList ps k = classInOctave (fromIntegral o) (ps !! i)
  where
    (o,i) = k `divMod` length ps

ttPFD :: TwelveTone -> PitchFactorDiagram
ttPFD (TwelveTone k) = fromDiatonic k

classInterval :: PitchClass -> PitchFactorDiagram -> PitchFactorDiagram
classInterval pc pfd = addPFD pfd $ classInOctave 0 pc

-- | The non interval, ratio is 1. Identity of `addPFD`.
unison :: PitchClass
unison = ClassFactors []

-- | Interval of a perfect fourth 4:3
perfectFourth :: PitchClass
perfectFourth = complPitchClass perfectFifth

-- | Interval of a perfect fifth 3:2
perfectFifth :: PitchClass
perfectFifth = ClassFactors [1]

-- | Interval of a major third 5:4
majorThird :: PitchClass
majorThird = ClassFactors [0,1]

-- | Interval of a minor third 6:5
minorThird :: PitchClass
minorThird = ClassFactors [1,-1]

-- | Interval of a minor sixth 8:5
minorSixth :: PitchClass
minorSixth = complPitchClass majorThird

-- | Interval of a major sixth 5:3
majorSixth :: PitchClass
majorSixth = complPitchClass minorThird

-- | Interval 7:4
harmonicSeven :: PitchClass
harmonicSeven = ClassFactors [0,0,1]

-- | Interval of a major seventh 16:9
minorSeventh :: PitchClass
minorSeventh = ClassFactors [-2]

-- | Interval of a major seventh 15:8
majorSeventh :: PitchClass
majorSeventh = ClassFactors [1,1]

-- | Interval of a minor second 16:15
minorSecond :: PitchClass
minorSecond = complPitchClass majorSeventh

-- | Interval of a major second 9:8
majorSecond :: PitchClass
majorSecond = ClassFactors [2]

-- | Interval 25:16
mystery25 :: PitchClass
mystery25 = ClassFactors [0,2]

tritone :: PitchClass
tritone = getPitchClass . countPFD $ approxRational (sqrt 2) 0.001

majorScale :: [PitchClass]
majorScale = [unison,majorSecond,majorThird,perfectFourth,perfectFifth,majorSixth,majorSeventh]

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

powerChord :: ChordVoicing
powerChord = voiceChord [classInOctave 0 unison,classInOctave 0 perfectFifth,octave]

fphRanges :: [[PitchFactorDiagram]]
fphRanges = fmap (fmap fromDiatonic) [bass,tenor,alto,soprano]
  where
    soprano = [0..19]
    alto = [-5..12]
    tenor = [-12..7]
    bass = [-19..0]

data IntervalClassification = AscSkip | DecSkip | AscStep | DecStep | NoChange

classifyPFD :: PitchFactorDiagram -> IntervalClassification
classifyPFD pfd = if pfd == Factors [] then NoChange else if pfd > Factors []
  then if pfd >= classInOctave 0 minorThird
    then AscSkip
    else AscStep
  else if makePFDGoUp pfd >= classInOctave 0 minorThird
    then DecSkip
    else DecStep
