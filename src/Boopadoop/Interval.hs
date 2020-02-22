{-# LANGUAGE TypeApplications #-}
-- | Some common musical intervals written as @'PitchFactorDiagram'@s
module Boopadoop.Interval where

import Data.Ratio
import Boopadoop.Diagram

fromMajorScale :: Int -> Octaved PitchFactorDiagram
fromMajorScale = fromPitchList majorScale

fromDiatonic :: Int -> Octaved PitchFactorDiagram
fromDiatonic = fromPitchList [unison,minorSecond,majorSecond,minorThird,majorThird,perfectFourth,tritone,perfectFifth,minorSixth,majorSixth,minorSeventh,majorSeventh]

fromPitchList :: [a] -> Int -> Octaved a
fromPitchList ps k = Octaved o (ps !! i)
  where
    (o,i) = k `divMod` length ps

ttPFD :: Octaved TwelveTone -> Octaved PitchFactorDiagram
ttPFD = fromDiatonic . ttDeoctave

--classInterval :: PitchClass -> PitchFactorDiagram -> PitchFactorDiagram
--classInterval pc pfd = addPFD pfd $ inOctave 0 pc

-- | The non interval, ratio is 1. Identity of `addPFD`.
unison :: Monoid a => a
unison = mempty

-- | Interval of a perfect fourth 4:3
perfectFourth :: PitchFactorDiagram
perfectFourth = complPitchClass perfectFifth

-- | Interval of a perfect fifth 3:2
perfectFifth :: PitchFactorDiagram
perfectFifth = Factors [1]

-- | Interval of a major third 5:4
majorThird :: PitchFactorDiagram
majorThird = Factors [0,1]

-- | Interval of a minor third 6:5
minorThird :: PitchFactorDiagram
minorThird = Factors [1,-1]

-- | Interval of a minor sixth 8:5
minorSixth :: PitchFactorDiagram
minorSixth = complPitchClass majorThird

-- | Interval of a major sixth 5:3
majorSixth :: PitchFactorDiagram
majorSixth = complPitchClass minorThird

-- | Interval 7:4
harmonicSeven :: PitchFactorDiagram
harmonicSeven = Factors [0,0,1]

-- | Interval of a major seventh 16:9
minorSeventh :: PitchFactorDiagram
minorSeventh = Factors [-2]

-- | Interval of a major seventh 15:8
majorSeventh :: PitchFactorDiagram
majorSeventh = Factors [1,1]

-- | Interval of a minor second 16:15
minorSecond :: PitchFactorDiagram
minorSecond = complPitchClass majorSeventh

-- | Interval of a major second 9:8
majorSecond :: PitchFactorDiagram
majorSecond = Factors [2]

-- | Interval 25:16
mystery25 :: PitchFactorDiagram
mystery25 = Factors [0,2]

tritone :: PitchFactorDiagram
tritone = countPFD $ approxRational @Double (sqrt 2) 0.001

majorScale :: [PitchFactorDiagram]
majorScale = [unison,majorSecond,majorThird,perfectFourth,perfectFifth,majorSixth,majorSeventh]

-- | Interval 199:200. Should be mostly consonant to your ear but has non-small PFD:
-- @
--  [-3,0,-2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
-- @
counterExample :: PitchFactorDiagram
counterExample = Factors $ [-3,0,-2] ++ take 42 (repeat 0) ++ [1]

majorChord :: Chord PitchFactorDiagram
majorChord = chordOf [unison,majorThird,perfectFifth]

minorChord :: Chord PitchFactorDiagram
minorChord = chordOf [unison,minorThird,perfectFifth]

powerChord :: ChordVoicing PitchFactorDiagram
powerChord = chordOf [inOctave 0 unison,inOctave 0 perfectFifth,inOctave 1 unison]

fphRanges :: [[Octaved PitchFactorDiagram]]
fphRanges = fmap (fmap fromDiatonic) [bass,tenor,alto,soprano]
  where
    soprano = [0..19]
    alto = [-5..12]
    tenor = [-12..7]
    bass = [-19..0]

data IntervalClassification = AscSkip | DecSkip | AscStep | DecStep | NoChange

classifyPFD :: Octaved PitchFactorDiagram -> IntervalClassification
classifyPFD pfd = if pfd == inOctave 0 unison then NoChange else if pfd > inOctave 0 unison
  then if pfd >= inOctave 0 minorThird
    then AscSkip
    else AscStep
  else if makePFDGoUp pfd >= inOctave 0 minorThird
    then DecSkip
    else DecStep
