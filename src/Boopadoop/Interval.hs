-- | Some common musical intervals written as @'PitchFactorDiagram'@s
module Boopadoop.Interval where

import Boopadoop.Diagram

-- | Interval of one octave, ratio is 2.
octave :: PitchFactorDiagram
octave = Factors [1]

-- | Interval of a perfect fifth 3:2
perfectFifth :: PitchFactorDiagram
perfectFifth = normalizePFD $ Factors [0,1]

-- | Interval of a major third 5:4
majorThird :: PitchFactorDiagram
majorThird = normalizePFD $ Factors [0,0,1]

-- | Interval 7:4
mysterySeven :: PitchFactorDiagram
mysterySeven = normalizePFD $ Factors [0,0,0,1]

-- | Interval of a major second 9:8
majorSecond :: PitchFactorDiagram
majorSecond = normalizePFD $ Factors [0,2]

-- | Interval 25:16
mystery25 :: PitchFactorDiagram
mystery25 = normalizePFD $ Factors [0,0,2]

-- | Interval 199:200. Should be mostly consonant to your ear but has non-small PFD:
-- @
--  [-3,0,-2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
-- @
counterExample :: PitchFactorDiagram
counterExample = Factors $ [-3,0,-2] ++ take 42 (repeat 0) ++ [1]

