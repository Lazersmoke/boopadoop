-- | Representing rhythms as rose trees.
module Boopadoop.Rhythm where

import Data.Numbers.Primes
import Data.List.Split

-- | A rhythm is represented as a rose tree where each subtree is given equal amounts of time.
-- Leaves are either a Beat of type @a@ or empty (a rest).
data Beat a = RoseBeat [Beat a] | Beat a | Rest

-- | Class for things that can be summarized in a single character, for use in printing out rhythms.
class SummaryChar a where
  sumUp :: a -> Char

-- | Show the rhythm by printing the summary characters, or @'.'@ for rests.
instance SummaryChar a => Show (Beat a) where
  show (RoseBeat bs) = "[" ++ (bs >>= show) ++ "]"
  show (Beat x) = [sumUp x]
  show Rest = "."

-- | A rack of drums. Simple enumeration of the different possible drum types.
data DrumRack = Kick | Snare

instance SummaryChar DrumRack where
  sumUp Kick = 'O'
  sumUp Snare = 'x'

-- | The standard rock beat (or half of it) played on the 'DrumRack'
rockBeat :: Beat DrumRack
rockBeat = RoseBeat [Beat Kick, Rest, Beat Snare, Rest]

-- | Force there to be only prime divisions of time in the rhythm.
-- This is done without affecting the actual rhythm.
-- This operation is not uniquely valued in any way, and this algorithm prefers small primes first.
primeBeat :: Beat a -> Beat a
primeBeat (RoseBeat bs)
  | isPrime (length bs) = RoseBeat $ map primeBeat bs
  | otherwise = let (pf:_) = reverse $ primeFactors (length bs) in primeBeat . RoseBeat . map RoseBeat $ chunksOf pf bs
primeBeat x = x
