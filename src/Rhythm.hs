module Rhythm where

import Data.Numbers.Primes
import Data.List.Split

class SummaryChar a where
  sumUp :: a -> Char

data Beat a = RoseBeat [Beat a] | Beat (Maybe a)

data DrumRack = Kick | Snare
instance SummaryChar DrumRack where
  sumUp Kick = 'O'
  sumUp Snare = 'x'

-- Some long rock beat sequence
rockBeat :: Beat DrumRack
rockBeat = RoseBeat . concat $ replicate 1244 bumTss
  where
    bumTss = [Beat (Just Kick),Beat (Just Snare)]

instance SummaryChar a => Show (Beat a) where
  show (RoseBeat bs) = "[" ++ (bs >>= show) ++ "]"
  show (Beat (Just x)) = [sumUp x]
  show (Beat Nothing) = "."

-- Force there to be only prime divisions of time
primeBeat :: Beat a -> Beat a
primeBeat (RoseBeat bs)
  | isPrime (length bs) = RoseBeat $ map primeBeat bs
  | otherwise = let (pf:_) = reverse $ primeFactors (length bs) in primeBeat . RoseBeat . map RoseBeat $ chunksOf pf bs
primeBeat x = x
