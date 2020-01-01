module Main where

import Boopadoop
import Boopadoop.Example

main = foldl (flip seq) (pure ()) $ map (unDiscrete . sample filteredTicks) [0 .. 32000]
