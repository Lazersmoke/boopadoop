{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Boopadoop.Discrete where

import Data.Int
import Data.Bits

data Stream a = Stream !a (Stream a)

-- | @'Discrete' x@ represents @x/'discFactor'@ as a floating point number in [-1,1].
newtype Discrete = Discrete {unDiscrete :: Int32} deriving (Eq,Ord)

instance Show Discrete where
  show = show . unDiscrete --discreteToDouble

-- | Breaks when the double is not in [-1,1]
doubleToDiscrete :: Double -> Discrete
doubleToDiscrete x = Discrete . properFloor $ x * discFactor

-- | Convert @'Discrete'@ to the @'Double'@ it represents.
discreteToDouble :: Discrete -> Double
discreteToDouble (Discrete x) = fromIntegral x / discFactor

-- | This is the conversion factor between the internal value of a @'Discrete'@ and the value it represents.
discFactor :: Num a => a
discFactor = fromIntegral $ (maxBound :: Int32)
{-# SPECIALISE discFactor :: Discrete #-}
{-# SPECIALISE discFactor :: Double #-}
{-# SPECIALISE discFactor :: Int32 #-}

-- | Round toward zero
properFloor :: Double -> Int32
properFloor = if spookyFastHack
  then spookyFastTruncate
  else floor
  where
    spookyFastHack = True

-- | Spooky fast double to int32 truncation from http://stereopsis.com/sree/fpu2006.html
spookyFastTruncate :: Double -> Int32
spookyFastTruncate x = fromIntegral . fst . decodeFloat $ x + magic
  where
    magic = 6755399441055744.0 :: Double

instance Num Discrete where
  (Discrete a) + (Discrete b) = Discrete $ let s = a + b in if signum a == signum b && signum a /= signum s then error ("Discrete overflow! " ++ show (Discrete a) ++ " + " ++ show (Discrete b) ++ " /= " ++ show (Discrete s)) else s
  a - b = a + negate b
  (*) = multiplyDiscrete
  negate (Discrete a) = Discrete (negate a)
  abs (Discrete a) = Discrete (abs a)
  signum (Discrete a) = Discrete (signum a)
  fromInteger i = if i `elem` [-1,0,1]
    then Discrete $ discFactor * (fromInteger i :: Int32)
    else error $ "(fromInteger " ++ show i ++ " :: Discrete)"

-- | Perform fast @'Discrete'@ multiplication.
multiplyDiscrete :: Discrete -> Discrete -> Discrete
multiplyDiscrete (Discrete a) (Discrete b) = let m = unCheckedMultiplyDiscrete (Discrete a) (Discrete b) in if signum m /= 0 && signum a * signum b /= signum (unDiscrete m) then error ("Discrete multiply overflow!! " ++ show (Discrete a) ++ " * " ++ show (Discrete b) ++ " /= " ++ show m) else m

fastIncorrectMultiplyDiscrete :: Discrete -> Discrete -> Discrete
fastIncorrectMultiplyDiscrete (Discrete a) (Discrete b) = Discrete . fromIntegral $ ((fromIntegral (a `shiftR` 16) :: Int16) * (fromIntegral (b `shiftR` 16) :: Int16)) `shiftL` 16

unCheckedMultiplyDiscrete :: Discrete -> Discrete -> Discrete
unCheckedMultiplyDiscrete (Discrete a) (Discrete b) = Discrete . fromIntegral $ ((fromIntegral a :: Int64) * (fromIntegral b :: Int64)) `div` (discFactor + 1)

unCheckedDivideDiscrete :: Discrete -> Discrete -> Discrete
unCheckedDivideDiscrete (Discrete a) (Discrete b) = Discrete . fromIntegral $ ((fromIntegral a :: Int64) * (discFactor + 1)) `div` fromIntegral b

divideDiscrete :: Discrete -> Discrete -> Discrete
(Discrete a) `divideDiscrete` (Discrete b) = let d = unCheckedDivideDiscrete (Discrete a) (Discrete b) in if signum d /= 0 && signum a * signum b /= signum (unDiscrete d) then error ("Discrete division overflow!! " ++ show (Discrete a) ++ " / " ++ show (Discrete b) ++ " /= " ++ show d) else d

instance Fractional Discrete where
  (/) = unCheckedDivideDiscrete
  fromRational r = if r <= 1 && r >= -1
    then Discrete . floor $ discFactor * r
    else error $ "(fromRational " ++ show r ++ " :: Discrete)"

instance Bounded Discrete where
  minBound = -1
  maxBound = 1

-- | Make a function of doubles a function of discretes
disguise :: (Double -> Double) -> Discrete -> Discrete
disguise f (Discrete x) = Discrete . properFloor $ f (fromIntegral x / discFactor :: Double) * discFactor

-- | A discrete representation of time. See @'Boopadoop.tickTable'@ for the sampling rate.
type Tick = Int

--cisDiscrete :: Double -> Complex Discrete
--cisDiscrete t = let (a :+ b) = cis t in doubleToDiscrete a :+ doubleToDiscrete b

computeExternally :: (Double -> Double) -> Discrete -> Discrete
computeExternally f = doubleToDiscrete . f . discreteToDouble

-- | A well-behaved type for phases. Always takes a value in [0,1]
--newtype WBPhase = WBPhase {getW32Repr :: Word32}

