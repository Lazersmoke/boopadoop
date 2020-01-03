{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Boopadoop.Discrete where

import Data.Int
import Data.Bits

-- | @'Discrete' x@ represents @x/'discFactor'@ as a floating point number in [-1,1].
newtype Discrete = Discrete {unDiscrete :: Int32} deriving (Eq,Ord)

instance Show Discrete where
  show (Discrete x) = "Discrete {unDiscrete = " ++ show x ++ ", value = " ++ show (fromIntegral x / discFactor :: Double) ++ "}"

-- | Breaks when the double is not in [-1,1]
doubleToDiscrete :: Double -> Discrete
doubleToDiscrete x = Discrete . properFloor $ x * discFactor

-- | Convert @'Discrete'@ to the @'Double'@ it represents.
discreteToDouble :: Discrete -> Double
discreteToDouble (Discrete x) = fromIntegral x / discFactor

-- | This is the conversion factor between the internal value of a @'Discrete'@ and the value it represents.
discFactor :: Num a => a
discFactor = fromIntegral $ (maxBound :: Int32)

-- | Round toward zero
properFloor :: RealFrac a => a -> Int32
properFloor x = if x >= 0 then floor x else ceiling x
--properFloor = floor

instance Num Discrete where
  (Discrete a) + (Discrete b) = Discrete $ let s = a + b in if signum a == signum b && signum a /= signum s then error ("Discrete overflow! " ++ show (Discrete a) ++ " + " ++ show (Discrete b) ++ " /= " ++ show (Discrete s)) else s
  a - b = a + negate b
  (*) = multiplyDiscrete --(Discrete a) * (Discrete b) = Discrete . properFloor $ ((fromIntegral a / discFactor :: Double) * (fromIntegral b :: Double))
  negate (Discrete a) = Discrete (negate a)
  abs (Discrete a) = Discrete (abs a)
  signum (Discrete a) = Discrete (signum a)
  fromInteger i = if i `elem` [-1,0,1]
    then Discrete $ discFactor * (fromInteger i :: Int32)
    else error $ "(fromInteger " ++ show i ++ " :: Discrete)"

-- | Perform fast @'Discrete'@ multiplication.
multiplyDiscrete :: Discrete -> Discrete -> Discrete
multiplyDiscrete (Discrete a) (Discrete b) = let m = Discrete . fromIntegral $ ((fromIntegral a :: Int64) * (fromIntegral b :: Int64)) `div` (discFactor + 1) in if signum m /= 0 && signum a * signum b /= signum (unDiscrete m) then error ("Discrete multiply overflow!! " ++ show (Discrete a) ++ " * " ++ show (Discrete b) ++ " /= " ++ show m) else m

instance Fractional Discrete where
  (Discrete a) / (Discrete b) = let d = Discrete . fromIntegral $ ((fromIntegral a :: Int64) * (discFactor + 1)) `div` fromIntegral b in if signum d /= 0 && signum a * signum b /= signum (unDiscrete d) then error ("Discrete division overflow!! " ++ show (Discrete a) ++ " / " ++ show (Discrete b) ++ " /= " ++ show d) else d
  fromRational r = if r <= 1 && r >= -1
    then Discrete . properFloor $ discFactor * r
    else error $ "(fromRational " ++ show r ++ " :: Discrete)"

instance Bounded Discrete where
  minBound = -1
  maxBound = 1

-- | Make a function of doubles a function of discretes
disguise :: (Double -> Double) -> Discrete -> Discrete
disguise f (Discrete x) = Discrete . properFloor $ f (fromIntegral x / discFactor :: Double) * discFactor

-- | A discrete representation of time. See @'Boopadoop.tickTable'@ for the sampling rate.
newtype Tick = Tick {unTick :: Int32} deriving (Enum,Num,Ord,Eq,Real,Integral)

instance Show Tick where
  show (Tick a) = "Tick[" ++ show a ++ "]"

--cisDiscrete :: Double -> Complex Discrete
--cisDiscrete t = let (a :+ b) = cis t in doubleToDiscrete a :+ doubleToDiscrete b
