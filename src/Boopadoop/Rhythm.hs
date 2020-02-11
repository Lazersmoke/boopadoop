{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}
-- | Representing rhythms as rose trees.
module Boopadoop.Rhythm where

import Data.Tree
import Control.Monad
import Debug.Trace
import GHC.Exts

-- | A rhythm is represented as a rose tree where each subtree is given time with integer weights.
-- Leaves are any data.
data Beat a = RoseBeat [(Int,Beat a)] | Beat a deriving (Functor)

newtype Timed a = Timed [(Int,a)] deriving Functor

data TimeStream a = TimeStream Rational a (TimeStream a) | EndStream

instance SummaryChar a => Show (TimeStream a) where
  show (TimeStream t x xs@(TimeStream _ _ _)) = "<" ++ [sumUp x] ++ "|" ++ take 4 (show (fromRational t :: Double) ++ repeat '0') ++ "> :> " ++ show xs
  show (TimeStream t x EndStream) = "<" ++ [sumUp x] ++ "|" ++ take 4 (show (fromRational t :: Double) ++ repeat '0') ++ "> :|"
  show EndStream = "EndStream"

instance Functor TimeStream where
  fmap f (TimeStream t x xs) = TimeStream t (f x) (fmap f xs)
  fmap _ EndStream = EndStream

instance Applicative TimeStream where
  pure x = TimeStream 1 x EndStream
  (<*>) = ap

instance Monad TimeStream where
  times >>= f = join' $ fmap f times
    where
      join' (TimeStream t ts xs) = go (join' xs) t ts
      join' EndStream = EndStream
      go k t' (TimeStream t x xs) = TimeStream (t * t') x (go k t' xs)
      go k _ EndStream = k

modulateTimeStream :: (a -> b -> c) -> TimeStream a -> TimeStream b -> TimeStream c
modulateTimeStream f (TimeStream tl xl xsl) (TimeStream tr xr xsr) = case compare tl tr of
  LT -> TimeStream tl (f xl xr) $ modulateTimeStream f xsl (TimeStream (tr - tl) xr xsr)
  EQ -> TimeStream tl (f xl xr) $ modulateTimeStream f xsl xsr
  GT -> TimeStream tr (f xl xr) $ modulateTimeStream f (TimeStream (tl - tr) xl xsl) xsr
modulateTimeStream _ _ _ = EndStream

instance Foldable TimeStream where
  foldr f z (TimeStream _ x xs) = f x (foldr f z xs)
  foldr _ z EndStream = z

reverseTimeStream :: TimeStream a -> TimeStream a
reverseTimeStream = go EndStream
  where
    go acc EndStream = acc
    go acc (TimeStream t x xs) = go (TimeStream t x acc) xs

limitTimeStream :: Rational -> TimeStream a -> TimeStream a
limitTimeStream !tmax (TimeStream t x xs) = if t < tmax
  then TimeStream t x (limitTimeStream (tmax - t) xs)
  else TimeStream tmax x EndStream
limitTimeStream _ EndStream = EndStream

overTimings :: (Int -> a -> b) -> Timed a -> Timed b
overTimings f (Timed xs) = Timed $ fmap (\(k,a) -> (k,f k a)) xs

overTimingsTimeStream :: (Rational -> a -> b) -> TimeStream a -> TimeStream b
overTimingsTimeStream f (TimeStream t x xs) = TimeStream t (f t x) $ overTimingsTimeStream f xs
overTimingsTimeStream _ EndStream = EndStream

instance SummaryChar a => Show (Timed a) where
  show (Timed bs) = "[" ++ (bs >>= \(k,b) -> sumUp b : replicate (k-1) '-') ++ "]"

instance Semigroup (Timed a) where
  (<>) (Timed a) (Timed b) = Timed (a ++ b)

instance Monoid (Timed a) where
  mempty = Timed []

instance Applicative Timed where
  pure x = Timed [(1,x)]
  (<*>) = ap

instance Monad Timed where
  t >>= f = join' $ fmap f t
    where
      join' (Timed xs) = Timed $ concatMap (\(k,Timed timed) -> fmap (\(k',x) -> (k * k',x)) timed) xs

instance IsList (Timed a) where
  type Item (Timed a) = (Int,a)
  fromList = Timed
  toList (Timed t) = t

viewBeat :: Beat String -> String
viewBeat beat = drawTree . toTree $ beat
  where
    toTree (RoseBeat bs) = Node "RoseBeat" (fmap (\(k,a) -> fmap (("." ++ show k)++) $ toTree a) bs)
    toTree (Beat b) = Node ("Beat: " ++ b) []

flattenTimes :: (Beat a -> Beat a) -> Beat a -> Beat a
flattenTimes _ (Beat a) = Beat a
flattenTimes hf (RoseBeat xs) = RoseBeat $ concatMap (\(k,w) -> (1,flattenTimes hf w) : replicate (k-1) (1,hf (flattenTimes hf w))) xs

beatList :: Beat a -> [a]
beatList (Beat a) = [a]
beatList (RoseBeat xs) = concatMap (beatList . snd) xs

--analyzeBeat :: SummaryChar a => Beat a -> String
--analyzeBeat (RoseBeat bs) = show $ fmap (\(k,b) -> (k,analyzeBeat b)) $ bs
--analyzeBeat b = show b

instance Applicative Beat where
  pure = Beat
  Beat f <*> Beat x = Beat $ f x
  Beat f <*> RoseBeat xs = RoseBeat $ fmap (\(k,x) -> (k,fmap f x)) xs
  RoseBeat fs <*> x = RoseBeat $ fmap (\(k,f) -> (k,f <*> x)) fs

instance Monad Beat where
  Beat x >>= f = f x
  RoseBeat xs >>= f = RoseBeat $ fmap (\(k,x) -> (k,x >>= f)) xs

subdivs :: [(Int,a)] -> Int
subdivs = sum . fmap fst

-- | Class for things that can be summarized in a single character, for use in printing out rhythms.
class SummaryChar a where
  sumUp :: a -> Char

-- | Show the rhythm by printing the summary characters, or @'.'@ for rests.
instance SummaryChar a => Show (Beat a) where
  show (RoseBeat bs) = "[" ++ (bs >>= \(k,b) -> show b ++ replicate (k-1) '-') ++ "]"
  show (Beat x) = [sumUp x]

instance SummaryChar a => SummaryChar (Maybe a) where
  sumUp Nothing = '_'
  sumUp (Just x) = sumUp x

instance SummaryChar DrumRack where
  sumUp Kick = 'O'
  sumUp Snare = 'x'

instance SummaryChar () where
  sumUp () = '\''

instance SummaryChar Char where
  sumUp = id

instance SummaryChar Bool where
  sumUp True = '1'
  sumUp False = '0'

-- | A rack of drums. Simple enumeration of the different possible drum types.
data DrumRack = Kick | Snare

weightTimes :: [Beat a] -> Beat a
weightTimes bs = RoseBeat $ zip (fmap (`div` tot) ts) bs
  where
    ts = fmap getTime $ bs
    tot = traceShowId $ product ts
    getTime (Beat _) = 1
    getTime (RoseBeat xs) = min 1 $ sum . fmap fst $ xs

equalTime :: [Beat a] -> Beat a
equalTime = RoseBeat . fmap (1,)

-- | The standard rock beat (or half of it) played on the 'DrumRack'
rockBeat :: Beat (Maybe DrumRack)
rockBeat = equalTime [Beat (Just Kick), Beat Nothing, Beat (Just Snare), Beat Nothing]

swingIt :: Beat Int
swingIt = RoseBeat [(3,Beat 0),(1,Beat 1)]

tremelloTwice :: a -> Beat a
tremelloTwice a = equalTime [Beat a, Beat a]

swingTremelloTwice :: a -> Beat a
swingTremelloTwice a = RoseBeat [(3,Beat a), (1,Beat a)]

repeatBeat :: Int -> Beat a -> Beat a
repeatBeat k b = equalTime $ replicate k b

-- | Force there to be only prime divisions of time in the rhythm.
-- This is done without affecting the actual rhythm.
-- This operation is not uniquely valued in any way, and this algorithm prefers small primes first.
{-
primeBeat :: Beat a -> Beat a
primeBeat (RoseBeat bs)
  | isPrime (length bs) = RoseBeat $ map primeBeat bs
  | otherwise = let (pf:_) = reverse $ primeFactors (length bs) in primeBeat . RoseBeat . map RoseBeat $ chunksOf pf bs
primeBeat x = x
-}


