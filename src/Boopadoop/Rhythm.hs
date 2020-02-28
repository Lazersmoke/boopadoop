{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}
-- | Representing rhythms as step functions of rational time
module Boopadoop.Rhythm where

import Control.Monad

-- | A 'TimeStream' is a list annotated at each entry with a @'Rational'@ length of time, in arbitary units, that it lasts for.
-- This makes a 'TimeStream' a step function of rational time that can be streamed.
data TimeStream a = TimeStream Rational a (TimeStream a) | EndStream

instance Show a => Show (TimeStream a) where
  show (TimeStream t x xs@(TimeStream _ _ _)) = "<" ++ show x ++ "|" ++ take 4 (show (fromRational t :: Double) ++ repeat '0') ++ "> :> " ++ show xs
  show (TimeStream t x EndStream) = "<" ++ show x ++ "|" ++ take 4 (show (fromRational t :: Double) ++ repeat '0') ++ "> :|"
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

-- | Zip two timestreams together with a combining function, interlacing them as required.
modulateTimeStream :: (a -> b -> c) -> TimeStream a -> TimeStream b -> TimeStream c
modulateTimeStream f (TimeStream tl xl xsl) (TimeStream tr xr xsr) = case compare tl tr of
  LT -> TimeStream tl (f xl xr) $ modulateTimeStream f xsl (TimeStream (tr - tl) xr xsr)
  EQ -> TimeStream tl (f xl xr) $ modulateTimeStream f xsl xsr
  GT -> TimeStream tr (f xl xr) $ modulateTimeStream f (TimeStream (tl - tr) xl xsl) xsr
modulateTimeStream _ _ _ = EndStream

instance Foldable TimeStream where
  foldr f z (TimeStream _ x xs) = f x (foldr f z xs)
  foldr _ z EndStream = z

-- | Reverse a @'TimeStream'@. This is O(n) and of course breaks for infinite 'TimeStream's
reverseTimeStream :: TimeStream a -> TimeStream a
reverseTimeStream = go EndStream
  where
    go acc EndStream = acc
    go acc (TimeStream t x xs) = go (TimeStream t x acc) xs

-- | Limit a (possibly infinite) 'TimeStream' to have length at most @tmax@. This is 'take' but it works over the time domain.
limitTimeStream :: Rational -> TimeStream a -> TimeStream a
limitTimeStream !tmax (TimeStream t x xs) = if t < tmax
  then TimeStream t x (limitTimeStream (tmax - t) xs)
  else TimeStream tmax x EndStream
limitTimeStream _ EndStream = EndStream

-- | Use the first 'TimeStream' until it ends or a specified time (whichever is earlier), then branch to the second 'TimeStream'.
-- Also returns the last value of the first stream before the branch occurs
branchAfterTimeStream :: Rational -> TimeStream a -> TimeStream a -> (a,TimeStream a)
branchAfterTimeStream !tmax (TimeStream t x EndStream) branch = (x,TimeStream (min t tmax) x branch)
branchAfterTimeStream !tmax (TimeStream t x xs) branch = if t < tmax
  then let (l,ts) = branchAfterTimeStream (tmax - t) xs branch in (l,TimeStream t x ts)
  else (x,TimeStream tmax x branch)
branchAfterTimeStream _ EndStream branch = (error "branchAfterTimeStream: forced last element of empty stream",branch)

-- | Use the first 'TimeStream' until it ends, then switch to the second one, and return the last value before the branch.
-- Same as @('branchAfterTimeStream' Infinity)@.
branchAfterEndStream :: TimeStream a -> TimeStream a -> (a,TimeStream a)
branchAfterEndStream (TimeStream t x EndStream) branch = (x,TimeStream t x branch)
branchAfterEndStream (TimeStream t x xs) branch = let (l,ts) = branchAfterEndStream xs branch in (l,TimeStream t x ts)
branchAfterEndStream EndStream branch = (error "branchAfterEndStream: forced last element of empty stream",branch)

-- | Unsafe 'tail' for 'TimeStream's
forceLastAppendTimeStream :: TimeStream a -> a
forceLastAppendTimeStream (TimeStream _ x EndStream) = x
forceLastAppendTimeStream (TimeStream _ _ xs) = forceLastAppendTimeStream xs
forceLastAppendTimeStream _ = error "forceLastAppendTimeStream: Empty TimeStream"

-- | Map a 'TimeStream' in a way that depends on the timings.
overTimingsTimeStream :: (Rational -> a -> b) -> TimeStream a -> TimeStream b
overTimingsTimeStream f (TimeStream t x xs) = TimeStream t (f t x) $ overTimingsTimeStream f xs
overTimingsTimeStream _ EndStream = EndStream

-- | Stretch a 'TimeStream' in the time domain by a constant factor.
stretchTimeStream :: Rational -> TimeStream a -> TimeStream a
stretchTimeStream factor (TimeStream t x xs) = TimeStream (t * factor) x (stretchTimeStream factor xs)
stretchTimeStream _ EndStream = EndStream

-- | Class for things that can be summarized in a single character, for use in printing out rhythms.
class SummaryChar a where
  sumUp :: a -> Char

instance SummaryChar a => SummaryChar (Maybe a) where
  sumUp Nothing = '_'
  sumUp (Just x) = sumUp x

instance SummaryChar () where
  sumUp () = '\''

instance SummaryChar Char where
  sumUp = id

instance SummaryChar Bool where
  sumUp True = '1'
  sumUp False = '0'

-- | Produce a 'TimeStream' from a list of steps, with equal timing
equalTime :: [a] -> TimeStream a
equalTime = foldr (TimeStream 1) EndStream

-- | Repeat a 'TimeStream' multiple times
replicateTimeStream :: Int -> TimeStream a -> TimeStream a
replicateTimeStream k b = equalTime (replicate k ()) >> b
