{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Quantity.Core.Data.Time
-- Copyright    : (C) 2017 - 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Quantity.Core.Data.Time (
  -- * Types
    DurationQuantity
  , DurationUnits(..)
  -- * Functions
  , useUnits
  , microseconds
  , milliseconds
  , centiseconds
  , deciseconds
  , seconds
  , minutes
  , hours
  , days
  , weeks
  , inMicroseconds
  , inMilliseconds
  , inCentiseconds
  , inDeciseconds
  , inSeconds
  , inMinutes
  , inHours
  , inDays
  , inWeeks
  ) where

import Data.Data (Data)

import Preamble

-- wraps an integer representing microseconds
newtype DurationQuantity = DurationQuantity Integer deriving (Show, Eq, Data)

instance Semigroup DurationQuantity where
--(<>) :: a -> a -> a
  (<>) x y = microseconds $ ((+) `on` inMicroseconds :: DurationQuantity -> DurationQuantity -> Integer) x y

instance Monoid DurationQuantity where
--mempty :: a
  mempty = DurationQuantity 0

--mappend :: a -> a -> a
  mappend x y = microseconds $ ((+) `on` inMicroseconds :: DurationQuantity -> DurationQuantity -> Integer) x y

data DurationUnits =
    Microseconds
  | Milliseconds
  | Centiseconds
  | Deciseconds
  | Seconds
  | Minutes
  | Hours
  | Days
  | Weeks
    deriving (Show, Eq, Data)

useUnits :: (Integral a) => a -> DurationUnits -> DurationQuantity
useUnits = flip $ \case
  Microseconds    -> microseconds
  Milliseconds    -> milliseconds
  Centiseconds    -> centiseconds
  Deciseconds     -> deciseconds
  Seconds         -> seconds
  Minutes         -> minutes
  Hours           -> hours
  Days            -> days
  Weeks           -> weeks

microseconds :: (Integral a) => a -> DurationQuantity
microseconds = DurationQuantity . toInteger

milliseconds :: (Integral a) => a -> DurationQuantity
milliseconds = DurationQuantity . (* 1000) . toInteger

centiseconds :: (Integral a) => a -> DurationQuantity
centiseconds = DurationQuantity . (* 10000) . toInteger

deciseconds :: (Integral a) => a -> DurationQuantity
deciseconds = DurationQuantity . (* 100000) . toInteger

seconds :: (Integral a) => a -> DurationQuantity
seconds = DurationQuantity . (* 1000000) . toInteger

minutes :: (Integral a) => a -> DurationQuantity
minutes = DurationQuantity . (\x -> x * 60 * 1000000) . toInteger

hours :: (Integral a) => a -> DurationQuantity
hours = DurationQuantity . (\x -> x * 60 * 60 * 1000000) . toInteger

days :: (Integral a) => a -> DurationQuantity
days = DurationQuantity . (\x -> x * 24 * 60 * 60 * 1000000) . toInteger

weeks :: (Integral a) => a -> DurationQuantity
weeks = DurationQuantity . (\x -> x * 7 * 24 * 60 * 60 * 1000000) . toInteger

inMicroseconds :: (Integral a) => DurationQuantity -> a
inMicroseconds (DurationQuantity us) = fromInteger us

-- | returns the number of milliseconds and the remainder
--
inMilliseconds :: (Integral a) => DurationQuantity -> (a, DurationQuantity)
inMilliseconds (DurationQuantity t) = (fromInteger (t `quot` 1000), DurationQuantity $ t `mod` 1000)

inCentiseconds :: (Integral a) => DurationQuantity -> (a, DurationQuantity)
inCentiseconds (DurationQuantity t) = (fromInteger (t `quot` 10000), DurationQuantity $ t `mod` 10000)

inDeciseconds :: (Integral a) => DurationQuantity -> (a, DurationQuantity)
inDeciseconds (DurationQuantity t) = (fromInteger (t `quot` 100000), DurationQuantity $ t `mod` 100000)

inSeconds :: (Integral a) => DurationQuantity -> (a, DurationQuantity)
inSeconds (DurationQuantity t) = (fromInteger (t `quot` 1000000), DurationQuantity $ t `mod` 1000000)

inMinutes :: (Integral a) => DurationQuantity -> (a, DurationQuantity)
inMinutes (DurationQuantity t) = (fromInteger (t `quot` (60 * 1000000)), DurationQuantity $ t `mod` (60 * 1000000))

inHours :: (Integral a) => DurationQuantity -> (a, DurationQuantity)
inHours (DurationQuantity t) = (fromInteger (t `quot` (60 * 60 * 1000000)), DurationQuantity $ t `mod` (60 * 60 * 1000000))

inDays :: (Integral a) => DurationQuantity -> (a, DurationQuantity)
inDays (DurationQuantity t) = (fromInteger (t `quot` (24 * 60 * 60 * 1000000)), DurationQuantity $ t `mod` (24 * 60 * 60 * 1000000))

inWeeks :: (Integral a) => DurationQuantity -> (a, DurationQuantity)
inWeeks (DurationQuantity t) = (fromInteger (t `quot` (7 * 24 * 60 * 60 * 1000000)), DurationQuantity $ t `mod` (7 * 24 * 60 * 60 * 1000000))
