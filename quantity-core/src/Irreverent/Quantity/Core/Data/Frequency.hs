{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Quantity.Core.Data.Frequency
-- Copyright    : (C) 2017 - 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Quantity.Core.Data.Frequency (
  -- * Types
    FrequencyQuantity
  , FrequencyUnits(..)
  -- * Functions
  , useUnits
  , hz
  , kHz
  , mHz
  , gHz
  , tHz
  , pHz
  , inHz
  , inKHz
  , inMHz
  , inGHz
  , inTHz
  , inPHz
  ) where

import Data.Data ( Data )

import Preamble

-- wraps an integer representing hz
newtype FrequencyQuantity = FrequencyQuantity Integer deriving (Show, Eq, Data)

instance Monoid FrequencyQuantity where
--mempty :: a
  mempty = FrequencyQuantity 0

--mappend :: a -> a -> a
  mappend x y = hz $ ((+) `on` inHz :: FrequencyQuantity -> FrequencyQuantity -> Integer) x y

data FrequencyUnits =
    Hz
  | KiloHz
  | MegaHz
  | GigaHz
  | TeraHz
  | PetaHz
    deriving (Show, Eq, Data)

useUnits :: (Integral a) => a -> FrequencyUnits -> FrequencyQuantity
useUnits = flip $ \case
  Hz       -> hz
  KiloHz   -> kHz
  MegaHz   -> mHz
  GigaHz   -> gHz
  TeraHz   -> tHz
  PetaHz   -> pHz

hz :: (Integral a) => a -> FrequencyQuantity
hz = FrequencyQuantity . toInteger

kHz :: (Integral a) => a -> FrequencyQuantity
kHz = FrequencyQuantity . (* 1000) . toInteger

mHz :: (Integral a) => a -> FrequencyQuantity
mHz = FrequencyQuantity . (* (1000 * 1000)) . toInteger

gHz :: (Integral a) => a -> FrequencyQuantity
gHz = FrequencyQuantity . (* (1000 * 1000 * 1000)) . toInteger

tHz :: (Integral a) => a -> FrequencyQuantity
tHz = FrequencyQuantity . (* (1000 * 1000 * 1000 * 1000)) . toInteger

pHz :: (Integral a) => a -> FrequencyQuantity
pHz = FrequencyQuantity . (\x -> x * 1000 * 1000 * 1000 * 1000 * 1000) . toInteger

inHz :: (Integral a) => FrequencyQuantity -> a
inHz (FrequencyQuantity b) = fromInteger b

inKHz :: (Integral a) => FrequencyQuantity -> (a, FrequencyQuantity)
inKHz (FrequencyQuantity t) = (fromInteger (t `quot` 1000), FrequencyQuantity $ t `mod` 1000)

inMHz :: (Integral a) => FrequencyQuantity -> (a, FrequencyQuantity)
inMHz (FrequencyQuantity t) = (fromInteger (t `quot` (1000 * 1000)), FrequencyQuantity $ t `mod` (1000 * 1000))

inGHz :: (Integral a) => FrequencyQuantity -> (a, FrequencyQuantity)
inGHz (FrequencyQuantity t) = (fromInteger (t `quot` (1000 * 1000 * 1000)), FrequencyQuantity $ t `mod` (1000 * 1000 * 1000))

inTHz :: (Integral a) => FrequencyQuantity -> (a, FrequencyQuantity)
inTHz (FrequencyQuantity t) = (fromInteger (t `quot` (1000 * 1000 * 1000 * 1000)), FrequencyQuantity $ t `mod` (1000 * 1000 * 1000 * 1000))

inPHz :: (Integral a) => FrequencyQuantity -> (a, FrequencyQuantity)
inPHz (FrequencyQuantity t) = (fromInteger (t `quot` (1000 * 1000 * 1000 * 1000 * 1000)), FrequencyQuantity $ t `mod` (1000 * 1000 * 1000 * 1000 * 1000))
