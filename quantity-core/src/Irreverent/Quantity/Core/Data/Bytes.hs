{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Quantity.Core.Data.Bytes
-- Copyright    : (C) 2017-2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Quantity.Core.Data.Bytes (
  -- * Types
    BytesQuantity
  , BytesUnits(..)
  -- * Functions
  , useUnits
  , bytes
  , kbytes
  , mbytes
  , gbytes
  , tbytes
  , pbytes
  , inBytes
  , inKBytes
  , inMBytes
  , inGBytes
  , inTBytes
  , inPBytes
  ) where

import Data.Data (Data)

import Preamble

-- wraps an integer representing bytes
newtype BytesQuantity = BytesQuantity Integer deriving (Show, Eq, Data)

instance Monoid BytesQuantity where
--mempty :: a
  mempty = BytesQuantity 0

--mappend :: a -> a -> a
  mappend x y = bytes $ ((+) `on` inBytes :: BytesQuantity -> BytesQuantity -> Integer) x y

data BytesUnits =
    Bytes
  | Kilobytes
  | Megabytes
  | Gigabytes
  | Terabytes
  | Petabytes
    deriving (Show, Eq, Data)

useUnits :: (Integral a) => a -> BytesUnits -> BytesQuantity
useUnits = flip $ \case
  Bytes       -> bytes
  Kilobytes   -> kbytes
  Megabytes   -> mbytes
  Gigabytes   -> gbytes
  Terabytes   -> tbytes
  Petabytes   -> pbytes

bytes :: (Integral a) => a -> BytesQuantity
bytes = BytesQuantity . toInteger

kbytes :: (Integral a) => a -> BytesQuantity
kbytes = BytesQuantity . (* 1024) . toInteger

mbytes :: (Integral a) => a -> BytesQuantity
mbytes = BytesQuantity . (* (1024 * 1024)) . toInteger

gbytes :: (Integral a) => a -> BytesQuantity
gbytes = BytesQuantity . (* (1024 * 1024 * 1024)) . toInteger

tbytes :: (Integral a) => a -> BytesQuantity
tbytes = BytesQuantity . (* (1024 * 1024 * 1024 * 1024)) . toInteger

pbytes :: (Integral a) => a -> BytesQuantity
pbytes = BytesQuantity . (\x -> x * 1024 * 1024 * 1024 * 1024 * 1024) . toInteger

inBytes :: (Integral a) => BytesQuantity -> a
inBytes (BytesQuantity b) = fromInteger b

inKBytes :: (Integral a) => BytesQuantity -> (a, BytesQuantity)
inKBytes (BytesQuantity t) = (fromInteger (t `quot` 1024), BytesQuantity $ t `mod` 1024)

inMBytes :: (Integral a) => BytesQuantity -> (a, BytesQuantity)
inMBytes (BytesQuantity t) = (fromInteger (t `quot` (1024 * 1024)), BytesQuantity $ t `mod` (1024 * 1024))

inGBytes :: (Integral a) => BytesQuantity -> (a, BytesQuantity)
inGBytes (BytesQuantity t) = (fromInteger (t `quot` (1024 * 1024 * 1024)), BytesQuantity $ t `mod` (1024 * 1024 * 1024))

inTBytes :: (Integral a) => BytesQuantity -> (a, BytesQuantity)
inTBytes (BytesQuantity t) = (fromInteger (t `quot` (1024 * 1024 * 1024 * 1024)), BytesQuantity $ t `mod` (1024 * 1024 * 1024 * 1024))

inPBytes :: (Integral a) => BytesQuantity -> (a, BytesQuantity)
inPBytes (BytesQuantity t) = (fromInteger (t `quot` (1024 * 1024 * 1024 * 1024 * 1024)), BytesQuantity $ t `mod` (1024 * 1024 * 1024 * 1024 * 1024))
