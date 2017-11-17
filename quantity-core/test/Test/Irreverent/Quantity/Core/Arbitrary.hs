{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Test.Irreverent.Quantity.Core.Arbitrary
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Test.Irreverent.Quantity.Core.Arbitrary (
    -- * Generators
        bytesUnits
    ,   bytesQuantity
    ,   durationUnits
    ,   durationQuantity
    ,   frequencyUnits
    ,   frequencyQuantity
    ) where

import Irreverent.Quantity.Core.Data.Bytes as B
import Irreverent.Quantity.Core.Data.Frequency as F
import Irreverent.Quantity.Core.Data.Time as T

import Lab.Core.QuickCheck

import Preamble

bytesUnits :: Gen BytesUnits
bytesUnits = elements
  [ Bytes
  , Kilobytes
  , Megabytes
  , Gigabytes
  , Terabytes
  , Petabytes
  ]

durationUnits :: Gen DurationUnits
durationUnits = elements
  [ Microseconds
  , Milliseconds
  , Centiseconds
  , Deciseconds
  , Seconds
  , Minutes
  , Hours
  , Days
  , Weeks
  ]

frequencyUnits :: Gen FrequencyUnits
frequencyUnits = elements
  [ Hz
  , KiloHz
  , MegaHz
  , GigaHz
  , TeraHz
  , PetaHz
  ]

bytesQuantity :: Gen BytesQuantity
bytesQuantity = B.useUnits
  <$> fmap (\(NonNegative n) -> n) (arbitrary :: Gen (NonNegative Integer))
  <*> bytesUnits

durationQuantity :: Gen DurationQuantity
durationQuantity = T.useUnits
  <$> fmap (\(NonNegative n) -> n) (arbitrary :: Gen (NonNegative Integer))
  <*> durationUnits

frequencyQuantity :: Gen FrequencyQuantity
frequencyQuantity = F.useUnits
  <$> fmap (\(NonNegative n) -> n) (arbitrary :: Gen (NonNegative Integer))
  <*> frequencyUnits
