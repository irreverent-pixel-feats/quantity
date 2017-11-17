{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------
-- |
-- Module       : Test.Irreverent.Quantity.Megaparsec.Data.Frequency
-- Copyright    : (C) 2017 - 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Test.Irreverent.Quantity.Megaparsec.Data.Frequency where

import Irreverent.Quantity.Core.Data.Frequency
import qualified Irreverent.Quantity.Megaparsec.Data.Frequency as Q

import Text.Megaparsec (ParseError, runParser)

import qualified Ultra.Data.Text as T

import Lab.Core.Control.RoundTrip
import Lab.Core.QuickCheck
import Lab.Core.QuickCheck.TH

import Test.Irreverent.Quantity.Core.Arbitrary

import Test.QuickCheck (NonNegative(..))

import Preamble

prop_frequencyQuantity :: Property
prop_frequencyQuantity =
  let
    abbrev :: FrequencyUnits -> T.Text
    abbrev Hz        = "Hz"
    abbrev KiloHz    = "KHz"
    abbrev MegaHz    = "MHz"
    abbrev GigaHz    = "GHz"
    abbrev TeraHz    = "THz"
    abbrev PetaHz    = "PHz"
  in forAll (arbitrary :: Gen (NonNegative Integer)) $ \(NonNegative x) ->
    forAll frequencyUnits $ \u ->
      roundTripProp (const $ (T.pack . show) x <> abbrev u) (runParser Q.frequencyQuantity "" :: T.Text -> Either (ParseError Char Void) FrequencyQuantity) $ useUnits x u

return []
tests :: IO Bool
tests = $quickCheckAll
