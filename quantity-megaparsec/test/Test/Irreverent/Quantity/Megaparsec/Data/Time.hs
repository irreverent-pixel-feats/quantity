{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------
-- |
-- Module       : Test.Irreverent.Quantity.Megaparsec.Data.Time
-- Copyright    : (C) 2017 - 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Test.Irreverent.Quantity.Megaparsec.Data.Time where

import Irreverent.Quantity.Core.Data.Time
import qualified Irreverent.Quantity.Megaparsec.Data.Time as Q

import Text.Megaparsec (ParseError, runParser)

import qualified Ultra.Data.Text as T

import Lab.Core.Control.RoundTrip
import Lab.Core.QuickCheck
import Lab.Core.QuickCheck.TH

import Test.Irreverent.Quantity.Core.Arbitrary

import Test.QuickCheck (NonNegative(..))

import Preamble

prop_durationQuantity :: Property
prop_durationQuantity =
  let
    abbrev :: DurationUnits -> T.Text
    abbrev Microseconds = "us"
    abbrev Milliseconds = "ms"
    abbrev Centiseconds = "cs"
    abbrev Deciseconds  = "ds"
    abbrev Seconds      = "s"
    abbrev Minutes      = "min"
    abbrev Hours        = "h"
    abbrev Days         = "days"
    abbrev Weeks        = "weeks"
  in forAll (arbitrary :: Gen (NonNegative Integer)) $ \(NonNegative x) ->
    forAll durationUnits $ \u ->
      roundTripProp (const $ (T.pack . show) x <> abbrev u) (runParser Q.durationQuantity "" :: T.Text -> Either (ParseError Char Void) DurationQuantity) $ useUnits x u

return []
tests :: IO Bool
tests = $quickCheckAll
