{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------
-- |
-- Module       : Test.Irreverent.Quantity.Megaparsec.Data.Bytes
-- Copyright    : (C) 2017 -2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Test.Irreverent.Quantity.Megaparsec.Data.Bytes where

import Irreverent.Quantity.Core.Data.Bytes
import qualified Irreverent.Quantity.Megaparsec.Data.Bytes as Q

import Text.Megaparsec (ParseError, runParser)

import qualified Ultra.Data.Text as T

import Lab.Core.Control.RoundTrip
import Lab.Core.QuickCheck
import Lab.Core.QuickCheck.TH

import Test.Irreverent.Quantity.Core.Arbitrary

import Test.QuickCheck (NonNegative(..))

import Preamble

prop_bytesQuantity :: Property
prop_bytesQuantity =
  let
    abbrev :: BytesUnits -> T.Text
    abbrev Bytes        = "b"
    abbrev Kilobytes    = "kb"
    abbrev Megabytes    = "mb"
    abbrev Gigabytes    = "gb"
    abbrev Terabytes    = "tb"
    abbrev Petabytes    = "pb"
  in forAll (arbitrary :: Gen (NonNegative Integer)) $ \(NonNegative x) ->
    forAll bytesUnits $ \u ->
      roundTripProp (const $ (T.pack . show) x <> abbrev u) (runParser Q.bytesQuantity "" :: T.Text -> Either (ParseError Char Void) BytesQuantity) $ useUnits x u

return []
tests :: IO Bool
tests = $quickCheckAll
