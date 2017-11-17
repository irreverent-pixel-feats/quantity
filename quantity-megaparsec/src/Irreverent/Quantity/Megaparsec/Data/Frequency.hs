{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Quantity.Megaparsec.Data.Frequency
-- Copyright    : (C) 2017 - 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Quantity.Megaparsec.Data.Frequency (
  -- * Parsers
    frequencyQuantity
  , frequencyUnit
  ) where

import Irreverent.Quantity.Core.Data.Frequency

import qualified Ultra.Data.Text as T

import Text.Megaparsec (ParsecT, try)
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Char.Lexer (decimal)

import Data.Monoid ((<>))

import Preamble hiding ((<>))

frequencyUnit :: (Ord e) => ParsecT e T.Text m FrequencyUnits
frequencyUnit =
      try (Hz <$ string "Hz")
  <|> try (KiloHz <$ string "KHz")
  <|> try (MegaHz <$ string "MHz")
  <|> try (GigaHz <$ string "GHz")
  <|> try (TeraHz <$ string "THz")
  <|> (PetaHz <$ string "PHz")

frequencyQuantity :: (Ord e) => ParsecT e T.Text m FrequencyQuantity
frequencyQuantity =
    let
        f :: Integer -> FrequencyUnits -> FrequencyQuantity
        f = useUnits
    in foldr (<>) mempty <$> some (f <$> decimal <*> frequencyUnit)
