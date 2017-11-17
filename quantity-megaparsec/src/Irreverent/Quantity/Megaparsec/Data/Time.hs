{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Quantity.Megaparsec.Data.Time
-- Copyright    : (C) 2017 - 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Quantity.Megaparsec.Data.Time (
    -- * Parsers
        durationQuantity
    ,   durationUnit
    ) where

import Irreverent.Quantity.Core.Data.Time

import qualified Ultra.Data.Text as T

import Text.Megaparsec (ParsecT, try)
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Char.Lexer (decimal)

import Data.Monoid ((<>))

import Preamble hiding ((<>))

durationUnit :: (Ord e) => ParsecT e T.Text m DurationUnits
durationUnit =
      try (Microseconds <$ string "us")
  <|> try (Milliseconds <$ string "ms")
  <|> try (Centiseconds <$ string "cs")
  <|> try (Deciseconds <$ string "ds")
  <|> try (Seconds <$ string "s")
  <|> try (Minutes <$ string "min")
  <|> try (Hours <$ string "h")
  <|> try (Days <$ string "days")
  <|> Weeks <$ string "w"

durationQuantity :: (Ord e) => ParsecT e T.Text m DurationQuantity
durationQuantity =
    let
        f :: Integer -> DurationUnits -> DurationQuantity
        f = useUnits
    in foldr (<>) mempty <$> some (f <$> decimal <*> durationUnit)
