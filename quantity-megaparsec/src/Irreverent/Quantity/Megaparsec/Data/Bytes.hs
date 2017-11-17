{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Quantity.Megaparsec.Data.Bytes
-- Copyright    : (C) 2017 - 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Quantity.Megaparsec.Data.Bytes (
    -- * Parsers
        bytesQuantity
    ,   bytesUnit
    ) where

import Irreverent.Quantity.Core.Data.Bytes

import qualified Ultra.Data.Text as T

import Text.Megaparsec (ParsecT, try)
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Char.Lexer (decimal)

import Data.Monoid ((<>))

import Preamble hiding ((<>))

bytesUnit :: (Ord e) => ParsecT e T.Text m BytesUnits
bytesUnit =
      try (Bytes <$ string "b")
  <|> try (Kilobytes <$ string "kb")
  <|> try (Megabytes <$ string "mb")
  <|> try (Gigabytes <$ string "gb")
  <|> try (Terabytes <$ string "tb")
  <|> (Petabytes <$ string "pb")

bytesQuantity :: (Ord e) => ParsecT e T.Text m BytesQuantity
bytesQuantity =
    let
        f :: Integer -> BytesUnits -> BytesQuantity
        f = useUnits
    in foldr (<>) mempty <$> some (f <$> decimal <*> bytesUnit)
