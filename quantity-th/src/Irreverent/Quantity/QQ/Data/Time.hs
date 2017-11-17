{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Quantity.QQ.Data.Time
-- Copyright    : (C) 2017-2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Quantity.QQ.Data.Time (
  -- * Quasiquoters
    durationQ
  ) where

import Irreverent.Quantity.Core.Data.Time
import Irreverent.Quantity.Megaparsec.Data.Time

import qualified Ultra.Data.Text as T

import Text.Megaparsec (Parsec, runParser, eof)

import Data.Generics ( extQ )
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Preamble
import Prelude ( error )

durationQ :: QuasiQuoter
durationQ =
    let
        p :: String -> Q Exp
        p s = case either (const Nothing) pure $ runParser ((durationQuantity :: Parsec Void T.Text DurationQuantity) <* eof) "" (T.pack s) of
            Nothing -> error $ "Failed to parse duration: " <> s
            Just x -> dataToExpQ (const Nothing `extQ` textExp) x

    in QuasiQuoter {
            quoteExp = p
        ,   quotePat = error "not able to quote pp pats"
        ,   quoteType = error "not able to quote qq types"
        ,   quoteDec = error "not able to qq decs"
        }

textExp :: T.Text -> Maybe ExpQ
textExp = pure . appE (varE 'T.pack) . litE . StringL . T.unpack
