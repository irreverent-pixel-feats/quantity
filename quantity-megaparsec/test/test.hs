module Main where

import qualified Test.Irreverent.Quantity.Megaparsec.Data.Bytes
import qualified Test.Irreverent.Quantity.Megaparsec.Data.Frequency
import qualified Test.Irreverent.Quantity.Megaparsec.Data.Time

import Lab.Core.Main

import Preamble

main :: IO ()
main = labMain
    [   Test.Irreverent.Quantity.Megaparsec.Data.Bytes.tests
    ,   Test.Irreverent.Quantity.Megaparsec.Data.Frequency.tests
    ,   Test.Irreverent.Quantity.Megaparsec.Data.Time.tests
    ]
