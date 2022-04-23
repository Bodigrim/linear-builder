-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>

module Main where

import Test.Tasty.Bench

import BenchChar
import BenchDecimal
import BenchDouble
import BenchHexadecimal
import BenchText

main ∷ IO ()
main = defaultMain
  [ benchText
  , benchChar
  , benchDecimal
  , benchHexadecimal
  , benchDouble
  ]
