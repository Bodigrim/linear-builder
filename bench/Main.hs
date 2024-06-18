-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
module Main where

import Test.Tasty.Bench
import Test.Tasty.Patterns.Printer

import BenchChar
import BenchDecimal
import BenchDecimalUnbounded (benchDecimalUnbounded)
import BenchDouble
import BenchHexadecimal
import BenchText

main ∷ IO ()
main =
  defaultMain $
    map (mapLeafBenchmarks addCompare) $
      [ benchText
      , benchChar
      , benchDecimal
      , benchDecimalUnbounded
      , benchHexadecimal
      , benchDouble
      ]

textBenchName ∷ String
-- textBenchName = "Data.Text.Lazy.Builder"
textBenchName = "Data.ByteString.Builder"

addCompare ∷ ([String] → Benchmark → Benchmark)
addCompare (name : path)
  | name /= textBenchName = bcompare (printAwkExpr (locateBenchmark (textBenchName : path)))
addCompare _ = id
