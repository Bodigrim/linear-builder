-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>

module Main where

import Test.Tasty.Bench
import Test.Tasty.Patterns.Printer

import BenchUnicode

main ∷ IO ()
main = defaultMain $ map (mapLeafBenchmarks addCompare) $
  [ benchUnicode
  ]

textBenchName :: String
textBenchName = "Data.Text.Lazy.Builder"

addCompare :: ([String] -> Benchmark -> Benchmark)
addCompare (name : path)
  | name /= textBenchName = bcompare (printAwkExpr (locateBenchmark (textBenchName : path)))
addCompare _ = id
