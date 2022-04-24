-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>

module Main where

import Data.Foldable (foldl')
import Test.Tasty.Bench
import Test.Tasty.Runners (TestTree(..))
import Test.Tasty.Patterns.Printer
import Test.Tasty.Patterns.Types as Pat

import BenchChar
import BenchDecimal
import BenchDouble
import BenchHexadecimal
import BenchText

main ∷ IO ()
main = defaultMain $ map (mapLeafs addCompare) $
  [ benchText
  , benchChar
  , benchDecimal
  , benchHexadecimal
  , benchDouble
  ]

textBenchName :: String
textBenchName = "Data.Text.Lazy.Builder"

addCompare :: ([String] -> Benchmark -> Benchmark)
addCompare (name : path)
  | name /= textBenchName = bcompare (printAwkExpr (locateLeaf (textBenchName : path)))
addCompare _ = id

mapLeafs :: ([String] -> Benchmark -> Benchmark) -> Benchmark -> Benchmark
mapLeafs processLeaf = go mempty
  where
    go :: [String] -> Benchmark -> Benchmark
    go path = \case
      SingleTest name t    -> processLeaf (name : path) (SingleTest name t)
      TestGroup name tts   -> TestGroup name (map (go (name : path)) tts)
      PlusTestOptions g tt -> PlusTestOptions g (go path tt)
      WithResource res f   -> WithResource res (go path . f)
      AskOptions f         -> AskOptions (go path . f)
      After dep expr tt    -> After dep expr (go path tt)

locateLeaf :: [String] -> Expr
locateLeaf path
  = foldl' And (IntLit 1)
  $ zipWith (\i name -> Pat.EQ (Field (Sub NF (IntLit i))) (StringLit name)) [0..] path
