{-# LANGUAGE UnboxedTuples #-}

-- |
-- Copyright:   (c) 2023 Pierre Le Marre
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
module BenchUnicode (benchUnicode) where

import Data.Char (chr, ord)
import Data.Text qualified as T
import Data.Text.Builder.Linear.Buffer
import Examples.Unicode ((|>&&))
import Test.Tasty.Bench

benchLinearBuilder ∷ Word → T.Text
benchLinearBuilder m = runBuffer (\b → go b (fromIntegral m))
  where
    go ∷ Buffer ⊸ Int → Buffer
    go !acc 0 = acc
    go !acc n = let ch = chr (fromIntegral n) in go (appendCodePoint acc ch) (n - 1)

    -- NOTE: for the sake of simplicity, we use the built-in (|>&) operator.
    --       This operator produces _lower_ case hexadecimal, while we really
    --       want _upper_ case here, but there is no public API for it, although
    --       it would perform equally if there were such function.
    appendCodePoint ∷ Buffer ⊸ Char → Buffer
    appendCodePoint b ch = case newEmptyBuffer b of
      (# b', empty #) → b' >< justifyRight 4 '0' (empty |>& ord ch)

benchLinearBuilderOpt ∷ Word → T.Text
benchLinearBuilderOpt m = runBuffer (\b → go b (fromIntegral m))
  where
    go ∷ Buffer ⊸ Int → Buffer
    go !acc 0 = acc
    go !acc n = let ch = chr (fromIntegral n) in go (acc |>&& ch) (n - 1)

benchUnicode ∷ Benchmark
benchUnicode = bgroup "Unicode code point" $ map mkGroup [1e0, 1e1, 1e2, 1e3, 1e4, 1e5]

mkGroup ∷ Word → Benchmark
mkGroup n =
  bgroup
    (show n)
    [ bench "Data.Text.Builder.Linear" $ nf benchLinearBuilder n
    , bench "Data.Text.Builder.Linear (optimized)" $ nf benchLinearBuilderOpt n
    ]
