-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>

module BenchChar (benchChar) where

import Data.Char
import qualified Data.Text as T
import Data.Text.Builder.Linear.Buffer
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText, singleton)
import Test.Tasty.Bench

#ifdef MIN_VERSION_text_builder
import qualified Text.Builder
#endif

benchLazyBuilder ∷ Int → T.Text
benchLazyBuilder = toStrict . toLazyText . go mempty
  where
    go !acc 0 = acc
    go !acc n = let ch = chr n in go (singleton ch <> (acc <> singleton ch)) (n - 1)

#ifdef MIN_VERSION_text_builder
benchStrictBuilder ∷ Int → T.Text
benchStrictBuilder = Text.Builder.run . go mempty
  where
    go !acc 0 = acc
    go !acc n = let ch = chr n in go (Text.Builder.char ch <> (acc <> Text.Builder.char ch)) (n - 1)
#endif

benchLinearBuilder ∷ Int → T.Text
benchLinearBuilder m = runBuffer (\b → go b m)
  where
    go ∷ Buffer ⊸ Int → Buffer
    go !acc 0 = acc
    go !acc n = let ch = chr n in go (ch .<| (acc |>. ch)) (n - 1)

benchChar ∷ Benchmark
benchChar = bgroup "Char" $ map mkGroup [1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6]

mkGroup :: Int → Benchmark
mkGroup n = bgroup (show n)
  [ bench "Data.Text.Lazy.Builder" $ nf benchLazyBuilder n
#ifdef MIN_VERSION_text_builder
  , bcompare ("$NF == \"Data.Text.Lazy.Builder\" && $(NF-1) == \"" ++ show n ++ "\" && $(NF-2) == \"Char\"")
  $ bench "Text.Builder" $ nf benchStrictBuilder n
#endif
  , bcompare ("$NF == \"Data.Text.Lazy.Builder\" && $(NF-1) == \"" ++ show n ++ "\" && $(NF-2) == \"Char\"")
  $ bench "Data.Text.Builder.Linear" $ nf benchLinearBuilder n
  ]
