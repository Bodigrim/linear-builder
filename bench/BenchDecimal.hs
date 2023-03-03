-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>

module BenchDecimal (benchDecimal) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.Text as T
import Data.Text.Builder.Linear.Buffer
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import Test.Tasty.Bench

#ifdef MIN_VERSION_text_builder
import qualified Text.Builder
#endif

int :: Int
int = 123456789123456789

benchLazyBuilder ∷ Int → T.Text
benchLazyBuilder = toStrict . toLazyText . go mempty
  where
    go !acc 0 = acc
    go !acc n = let i = n * int in go (decimal i <> (acc <> decimal i)) (n - 1)

benchLazyBuilderBS ∷ Int → B.ByteString
benchLazyBuilderBS = B.toStrict . B.toLazyByteString . go mempty
  where
    go !acc 0 = acc
    go !acc n = let i = n * int in go (B.intDec i <> (acc <> B.intDec i)) (n - 1)

#ifdef MIN_VERSION_text_builder
benchStrictBuilder ∷ Int → T.Text
benchStrictBuilder = Text.Builder.run . go mempty
  where
    go !acc 0 = acc
    go !acc n = let i = n * int in go (Text.Builder.decimal i <> (acc <> Text.Builder.decimal i)) (n - 1)
#endif

benchLinearBuilder ∷ Int → T.Text
benchLinearBuilder m = runBuffer (\b → go b m)
  where
    go ∷ Buffer ⊸ Int → Buffer
    go !acc 0 = acc
    go !acc n = let i = n * int in go (i $<| (acc |>$ i)) (n - 1)

benchDecimal ∷ Benchmark
benchDecimal = bgroup "Decimal" $ map mkGroup [1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6]

mkGroup :: Int → Benchmark
mkGroup n = bgroup (show n)
  [ bench "Data.Text.Lazy.Builder" $ nf benchLazyBuilder n
  , bench "Data.ByteString.Builder" $ nf benchLazyBuilderBS n
#ifdef MIN_VERSION_text_builder
  , bench "Text.Builder" $ nf benchStrictBuilder n
#endif
  , bench "Data.Text.Builder.Linear" $ nf benchLinearBuilder n
  ]
