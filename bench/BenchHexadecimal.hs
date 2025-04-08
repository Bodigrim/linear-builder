-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>

module BenchHexadecimal (benchHexadecimal) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.Text as T
import Data.Text.Builder.Linear.Buffer
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (hexadecimal)
import Test.Tasty.Bench

#ifdef MIN_VERSION_text_builder
import qualified TextBuilder
#endif

word :: Word
word = 123456789123456789

benchLazyBuilder ∷ Word → T.Text
benchLazyBuilder = toStrict . toLazyText . go mempty
  where
    go !acc 0 = acc
    go !acc n = let i = n * word in go (hexadecimal i <> (acc <> hexadecimal i)) (n - 1)

benchLazyBuilderBS ∷ Word → B.ByteString
benchLazyBuilderBS = B.toStrict . B.toLazyByteString . go mempty
  where
    go !acc 0 = acc
    go !acc n = go (B.wordHex n <> (acc <> B.wordHex n)) (n - 1)

#ifdef MIN_VERSION_text_builder
benchStrictBuilder ∷ Word → T.Text
benchStrictBuilder = TextBuilder.toText . go mempty
  where
    go !acc 0 = acc
    go !acc n = let i = n * word in go (TextBuilder.hexadecimal i <> (acc <> TextBuilder.hexadecimal i)) (n - 1)
#endif

benchLinearBuilderWord ∷ Word → T.Text
benchLinearBuilderWord m = runBuffer (\b → go b m)
  where
    go ∷ Buffer ⊸ Word → Buffer
    go !acc 0 = acc
    go !acc n = let i = n * word in go (i &<| (acc |>& i)) (n - 1)

benchLinearBuilderInt ∷ Word → T.Text
benchLinearBuilderInt m = runBuffer (\b → go b (fromIntegral m))
  where
    go ∷ Buffer ⊸ Int → Buffer
    go !acc 0 = acc
    go !acc n = let i = n * fromIntegral word in go (i &<| (acc |>& i)) (n - 1)

benchHexadecimal ∷ Benchmark
benchHexadecimal = bgroup "Hexadecimal" $ map mkGroup [1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6]

mkGroup :: Word → Benchmark
mkGroup n = bgroup (show n)
  [ bench "Data.Text.Lazy.Builder" $ nf benchLazyBuilder n
  , bench "Data.ByteString.Builder" $ nf benchLazyBuilderBS n
#ifdef MIN_VERSION_text_builder
  , bench "TextBuilder" $ nf benchStrictBuilder n
#endif
  , bench "Data.Text.Builder.Linear (Word)" $ nf benchLinearBuilderWord n
  , bench "Data.Text.Builder.Linear (Int)" $ nf benchLinearBuilderInt n
  ]
