-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
module BenchDecimal (benchDecimal) where

import Data.ByteString qualified as B
import Data.ByteString.Builder qualified as B
import Data.Text qualified as T
import Data.Text.Builder.Linear.Buffer (Buffer, runBuffer, ($$<|), ($<|), (|>$), (|>$$))
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

#ifdef MIN_VERSION_text_builder
import qualified TextBuilder
#endif

#ifdef MIN_VERSION_bytestring_strict_builder
import qualified ByteString.StrictBuilder
#endif

benchDecimal ∷ Benchmark
benchDecimal = bgroup "Decimal" [benchBoundedDecimal, benchUnboundedDecimal]

--------------------------------------------------------------------------------
-- Bounded
--------------------------------------------------------------------------------

int ∷ Int
int = 123456789123456789

benchLazyBuilder ∷ Integral a ⇒ a → Int → T.Text
benchLazyBuilder k = toStrict . toLazyText . go mempty
  where
    go !acc 0 = acc
    go !acc n = let i = fromIntegral n * k in go (decimal i <> (acc <> decimal i)) (n - 1)
{-# SPECIALIZE benchLazyBuilder ∷ Int → Int → T.Text #-}
{-# SPECIALIZE benchLazyBuilder ∷ Integer → Int → T.Text #-}

benchLazyBuilderBS ∷ Int → Int → B.ByteString
benchLazyBuilderBS k = B.toStrict . B.toLazyByteString . go mempty
  where
    go !acc 0 = acc
    go !acc n = let i = n * k in go (B.intDec i <> (acc <> B.intDec i)) (n - 1)

#ifdef MIN_VERSION_text_builder
benchStrictBuilder ∷ (Integral a) ⇒ a → Int → T.Text
benchStrictBuilder k = TextBuilder.toText . go mempty
  where
    go !acc 0 = acc
    go !acc n = let i = fromIntegral n * k in go (TextBuilder.decimal i <> (acc <> TextBuilder.decimal i)) (n - 1)
{-# SPECIALIZE benchStrictBuilder ∷ Int → Int → T.Text #-}
{-# SPECIALIZE benchStrictBuilder ∷ Integer → Int → T.Text #-}
#endif

#ifdef MIN_VERSION_bytestring_strict_builder
benchStrictBuilderBS ∷ (Integral a) ⇒ a  → Int → B.ByteString
benchStrictBuilderBS k = ByteString.StrictBuilder.builderBytes . go mempty
  where
    go !acc 0 = acc
    go !acc n = let i = fromIntegral n * k in go (ByteString.StrictBuilder.asciiIntegral i <> (acc <> ByteString.StrictBuilder.asciiIntegral i)) (n - 1)
{-# SPECIALIZE benchStrictBuilderBS ∷ Int → Int → B.ByteString #-}
{-# SPECIALIZE benchStrictBuilderBS ∷ Integer → Int → B.ByteString #-}
#endif

benchBoundedLinearBuilder ∷ Int → Int → T.Text
benchBoundedLinearBuilder k m = runBuffer (\b → go b m)
  where
    go ∷ Buffer ⊸ Int → Buffer
    go !acc 0 = acc
    go !acc n = let i = n * k in go (i $<| (acc |>$ i)) (n - 1)

benchBoundedDecimal ∷ Benchmark
benchBoundedDecimal = bgroup "Bounded" $ map mkBoundedGroup [1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6]

mkBoundedGroup ∷ Int → Benchmark
mkBoundedGroup n =
  bgroup
    (show n)
    [ bench "Data.Text.Lazy.Builder" $ nf (benchLazyBuilder int) n
    , bench "Data.ByteString.Builder" $ nf (benchLazyBuilderBS int) n
#ifdef MIN_VERSION_text_builder
    , bench "TextBuilder" $ nf (benchStrictBuilder int) n
#endif
#ifdef MIN_VERSION_bytestring_strict_builder
    , bench "ByteString.StrictBuilder" $ nf (benchStrictBuilderBS int) n
#endif
    , bench "Data.Text.Builder.Linear" $ nf (benchBoundedLinearBuilder int) n
    ]

--------------------------------------------------------------------------------
-- Unbounded
--------------------------------------------------------------------------------

integerSmall ∷ Integer
integerSmall = toInteger (div @Word maxBound 20)

integerBig ∷ Integer
integerBig = toInteger (maxBound @Word - 1) ^ (10 ∷ Word)

integerHuge ∷ Integer
integerHuge = toInteger (maxBound @Word - 1) ^ (100 ∷ Word)

benchUnboundedDecimal ∷ Benchmark
benchUnboundedDecimal =
  bgroup
    "Unbounded"
    [ bgroup "Small" $ map (mkUnboundedGroup integerSmall) [1e0, 1e1, 1e2, 1e3, 1e4, 1e5]
    , bgroup "Big" $ map (mkUnboundedGroup integerBig) [1e0, 1e1, 1e2, 1e3, 1e4]
    , bgroup "Huge" $ map (mkUnboundedGroup integerHuge) [1e0, 1e1, 1e2, 1e3]
    ]

-- NOTE: In the following benchmarks, the ByteString builder would share work
-- if the prepender and the appender are identical, while our linear buffer does
-- not. So we increment the appender to get a fair benchmark.

benchUnboundedLazyBuilderBS ∷ Integer → Int → B.ByteString
benchUnboundedLazyBuilderBS k = B.toStrict . B.toLazyByteString . go mempty
  where
    go !acc 0 = acc
    go !acc n = let i = fromIntegral n * k in go (B.integerDec i <> (acc <> B.integerDec (i + 1))) (n - 1)

benchUnboundedLinearBuilder ∷ Integer → Int → T.Text
benchUnboundedLinearBuilder k m = runBuffer (\b → go b m)
  where
    go ∷ Buffer ⊸ Int → Buffer
    go !acc 0 = acc
    go !acc n = let i = fromIntegral n * k in go (i $$<| (acc |>$$ (i + 1))) (n - 1)

mkUnboundedGroup ∷ Integer → Int → Benchmark
mkUnboundedGroup integer n =
  bgroup
    (show n)
    [ bench "Data.Text.Lazy.Builder" $ nf (benchLazyBuilder integer) n
    , bench "Data.ByteString.Builder" $ nf (benchUnboundedLazyBuilderBS integer) n
#ifdef MIN_VERSION_text_builder
    , bench "TextBuilder" $ nf (benchStrictBuilder integer) n
#endif
#ifdef MIN_VERSION_bytestring_strict_builder
    , bench "ByteString.StrictBuilder" $ nf (benchStrictBuilderBS integer) n
#endif
    , bench "Data.Text.Builder.Linear" $ nf (benchUnboundedLinearBuilder integer) n
    ]
