{-# LANGUAGE NumDecimals #-}

-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
module BenchDecimalUnbounded (benchDecimalUnbounded) where

import Data.ByteString qualified as B
import Data.ByteString.Builder qualified as B
import Data.Text qualified as T
import Data.Text.Builder.Linear.Buffer (Buffer, runBuffer, ($$<|), (|>$$))
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder.Int qualified as TB
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

benchUnboundedLinearBuilderAppend ∷ Integer → Int → T.Text
benchUnboundedLinearBuilderAppend k m = runBuffer (`go` m)
  where
    go ∷ Buffer ⊸ Int → Buffer
    go !acc 0 = acc
    go !acc n = let i = fromIntegral n * k in go (acc |>$$ i) (n - 1)

benchUnboundedLinearBuilderPrepend ∷ Integer → Int → T.Text
benchUnboundedLinearBuilderPrepend k m = runBuffer (`go` m)
  where
    go ∷ Buffer ⊸ Int → Buffer
    go !acc 0 = acc
    go !acc n = let i = fromIntegral n * k in go (i $$<| acc) (n - 1)

-- NOTE: In the following benchmark, the ByteString builder would share work
-- if the prepender and the appender are identical, while our linear buffer does
-- not. So we increment the appender to get a fair benchmark.

benchUnboundedLinearBuilder ∷ Integer → Int → T.Text
benchUnboundedLinearBuilder k m = runBuffer (`go` m)
  where
    go ∷ Buffer ⊸ Int → Buffer
    go !acc 0 = acc
    go !acc n = let i = fromIntegral n * k in go (i $$<| (acc |>$$ (i + 1))) (n - 1)

benchUnboundedLazyBuilderBSAppend ∷ Integer → Int → B.ByteString
benchUnboundedLazyBuilderBSAppend k = B.toStrict . B.toLazyByteString . go mempty
  where
    go !acc 0 = acc
    go !acc n = let i = fromIntegral n * k in go (acc <> B.integerDec i) (n - 1)

benchUnboundedLazyBuilderBSPrepend ∷ Integer → Int → B.ByteString
benchUnboundedLazyBuilderBSPrepend k = B.toStrict . B.toLazyByteString . go mempty
  where
    go !acc 0 = acc
    go !acc n = let i = fromIntegral n * k in go (B.integerDec i <> acc) (n - 1)

benchUnboundedLazyBuilderBS ∷ Integer → Int → B.ByteString
benchUnboundedLazyBuilderBS k = B.toStrict . B.toLazyByteString . go mempty
  where
    go !acc 0 = acc
    go !acc n = let i = fromIntegral n * k in go (B.integerDec i <> (acc <> B.integerDec (i + 1))) (n - 1)

benchLazyBuilderAppend ∷ Integer → Int → T.Text
benchLazyBuilderAppend k = TL.toStrict . TB.toLazyText . go mempty
  where
    go !acc 0 = acc
    go !acc n = let i = fromIntegral n * k in go (acc <> TB.decimal i) (n - 1)

benchLazyBuilderPrepend ∷ Integer → Int → T.Text
benchLazyBuilderPrepend k = TL.toStrict . TB.toLazyText . go mempty
  where
    go !acc 0 = acc
    go !acc n = let i = fromIntegral n * k in go (TB.decimal i <> acc) (n - 1)

benchLazyBuilder ∷ Integer → Int → T.Text
benchLazyBuilder k = TL.toStrict . TB.toLazyText . go mempty
  where
    go !acc 0 = acc
    go !acc n = let i = fromIntegral n * k in go (TB.decimal i <> (acc <> TB.decimal (i + 1))) (n - 1)

data NamedInteger = I !String !Integer

mkGroup
  ∷ String
  → [Int]
  → (Integer → Int → T.Text)
  → (Integer → Int → B.ByteString)
  → (Integer → Int → T.Text)
  → [NamedInteger]
  → Benchmark
mkGroup name counts f g h = bgroup name . map mkBenches
  where
    mkBenches (I benchName i) =
      bgroup
        benchName
        (map (\count → bgroup (show count) (mkBench i count)) counts)
    mkBench i count =
      [ bench "Data.Text.Lazy.Builder" $ nf (f i) count
      , bench "Data.ByteString.Builder" $ nf (g i) count
      , bench "Data.Text.Builder.Linear" $ nf (h i) count
      ]
{-# INLINE mkGroup #-}

integers ∷ [NamedInteger]
integers =
  [ I "Small" (toInteger (div @Word maxBound 20)) -- ~ 9e17
  , I "Big01" (toInteger (maxBound @Word - 1) ^ (2 ∷ Word)) -- ~3e38
  , I "Big02" (toInteger (maxBound @Word - 1) ^ (5 ∷ Word)) -- ~2e96
  , I "Big03" (toInteger (maxBound @Word - 1) ^ (10 ∷ Word)) -- ~5e192
  , I "Big04" (toInteger (maxBound @Word - 1) ^ (15 ∷ Word)) -- ~1e289
  , I "Big05" (toInteger (maxBound @Word - 1) ^ (20 ∷ Word)) -- ~2e385
  -- , I "Big05a" (toInteger (maxBound @Word - 1) ^ (21 ∷ Word)) -- ~4e404
  -- , I "Big05b" (toInteger (maxBound @Word - 1) ^ (22 ∷ Word)) -- ~7e423
  -- , I "Big05c" (toInteger (maxBound @Word - 1) ^ (23 ∷ Word)) -- ~1e443
  -- , I "Big05d" (toInteger (maxBound @Word - 1) ^ (24 ∷ Word)) -- ~2e462
  , I "Big06" (toInteger (maxBound @Word - 1) ^ (25 ∷ Word)) -- ~4e481
  -- , I "Big06a" (toInteger (maxBound @Word - 1) ^ (26 ∷ Word)) -- ~8e500
  -- , I "Big06b" (toInteger (maxBound @Word - 1) ^ (27 ∷ Word)) -- ~2e520
  -- , I "Big06c" (toInteger (maxBound @Word - 1) ^ (28 ∷ Word))
  -- , I "Big06d" (toInteger (maxBound @Word - 1) ^ (29 ∷ Word))
  , I "Big07" (toInteger (maxBound @Word - 1) ^ (30 ∷ Word)) -- ~ 9e577
  , I "Big08" (toInteger (maxBound @Word - 1) ^ (35 ∷ Word)) -- ~ 2e674
  , I "Big09" (toInteger (maxBound @Word - 1) ^ (40 ∷ Word)) -- ~ 4e770
  , I "Big10" (toInteger (maxBound @Word - 1) ^ (45 ∷ Word)) -- ~ 9e866
  , I "Big11" (toInteger (maxBound @Word - 1) ^ (50 ∷ Word)) -- ~ 2e963
  , I "Huge01" (toInteger (maxBound @Word - 1) ^ (75 ∷ Word)) -- ~9e1444
  , I "Huge02" (toInteger (maxBound @Word - 1) ^ (100 ∷ Word)) -- ~4e1926
  , I "Huge03" (toInteger (maxBound @Word - 1) ^ (200 ∷ Word)) -- ~2e3853
  , I "Huge04" (toInteger (maxBound @Word - 1) ^ (300 ∷ Word)) -- ~6e5779
  , I "Huge05" (toInteger (maxBound @Word - 1) ^ (400 ∷ Word)) -- ~2e7706
  -- , I "Huge05a" (toInteger (maxBound @Word - 1) ^ (450 ∷ Word))
  , I "Huge06" (toInteger (maxBound @Word - 1) ^ (500 ∷ Word)) -- ~9e9632
  -- , I "Huge06b" (toInteger (maxBound @Word - 1) ^ (600 ∷ Word))
  , I "Huge07" (toInteger (maxBound @Word - 1) ^ (700 ∷ Word)) -- ~1e13486
  , I "Huge08" (toInteger (maxBound @Word - 1) ^ (1000 ∷ Word)) -- ~8e19265
  , I "Huge09" (toInteger (maxBound @Word - 1) ^ (3000 ∷ Word)) -- ~6e57797
  , I "Huge10" (toInteger (maxBound @Word - 1) ^ (5000 ∷ Word)) -- ~4e96329
  , I "Huge11" (toInteger (maxBound @Word - 1) ^ (10000 ∷ Word)) -- ~2e192659
  , I "Huge12" (toInteger (maxBound @Word - 1) ^ (100000 ∷ Word)) -- ~9e1926591
  -- , I "Huge13" (toInteger (maxBound @Word - 1) ^ (1000000 ∷ Word))
  , I "1e20" 1e20
  , I "1e100" 1e100
  , I "1e300" (10 ^ (300 ∷ Word))
  , I "1e500" (10 ^ (500 ∷ Word))
  , I "1e1000" (10 ^ (1000 ∷ Word))
  ]

benchDecimalUnbounded ∷ Benchmark
benchDecimalUnbounded =
  bgroup
    "Decimal: detailed unbounded"
    [ mkGroup
        "Append"
        counts
        benchLazyBuilderAppend
        benchUnboundedLazyBuilderBSAppend
        benchUnboundedLinearBuilderAppend
        integers
    , mkGroup
        "Prepend"
        counts
        benchLazyBuilderPrepend
        benchUnboundedLazyBuilderBSPrepend
        benchUnboundedLinearBuilderPrepend
        integers
    , mkGroup
        "Both"
        counts
        benchLazyBuilder
        benchUnboundedLazyBuilderBS
        benchUnboundedLinearBuilder
        integers
    ]
  where
    counts ∷ [Int]
    counts = [1e0, 1e1, 1e2]
