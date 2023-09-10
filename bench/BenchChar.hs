-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>

module BenchChar (benchChar) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import Data.Char
import qualified Data.Text as T
import Data.Text.Builder.Linear.Buffer
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy (toStrict)
import qualified Data.Text.Lazy.Builder as TB
import Data.Text.Lazy.Builder (toLazyText, singleton)
import qualified Data.Text.Internal.Fusion.Common as Fusion
import qualified Data.Text.Internal.Fusion as Fusion
import Test.Tasty.Bench

#ifdef MIN_VERSION_text_builder
import qualified Text.Builder
#endif

#ifdef MIN_VERSION_bytestring_strict_builder
import qualified ByteString.StrictBuilder
#endif

--------------------------------------------------------------------------------
-- Single char
--------------------------------------------------------------------------------

benchLazyBuilder ∷ Int → T.Text
benchLazyBuilder = toStrict . toLazyText . go mempty
  where
    go !acc 0 = acc
    go !acc n = let ch = chr n in go (singleton ch <> (acc <> singleton ch)) (n - 1)

benchLazyBuilderBS ∷ Int → B.ByteString
benchLazyBuilderBS = B.toStrict . B.toLazyByteString . go mempty
  where
    go !acc 0 = acc
    go !acc n = let ch = chr n in go (B.charUtf8 ch <> (acc <> B.charUtf8 ch)) (n - 1)

#ifdef MIN_VERSION_text_builder
benchStrictBuilder ∷ Int → T.Text
benchStrictBuilder = Text.Builder.run . go mempty
  where
    go !acc 0 = acc
    go !acc n = let ch = chr n in go (Text.Builder.char ch <> (acc <> Text.Builder.char ch)) (n - 1)
#endif

#ifdef MIN_VERSION_bytestring_strict_builder
benchStrictBuilderBS ∷ Int → B.ByteString
benchStrictBuilderBS = ByteString.StrictBuilder.builderBytes . go mempty
  where
    go !acc 0 = acc
    go !acc n = let ch = chr n in go (ByteString.StrictBuilder.utf8Char ch <> (acc <> ByteString.StrictBuilder.utf8Char ch)) (n - 1)
#endif

benchLinearBuilder ∷ Int → T.Text
benchLinearBuilder m = runBuffer (\b → go b m)
  where
    go ∷ Buffer ⊸ Int → Buffer
    go !acc 0 = acc
    go !acc n = let ch = chr n in go (ch .<| (acc |>. ch)) (n - 1)

benchSingleChar ∷ Benchmark
benchSingleChar = bgroup "Single" $ map mkGroupChar [1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6]

mkGroupChar :: Int → Benchmark
mkGroupChar n = bgroup (show n)
  [ bench "Data.Text.Lazy.Builder" $ nf benchLazyBuilder n
  , bench "Data.ByteString.Builder" $ nf benchLazyBuilderBS n
#ifdef MIN_VERSION_text_builder
  , bench "Text.Builder" $ nf benchStrictBuilder n
#endif
#ifdef MIN_VERSION_bytestring_strict_builder
  , bench "ByteString.StrictBuilder" $ nf benchStrictBuilderBS n
#endif
  , bench "Data.Text.Builder.Linear" $ nf benchLinearBuilder n
  ]

--------------------------------------------------------------------------------
-- Multiple chars
--------------------------------------------------------------------------------

charCount :: Word
charCount = 3

benchCharsLazyBuilder ∷ Int → T.Text
benchCharsLazyBuilder = TL.toStrict . TB.toLazyText . go mempty
  where
    go !acc 0 = acc
    go !acc n = let ch = chr n in go (replicateChar ch <> (acc <> replicateChar ch)) (n - 1)

    replicateChar ch = TB.fromText (Fusion.unstream (Fusion.replicateCharI charCount ch))

{- [FIXME] bad performance
benchCharsLazyBuilderBS ∷ Int → B.ByteString
benchCharsLazyBuilderBS = B.toStrict . B.toLazyByteString . go mempty
  where
    go !acc 0 = acc
    go !acc n =
      let ch = chr n
      in go (replicateChar ch <> (acc <> replicateChar ch)) (n - 1)

    replicateChar ch = stimes charCount (B.charUtf8 ch)
-}

#ifdef MIN_VERSION_text_builder
benchCharsStrictBuilder ∷ Int → T.Text
benchCharsStrictBuilder = Text.Builder.run . go mempty
  where
    go !acc 0 = acc
    go !acc n = let ch = chr n in go (replicateChar ch <> (acc <> replicateChar ch)) (n - 1)

    -- [TODO] Is there a better way?
    replicateChar ch = Text.Builder.padFromRight (fromIntegral charCount) ch mempty
#endif

{- [TODO]
#ifdef MIN_VERSION_bytestring_strict_builder
benchCharsStrictBuilderBS ∷ Int → B.ByteString
benchCharsStrictBuilderBS = ByteString.StrictBuilder.builderBytes . go mempty
  where
    go !acc 0 = acc
    go !acc n = let ch = chr n in go _ (n - 1)
#endif
-}

benchCharsLinearBuilder ∷ Int → T.Text
benchCharsLinearBuilder m = runBuffer (\b → go b m)
  where
    go ∷ Buffer ⊸ Int → Buffer
    go !acc 0 = acc
    go !acc n = let ch = chr n in go (prependChars charCount ch (appendChars charCount ch acc)) (n - 1)

benchMultipleChars ∷ Benchmark
benchMultipleChars = bgroup "Multiple" $ map mkGroupChars [1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6]

mkGroupChars :: Int → Benchmark
mkGroupChars n = bgroup (show n)
  [ bench "Data.Text.Lazy.Builder" $ nf benchCharsLazyBuilder n
  -- , bench "Data.ByteString.Builder" $ nf benchCharsLazyBuilderBS n
#ifdef MIN_VERSION_text_builder
  , bench "Text.Builder" $ nf benchCharsStrictBuilder n
#endif
-- #ifdef MIN_VERSION_bytestring_strict_builder
--   , bench "ByteString.StrictBuilder" $ nf benchCharsStrictBuilderBS n
-- #endif
  , bench "Data.Text.Builder.Linear" $ nf benchCharsLinearBuilder n
  ]

--------------------------------------------------------------------------------
-- Padding
--------------------------------------------------------------------------------

benchPaddingLazyBuilder ∷ Int → T.Text
benchPaddingLazyBuilder = toStrict . toLazyText . go mempty 0
  where
    go !acc !_ 0 = acc
    go !acc l  n =
      let ch = chr n
          !l' = l + 2 * fromIntegral charCount
      in go (withText (T.justifyLeft l' ch)
                      (withText (T.justifyRight (l + fromIntegral charCount) ch) acc))
            l'
            (n - 1)

    withText f = TB.fromText . f . TL.toStrict . TB.toLazyText

{- [TODO]
benchPaddingLazyBuilderBS ∷ Int → B.ByteString
benchPaddingLazyBuilderBS = B.toStrict . B.toLazyByteString . go mempty
  where
    go !acc 0 = acc
    go !acc n = let ch = chr n in go _ (n - 1)
-}

#ifdef MIN_VERSION_text_builder
benchPaddingStrictBuilder ∷ Int → T.Text
benchPaddingStrictBuilder = Text.Builder.run . go mempty 0
  where
    go !acc !_ 0 = acc
    go !acc l  n =
      let ch = chr n
          !l' = l + 2 * fromIntegral charCount
      in go (Text.Builder.padFromRight l' ch (Text.Builder.padFromLeft (l + fromIntegral charCount) ch acc))
            l'
            (n - 1)
#endif

{- [TODO]
#ifdef MIN_VERSION_bytestring_strict_builder
benchPaddingStrictBuilderBS ∷ Int → B.ByteString
benchPaddingStrictBuilderBS = ByteString.StrictBuilder.builderBytes . go mempty
  where
    go !acc 0 = acc
    go !acc n = let ch = chr n in go _ (n - 1)
#endif
-}

benchPaddingLinearBuilder ∷ Int → T.Text
benchPaddingLinearBuilder m = runBuffer (\b → go b 0 m)
  where
    go ∷ Buffer ⊸ Word → Int → Buffer
    go !acc !_ 0 = acc
    go !acc l  n =
      let ch = chr n
          !l' = l + 2 * charCount
      in go (justifyLeft l' ch (justifyRight (l + charCount) ch acc))
            l'
            (n - 1)

benchPadding ∷ Benchmark
benchPadding = bgroup "Padding" $ map mkGroupPadding [1e0, 1e1, 1e2, 1e3, 1e4{-, 1e5, 1e6-}] -- NOTE: too long with 1e5

mkGroupPadding :: Int → Benchmark
mkGroupPadding n = bgroup (show n)
  [ bench "Data.Text.Lazy.Builder" $ nf benchPaddingLazyBuilder n
  -- , bench "Data.ByteString.Builder" $ nf benchPaddingLazyBuilderBS n
#ifdef MIN_VERSION_text_builder
  , bench "Text.Builder" $ nf benchPaddingStrictBuilder n
#endif
-- #ifdef MIN_VERSION_bytestring_strict_builder
--   , bench "ByteString.StrictBuilder" $ nf benchPaddingStrictBuilderBS n
-- #endif
  , bench "Data.Text.Builder.Linear" $ nf benchPaddingLinearBuilder n
  ]

--------------------------------------------------------------------------------
-- All benchmarks
--------------------------------------------------------------------------------

benchChar ∷ Benchmark
benchChar = bgroup "Char"
  [ benchSingleChar
  , benchMultipleChars
  , benchPadding ]

