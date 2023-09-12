-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>

module BenchAsciiChar (benchAsciiChar) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.Text as T
import Data.Text.Internal.Unsafe.Char (unsafeChr8)
import Data.Text.Builder.Linear.Buffer
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy (toStrict)
import qualified Data.Text.Lazy.Builder as TB
import Data.Text.Lazy.Builder (toLazyText, singleton)
import qualified Data.Text.Internal.Fusion.Common as Fusion
import qualified Data.Text.Internal.Fusion as Fusion
import Data.Word (Word8)
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

benchLazyBuilder ∷ Word8 → T.Text
benchLazyBuilder = toStrict . toLazyText . go mempty
  where
    go !acc 0 = acc
    go !acc n = let ch = unsafeChr8 n in go (singleton ch <> (acc <> singleton ch)) (n - 1)

benchLazyBuilderBS ∷ Word8 → B.ByteString
benchLazyBuilderBS = B.toStrict . B.toLazyByteString . go mempty
  where
    go !acc 0 = acc
    go !acc n = go (B.word8 n <> (acc <> B.word8 n)) (n - 1)

#ifdef MIN_VERSION_text_builder
benchStrictBuilder ∷ Word8 → T.Text
benchStrictBuilder = Text.Builder.run . go mempty
  where
    go !acc 0 = acc
    go !acc n = go (Text.Builder.utf8CodeUnits1 n <> (acc <> Text.Builder.utf8CodeUnits1 n)) (n - 1)
#endif

#ifdef MIN_VERSION_bytestring_strict_builder
benchStrictBuilderBS ∷ Word8 → B.ByteString
benchStrictBuilderBS = ByteString.StrictBuilder.builderBytes . go mempty
  where
    go !acc 0 = acc
    go !acc n = go (ByteString.StrictBuilder.word8 n <> (acc <> ByteString.StrictBuilder.word8 n)) (n - 1)
#endif

benchLinearBuilder ∷ Word8 → T.Text
benchLinearBuilder m = runBuffer (\b → go b m)
  where
    go ∷ Buffer ⊸ Word8 → Buffer
    go !acc 0 = acc
    go !acc n = go (n @<| (acc |>@ n)) (n - 1)

benchSingleChar ∷ Benchmark
benchSingleChar = bgroup "Single" $ map mkGroupChar [1e0, 1e1, 1e2]

mkGroupChar :: Word8 → Benchmark
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

benchCharsLazyBuilder ∷ Word8 → T.Text
benchCharsLazyBuilder = TL.toStrict . TB.toLazyText . go mempty
  where
    go !acc 0 = acc
    go !acc n = let ch = unsafeChr8 n in go (replicateChar ch <> (acc <> replicateChar ch)) (n - 1)

    replicateChar ch = TB.fromText (Fusion.unstream (Fusion.replicateCharI charCount ch))

benchCharsLazyBuilderBS ∷ Word8 → B.ByteString
benchCharsLazyBuilderBS = B.toStrict . B.toLazyByteString . go mempty
  where
    go !acc 0 = acc
    go !acc n = go (replicateChar n <> (acc <> replicateChar n)) (n - 1)

    replicateChar ch = B.byteString (B.replicate (fromIntegral charCount) ch)

{- [TODO]
#ifdef MIN_VERSION_text_builder
benchCharsStrictBuilder ∷ Word8 → T.Text
benchCharsStrictBuilder = Text.Builder.run . go mempty
  where
    go !acc 0 = acc
    go !acc n = let ch = chr n in go (replicateChar ch <> (acc <> replicateChar ch)) (n - 1)

    -- [TODO] Is there a better way?
    replicateChar ch = Text.Builder.padFromRight (fromIntegral charCount) ch mempty
#endif

#ifdef MIN_VERSION_bytestring_strict_builder
benchCharsStrictBuilderBS ∷ Word8 → B.ByteString
benchCharsStrictBuilderBS = ByteString.StrictBuilder.builderBytes . go mempty
  where
    go !acc 0 = acc
    go !acc n = let ch = chr n in go _ (n - 1)
#endif
-}

benchCharsLinearBuilder ∷ Word8 → T.Text
benchCharsLinearBuilder m = runBuffer (\b → go b m)
  where
    go ∷ Buffer ⊸ Word8 → Buffer
    go !acc 0 = acc
    go !acc n = go (prependAsciiChars charCount n (appendAsciiChars charCount n acc)) (n - 1)

benchMultipleChars ∷ Benchmark
benchMultipleChars = bgroup "Multiple" $ map mkGroupChars [1e0, 1e1, 1e2]

mkGroupChars :: Word8 → Benchmark
mkGroupChars n = bgroup (show n)
  [ bench "Data.Text.Lazy.Builder" $ nf benchCharsLazyBuilder n
  , bench "Data.ByteString.Builder" $ nf benchCharsLazyBuilderBS n
-- #ifdef MIN_VERSION_text_builder
--   , bench "Text.Builder" $ nf benchCharsStrictBuilder n
-- #endif
-- #ifdef MIN_VERSION_bytestring_strict_builder
--   , bench "ByteString.StrictBuilder" $ nf benchCharsStrictBuilderBS n
-- #endif
  , bench "Data.Text.Builder.Linear" $ nf benchCharsLinearBuilder n
  ]

--------------------------------------------------------------------------------
-- Padding
--------------------------------------------------------------------------------

benchPaddingLazyBuilder ∷ Word8 → T.Text
benchPaddingLazyBuilder = toStrict . toLazyText . go mempty 0
  where
    go !acc !_ 0 = acc
    go !acc l  n =
      let ch = unsafeChr8 n
          !l' = l + 2 * fromIntegral charCount
      in go (withText (T.justifyLeft l' ch)
                      (withText (T.justifyRight (l + fromIntegral charCount) ch) acc))
            l'
            (n - 1)

    withText f = TB.fromText . f . TL.toStrict . TB.toLazyText

{- [TODO]
benchPaddingLazyBuilderBS ∷ Word8 → B.ByteString
benchPaddingLazyBuilderBS = B.toStrict . B.toLazyByteString . go mempty
  where
    go !acc 0 = acc
    go !acc n = let ch = chr n in go _ (n - 1)
-}

#ifdef MIN_VERSION_text_builder
benchPaddingStrictBuilder ∷ Word8 → T.Text
benchPaddingStrictBuilder = Text.Builder.run . go mempty 0
  where
    go !acc !_ 0 = acc
    go !acc l  n =
      let ch = unsafeChr8 n
          !l' = l + 2 * fromIntegral charCount
      in go (Text.Builder.padFromRight l' ch (Text.Builder.padFromLeft (l + fromIntegral charCount) ch acc))
            l'
            (n - 1)
#endif

{- [TODO]
#ifdef MIN_VERSION_bytestring_strict_builder
benchPaddingStrictBuilderBS ∷ Word8 → B.ByteString
benchPaddingStrictBuilderBS = ByteString.StrictBuilder.builderBytes . go mempty
  where
    go !acc 0 = acc
    go !acc n = let ch = chr n in go _ (n - 1)
#endif
-}

benchPaddingLinearBuilder ∷ Word8 → T.Text
benchPaddingLinearBuilder m = runBuffer (\b → go b 0 m)
  where
    go ∷ Buffer ⊸ Word → Word8 → Buffer
    go !acc !_ 0 = acc
    go !acc l  n =
      let !l' = l + 2 * charCount
      in go (justifyLeftAscii l' n (justifyRightAscii (l + charCount) n acc))
            l'
            (n - 1)

benchPadding ∷ Benchmark
benchPadding = bgroup "Padding" $ map mkGroupPadding [1e0, 1e1, 1e2]

mkGroupPadding :: Word8 → Benchmark
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

benchAsciiChar ∷ Benchmark
benchAsciiChar = bgroup "ASCII Char"
  [ benchSingleChar
  , benchMultipleChars
  , benchPadding ]

