-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- 'Buffer' for strict 'Text', based on linear types.
-- It's consistently outperforms
-- 'Data.Text.Lazy.toStrict' . 'Data.Text.Lazy.Builder.toLazyText'
-- and scales better:
--
-- @
-- 1
--   Data.Text.Lazy.Builder:
--     73.9 ns ± 6.8 ns
--   Data.Text.Builder.Linear:
--     33.0 ns ± 2.2 ns, 0.45x
-- 10
--   Data.Text.Lazy.Builder:
--     702  ns ±  70 ns
--   Data.Text.Builder.Linear:
--     178  ns ±  16 ns, 0.25x
-- 100
--   Data.Text.Lazy.Builder:
--     6.57 μs ± 292 ns
--   Data.Text.Builder.Linear:
--     1.58 μs ± 116 ns, 0.24x
-- 1000
--   Data.Text.Lazy.Builder:
--     74.7 μs ± 2.9 μs
--   Data.Text.Builder.Linear:
--     13.9 μs ± 1.2 μs, 0.19x
-- 10000
--   Data.Text.Lazy.Builder:
--     1.63 ms ±  64 μs
--   Data.Text.Builder.Linear:
--     220  μs ±  16 μs, 0.13x
-- 100000
--   Data.Text.Lazy.Builder:
--     26.2 ms ± 2.5 ms
--   Data.Text.Builder.Linear:
--     3.25 ms ± 294 μs, 0.12x
-- 1000000
--   Data.Text.Lazy.Builder:
--     302  ms ± 8.9 ms
--   Data.Text.Builder.Linear:
--     30.8 ms ± 3.1 ms, 0.10x
-- @

module Data.Text.Builder.Linear
  ( -- * Buffer
    Buffer
  , runBuffer
  , dupBuffer
  , (|>)
  , (|>.)
  , (|>#)
  , (<|)
  , (.<|)
  , (<|#)
  , (><)
  , (|>$)
  , ($<|)
  , (|>%)
  , (%<|)
  , (|>&)
  , (&<|)
    -- * Builder
  , Builder(..)
  , runBuilder
  , fromText
  , fromChar
  , fromAddr
  , fromDec
  , fromHex
  , fromDouble
  ) where

import Data.Bits
import qualified Data.Text.Array as A
import Data.Text.Internal (Text(..))
import GHC.Exts

import Data.Text.Builder.Linear.Char
import Data.Text.Builder.Linear.Core
import Data.Text.Builder.Linear.Dec
import Data.Text.Builder.Linear.Double
import Data.Text.Builder.Linear.Hex

-- | Thin wrapper over 'Buffer' with a handy 'Semigroup' instance.
--
-- >>> :set -XOverloadedStrings -XMagicHash
-- >>> fromText "foo" <> fromChar '_' <> fromAddr "bar"#
-- "foo_bar"
--
newtype Builder = Builder { unBuilder :: Buffer ⊸ Buffer }

-- | Run 'Builder' computation on an empty 'Buffer', returning 'Text'.
--
-- >>> :set -XOverloadedStrings -XMagicHash
-- >>> runBuilder (fromText "foo" <> fromChar '_' <> fromAddr "bar"#)
-- "foo_bar"
--
-- Be careful to write @runBuilder (\b -> ...)@ instead of @runBuilder $ \b -> ...@,
-- because current implementation of linear types lacks special support for '($)'.
-- Alternatively, you can import @Prelude.Linear.($)@ from @linear-base@.
--
runBuilder :: Builder ⊸ Text
runBuilder (Builder f) = runBuffer f
{-# INLINE runBuilder #-}

instance Show Builder where
  show (Builder f) = show (runBuffer f)

instance Semigroup Builder where
  Builder f <> Builder g = Builder $ \b -> g (f b)
  {-# INLINE (<>) #-}

instance Monoid Builder where
  mempty = Builder (\b -> b)
  {-# INLINE mempty #-}

instance IsString Builder where
  fromString = fromText . fromString
  {-# INLINE fromString #-}

-- | Create 'Builder', containing a given 'Text'.
--
-- >>> :set -XOverloadedStrings
-- >>> fromText "foo" <> fromText "bar"
-- "foobar"
--
fromText :: Text -> Builder
fromText x = Builder $ \b -> b |> x
{-# INLINE fromText #-}

-- | Create 'Builder', containing a given 'Char'.
--
-- >>> fromChar 'x' <> fromChar 'y'
-- "xy"
--
fromChar :: Char -> Builder
fromChar x = Builder $ \b -> b |>. x
{-# INLINE fromChar #-}

-- | Create 'Builder', containing a null-terminated UTF-8 string, specified by 'Addr#'.
--
-- >>> :set -XMagicHash
-- >>> fromAddr "foo"# <> fromAddr "bar"#
-- "foobar"
--
fromAddr :: Addr# -> Builder
fromAddr x = Builder $ \b -> b |># x
{-# INLINE fromAddr #-}

-- | Create 'Builder', containing decimal representation of a given number.
--
-- >>> fromChar 'x' <> fromDec (123 :: Int)
-- "x123"
--
fromDec :: (Integral a, FiniteBits a) => a -> Builder
fromDec x = Builder $ \b -> b |>$ x
{-# INLINE fromDec #-}

-- | Create 'Builder', containing hexadecimal representation of a given number.
--
-- >>> :set -XMagicHash
-- >>> fromAddr "0x"# <> fromHex (0x123def :: Int)
-- "0x123def"
--
fromHex :: (Integral a, FiniteBits a) => a -> Builder
fromHex x = Builder $ \b -> b |>& x
{-# INLINE fromHex #-}

-- | Create 'Builder', containing a given 'Double'.
--
-- >>> :set -XMagicHash
-- >>> fromAddr "pi="# <> fromDouble pi
-- "pi=3.141592653589793"
--
fromDouble :: Double -> Builder
fromDouble x = Builder $ \b -> b |>% x
{-# INLINE fromDouble #-}

-- | Append 'Text' suffix to a 'Buffer' by mutating it.
-- If a suffix is statically known, consider using '(|>#)' for optimal performance.
--
-- >>> :set -XOverloadedStrings -XLinearTypes
-- >>> runBuffer (\b -> b |> "foo" |> "bar")
-- "foobar"
--
(|>) ∷ Buffer ⊸ Text → Buffer
infixl 6 |>
buffer |> (Text src srcOff srcLen) = appendExact
  srcLen
  (\dst dstOff -> A.copyI srcLen dst dstOff src srcOff)
  buffer

-- | Prepend 'Text' prefix to a 'Buffer' by mutating it.
-- If a prefix is statically known, consider using '(<|#)' for optimal performance.
--
-- >>> :set -XOverloadedStrings -XLinearTypes
-- >>> runBuffer (\b -> "foo" <| "bar" <| b)
-- "foobar"
--
(<|) ∷ Text → Buffer ⊸ Buffer
infixr 6 <|
Text src srcOff srcLen <| buffer = prependExact
  srcLen
  (\dst dstOff -> A.copyI srcLen dst dstOff src srcOff)
  buffer

-- | Append a null-terminated UTF-8 string
-- to a 'Buffer' by mutating it. E. g.,
--
-- >>> :set -XOverloadedStrings -XLinearTypes -XMagicHash
-- >>> runBuffer (\b -> b |># "foo"# |># "bar"#)
-- "foobar"
--
-- The literal string must not contain zero bytes @\\0@, this condition is not checked.
(|>#) ∷ Buffer ⊸ Addr# → Buffer
infixl 6 |>#
buffer |># addr# = appendExact
  srcLen
  (\dst dstOff -> A.copyFromPointer dst dstOff (Ptr addr#) srcLen)
  buffer
  where
    srcLen = I# (cstringLength# addr#)

-- | Prepend a null-terminated UTF-8 string
-- to a 'Buffer' by mutating it. E. g.,
--
-- >>> :set -XOverloadedStrings -XLinearTypes -XMagicHash
-- >>> runBuffer (\b -> "foo"# <|# "bar"# <|# b)
-- "foobar"
--
-- The literal string must not contain zero bytes @\\0@, this condition is not checked.
(<|#) ∷ Addr# → Buffer ⊸ Buffer
infixr 6 <|#
addr# <|# buffer = prependExact
  srcLen
  (\dst dstOff -> A.copyFromPointer dst dstOff (Ptr addr#) srcLen)
  buffer
  where
    srcLen = I# (cstringLength# addr#)
