-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Builder for strict 'Text', based on linear types. It's consistently
-- outperforms "Data.Text.Lazy.Builder"
-- from @text@ as well as a strict builder from @text-builder@,
-- and scales better.

module Data.Text.Builder.Linear
  ( Builder(..)
  , runBuilder
  , runBuilderBS
  , fromText
  , fromChar
  , fromAddr
  , fromDec
  , fromHex
  , fromDouble
  ) where

import Data.Bits (FiniteBits)
import Data.ByteString.Internal (ByteString(..))
import Data.Text.Internal (Text(..))
import GHC.Exts (IsString(..), Addr#)

import Data.Text.Builder.Linear.Buffer

-- | Thin wrapper over 'Buffer' with a handy 'Semigroup' instance.
--
-- >>> :set -XOverloadedStrings -XMagicHash
-- >>> fromText "foo" <> fromChar '_' <> fromAddr "bar"#
-- "foo_bar"
--
-- Remember: this is a strict builder, so on contrary to "Data.Text.Lazy.Builder"
-- for optimal performance you should use strict left folds instead of lazy right ones.
--
-- Note that (similar to other builders) concatenation of 'Builder's allocates
-- thunks. This is to a certain extent mitigated by aggressive inlining,
-- but it is faster to use 'Buffer' directly.
--
newtype Builder = Builder { unBuilder :: Buffer ⊸ Buffer }

-- | Run 'Builder' computation on an empty 'Buffer', returning 'Text'.
--
-- >>> :set -XOverloadedStrings -XMagicHash
-- >>> runBuilder (fromText "foo" <> fromChar '_' <> fromAddr "bar"#)
-- "foo_bar"
--
-- This function has a polymorphic arrow and thus can be used both in
-- usual and linear contexts.
--
runBuilder :: forall m. Builder %m → Text
runBuilder (Builder f) = runBuffer f
{-# INLINE runBuilder #-}

-- | Same as 'runBuilder', but returning a UTF-8 encoded 'ByteString'.
runBuilderBS ∷ ∀ m. Builder %m → ByteString
runBuilderBS (Builder f) = runBufferBS f
{-# INLINE runBuilderBS #-}

instance Show Builder where
  show (Builder f) = show (runBuffer f)

instance Semigroup Builder where
  Builder f <> Builder g = Builder $ \b → g (f b)
  {-# INLINE (<>) #-}

instance Monoid Builder where
  mempty = Builder (\b → b)
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
fromText :: Text → Builder
fromText x = Builder $ \b → b |> x
{-# INLINE fromText #-}

-- | Create 'Builder', containing a given 'Char'.
--
-- >>> fromChar 'x' <> fromChar 'y'
-- "xy"
--
-- In contrast to 'Data.Text.Lazy.Builder.singleton', it's a responsibility
-- of the caller to sanitize surrogate code points with 'Data.Text.Internal.safe'.
--
fromChar :: Char → Builder
fromChar x = Builder $ \b → b |>. x
{-# INLINE fromChar #-}

-- | Create 'Builder', containing a null-terminated UTF-8 string, specified by 'Addr#'.
--
-- >>> :set -XMagicHash
-- >>> fromAddr "foo"# <> fromAddr "bar"#
-- "foobar"
--
-- The literal string must not contain zero bytes @\\0@, this condition is not checked.
--
fromAddr :: Addr# → Builder
fromAddr x = Builder $ \b → b |># x
{-# INLINE fromAddr #-}

-- | Create 'Builder', containing decimal representation of a given number.
--
-- >>> fromChar 'x' <> fromDec (123 :: Int)
-- "x123"
--
fromDec :: (Integral a, FiniteBits a) => a → Builder
fromDec x = Builder $ \b → b |>$ x
{-# INLINE fromDec #-}

-- | Create 'Builder', containing hexadecimal representation of a given number.
--
-- >>> :set -XMagicHash
-- >>> fromAddr "0x"# <> fromHex (0x123def :: Int)
-- "0x123def"
--
fromHex :: (Integral a, FiniteBits a) => a → Builder
fromHex x = Builder $ \b → b |>& x
{-# INLINE fromHex #-}

-- | Create 'Builder', containing a given 'Double'.
--
-- >>> :set -XMagicHash
-- >>> fromAddr "pi="# <> fromDouble pi
-- "pi=3.141592653589793"
--
fromDouble :: Double → Builder
fromDouble x = Builder $ \b → b |>% x
{-# INLINE fromDouble #-}
