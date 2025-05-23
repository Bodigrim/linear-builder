-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Builder for strict t'Text' and 'ByteString', based on linear types. It consistently
-- outperforms "Data.Text.Lazy.Builder"
-- from @text@ as well as a strict builder from @text-builder@,
-- and scales better.
module Data.Text.Builder.Linear (
  Builder (..),
  runBuilder,
  runBuilderBS,
  fromText,
  fromChar,
  fromAddr,
  fromDec,
  fromUnboundedDec,
  fromHex,
  fromDouble,
) where

import Data.Bits (FiniteBits)
import Data.ByteString.Internal (ByteString (..))
import Data.Text.Internal (Text (..))
import GHC.Exts (Addr#, IsString (..))

import Data.Text.Builder.Linear.Buffer

-- | Thin wrapper over t'Buffer' with a handy 'Semigroup' instance.
--
-- >>> :set -XOverloadedStrings -XMagicHash
-- >>> fromText "foo" <> fromChar '_' <> fromAddr "bar"#
-- "foo_bar"
--
-- Remember: this is a strict builder, so on contrary to "Data.Text.Lazy.Builder"
-- for optimal performance you should use strict left folds instead of lazy right ones.
--
-- Note that (similar to other builders) concatenation of t'Builder's allocates
-- thunks. This is to a certain extent mitigated by aggressive inlining,
-- but it is faster to use t'Buffer' directly.
newtype Builder = Builder {unBuilder ∷ Buffer ⊸ Buffer}

-- | @since 0.1.4
instance Eq Builder where
  b1 == b2 = runBuilder b1 == runBuilder b2

-- | @since 0.1.4
instance Ord Builder where
  compare b1 b2 = compare (runBuilder b1) (runBuilder b2)
  b1 <= b2 = runBuilder b1 <= runBuilder b2
  b1 < b2 = runBuilder b1 < runBuilder b2
  b1 >= b2 = runBuilder b1 >= runBuilder b2
  b1 > b2 = runBuilder b1 > runBuilder b2

-- | Run t'Builder' computation on an empty t'Buffer', returning strict t'Text'.
--
-- >>> :set -XOverloadedStrings -XMagicHash
-- >>> runBuilder (fromText "foo" <> fromChar '_' <> fromAddr "bar"#)
-- "foo_bar"
--
-- This function has a polymorphic arrow and thus can be used both in
-- usual and linear contexts.
runBuilder ∷ ∀ m. Builder %m → Text
runBuilder (Builder f) = runBuffer f
{-# INLINE runBuilder #-}

-- | Same as 'runBuilder', but returning a UTF-8 encoded strict 'ByteString'.
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

-- | Use 'fromString' to create t'Builder' from 'String'.
instance IsString Builder where
  fromString = fromText . fromString
  {-# INLINE fromString #-}

-- | Create t'Builder', containing a given t'Text'.
--
-- >>> :set -XOverloadedStrings
-- >>> fromText "foo" <> fromText "bar"
-- "foobar"
--
-- For literal strings it is faster to use 'fromAddr' instead of 'fromText'.
fromText ∷ Text → Builder
fromText x = Builder $ \b → b |> x
{-# INLINE fromText #-}

-- | Create t'Builder', containing a given 'Char'.
--
-- >>> fromChar 'x' <> fromChar 'y'
-- "xy"
--
-- In contrast to 'Data.Text.Lazy.Builder.singleton', it's a responsibility
-- of the caller to sanitize surrogate code points with 'Data.Text.Internal.safe'.
fromChar ∷ Char → Builder
fromChar x = Builder $ \b → b |>. x
{-# INLINE fromChar #-}

-- | Create t'Builder', containing a null-terminated UTF-8 string, specified by 'Addr#'.
--
-- >>> :set -XMagicHash
-- >>> fromAddr "foo"# <> fromAddr "bar"#
-- "foobar"
--
-- The literal string must not contain zero bytes @\\NUL@ and must be a valid UTF-8,
-- these conditions are not checked.
fromAddr ∷ Addr# → Builder
fromAddr x = Builder $ \b → b |># x
{-# INLINE fromAddr #-}

-- | Create t'Builder', containing decimal representation of a given /bounded/ integer.
--
-- >>> fromChar 'x' <> fromDec (123 :: Int)
-- "x123"
fromDec ∷ (Integral a, FiniteBits a) ⇒ a → Builder
fromDec x = Builder $ \b → b |>$ x
{-# INLINE fromDec #-}

-- | Create t'Builder', containing decimal representation of a given /unbounded/ integer.
--
-- >>> fromChar 'x' <> fromUnboundedDec (1e24 :: Integer)
-- "x1000000000000000000000000"
--
-- @since 0.1.3
fromUnboundedDec ∷ Integral a ⇒ a → Builder
fromUnboundedDec x = Builder $ \b → b |>$$ x
{-# INLINE fromUnboundedDec #-}

-- | Create t'Builder', containing hexadecimal representation of a given integer.
--
-- >>> :set -XMagicHash
-- >>> fromAddr "0x"# <> fromHex (0x123def :: Int)
-- "0x123def"
fromHex ∷ (Integral a, FiniteBits a) ⇒ a → Builder
fromHex x = Builder $ \b → b |>& x
{-# INLINE fromHex #-}

-- | Create t'Builder', containing decimal representation of a given 'Double'.
--
-- >>> :set -XMagicHash
-- >>> fromAddr "pi="# <> fromDouble pi
-- "pi=3.141592653589793"
fromDouble ∷ Double → Builder
fromDouble x = Builder $ \b → b |>% x
{-# INLINE fromDouble #-}
