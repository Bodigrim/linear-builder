-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
--              (c) 2023 Pierre Le Marre
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- 'Buffer' for strict 'Text', based on linear types.
module Data.Text.Builder.Linear.Buffer (
  -- * Type
  Buffer,

  -- * Basic interface
  runBuffer,
  runBufferBS,
  dupBuffer,
  consumeBuffer,
  eraseBuffer,
  foldlIntoBuffer,
  newEmptyBuffer,
  (><),

  -- * Single character
  (|>.),
  (.<|),

  -- * Multiple characters

  -- ** Character replication
  prependChars,
  appendChars,

  -- ** Text
  (|>),
  (<|),
  (|>…),
  (…<|),

  -- ** Raw 'Addr#'
  (|>#),
  ( #<| ), -- NOTE: extra spaces required because of -XUnboxedTuples
  (<|#),

  -- * Padding
  justifyLeft,
  justifyRight,
  center,

  -- * Number formatting

  -- ** Decimal
  (|>$),
  ($<|),

  -- ** Hexadecimal

  -- *** Lower-case
  (|>&),
  (&<|),

  -- *** Upper-case and padding
  -- $custom_hexadecimal

  -- ** Double
  (|>%),
  (%<|),
) where

import Data.Text.Array qualified as A
import Data.Text.Internal (Text (..))
import GHC.Exts (Addr#, Int (..), Ptr (..), cstringLength#, setByteArray#)
import GHC.ST (ST (..))

import Data.Text.Builder.Linear.Char
import Data.Text.Builder.Linear.Core
import Data.Text.Builder.Linear.Dec
import Data.Text.Builder.Linear.Double
import Data.Text.Builder.Linear.Hex

-- | Append 'Text' suffix to a 'Buffer' by mutating it.
-- If a suffix is statically known, consider using '(|>#)' for optimal performance.
--
-- >>> :set -XOverloadedStrings -XLinearTypes
-- >>> runBuffer (\b -> b |> "foo" |> "bar")
-- "foobar"
(|>) ∷ Buffer ⊸ Text → Buffer

infixl 6 |>
buffer |> (Text src srcOff srcLen) =
  appendExact
    srcLen
    (\dst dstOff → A.copyI srcLen dst dstOff src srcOff)
    buffer

-- | Prepend 'Text' prefix to a 'Buffer' by mutating it.
-- If a prefix is statically known, consider using '(#<|)' for optimal performance.
--
-- >>> :set -XOverloadedStrings -XLinearTypes
-- >>> runBuffer (\b -> "foo" <| "bar" <| b)
-- "foobar"
(<|) ∷ Text → Buffer ⊸ Buffer

infixr 6 <|
Text src srcOff srcLen <| buffer =
  prependExact
    srcLen
    (\dst dstOff → A.copyI srcLen dst dstOff src srcOff)
    buffer

-- | Append a null-terminated UTF-8 string
-- to a 'Buffer' by mutating it. E. g.,
--
-- >>> :set -XOverloadedStrings -XLinearTypes -XMagicHash
-- >>> runBuffer (\b -> b |># "foo"# |># "bar"#)
-- "foobar"
--
-- The literal string must not contain zero bytes @\\0@ and must be a valid UTF-8,
-- these conditions are not checked.
(|>#) ∷ Buffer ⊸ Addr# → Buffer

infixl 6 |>#
buffer |># addr# =
  appendExact
    srcLen
    (\dst dstOff → A.copyFromPointer dst dstOff (Ptr addr#) srcLen)
    buffer
  where
    srcLen = I# (cstringLength# addr#)

-- | Prepend a null-terminated UTF-8 string
-- to a 'Buffer' by mutating it. E. g.,
--
-- >>> :set -XOverloadedStrings -XLinearTypes -XMagicHash
-- >>> runBuffer (\b -> "foo"# #<| "bar"# #<| b)
-- "foobar"
--
-- The literal string must not contain zero bytes @\\0@ and must be a valid UTF-8,
-- these conditions are not checked.
--
-- /Note:/ When the syntactic extensions @UnboxedTuples@ or @UnboxedSums@ are
-- enabled, extra spaces are required when using parentheses: i.e. use @( '#<|' )@
-- instead of @('#<|')@. See the GHC User Guide chapter
-- “[Unboxed types and primitive operations](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/primitives.html#unboxed-tuples)”
-- for further information.
( #<| ) ∷ Addr# → Buffer ⊸ Buffer

infixr 6 #<|, <|#
addr# #<| buffer =
  prependExact
    srcLen
    (\dst dstOff → A.copyFromPointer dst dstOff (Ptr addr#) srcLen)
    buffer
  where
    srcLen = I# (cstringLength# addr#)

-- | Alias for @'(#<|)'@.
{-# DEPRECATED (<|#) "Use '(#<|)' instead" #-}
(<|#) ∷ Addr# → Buffer ⊸ Buffer
(<|#) = ( #<| ) -- NOTE: extra spaces required because of -XUnboxedTuples
{-# INLINE (<|#) #-}

-- | Append given number of spaces.
(|>…) ∷ Buffer ⊸ Word → Buffer

infixr 6 |>…
buf |>… 0 = buf
buffer |>… (fromIntegral → spaces@(I# spaces#)) =
  appendExact
    spaces
    ( \(A.MutableByteArray dst#) (I# dstOff#) →
        ST
          ( \s# →
              (# setByteArray# dst# dstOff# spaces# 32# s#, () #)
          )
    )
    buffer

-- | Prepend given number of spaces.
(…<|) ∷ Word → Buffer ⊸ Buffer

infixr 6 …<|
0 …<| buf = buf
(fromIntegral → spaces@(I# spaces#)) …<| buffer =
  prependExact
    spaces
    ( \(A.MutableByteArray dst#) (I# dstOff#) →
        ST
          ( \s# →
              (# setByteArray# dst# dstOff# spaces# 32# s#, () #)
          )
    )
    buffer

-- | This is just a normal 'Data.List.foldl'', but with a linear arrow
-- and unlifted accumulator.
foldlIntoBuffer ∷ ∀ a. (Buffer ⊸ a → Buffer) → Buffer ⊸ [a] → Buffer
foldlIntoBuffer f = go
  where
    go ∷ Buffer ⊸ [a] → Buffer
    go !acc [] = acc
    go !acc (x : xs) = go (f acc x) xs

-- $custom_hexadecimal
--
-- Note that no /upper/ case hexadecimal formatting is provided. This package
-- provides a minimal API with utility functions only for common cases. For
-- other use cases, please adapt the code of this package, e.g. as shown hereinafter.
--
-- __Example: Unicode code points__
--
-- The following example illustrates how to implement an efficient function that displays
-- the [Unicode code point](https://en.wikipedia.org/wiki/Unicode#Codespace_and_code_points)
-- of a character using the normative notation @U+NNNN@, such as @U+005A@ for “Z”. It
-- involves /upper-case/ and /right-justifying/ with “0”.
--
-- >>> :set -XLinearTypes -XMagicHash
-- >>> import Data.Char (ord)
-- >>> import Data.Bits (Bits(..), FiniteBits(..))
-- >>> import qualified Data.Text.Array as A
-- >>> import Data.Text.Builder.Linear.Core
-- >>> import GHC.Exts
-- >>> import GHC.ST (ST(..))
-- >>> :{
-- (|>&&) :: Buffer %1 -> Char -> Buffer
-- (|>&&) buf ch =
--   appendBounded 8 (\dst dstOff -> unsafeAppendCodePoint dst dstOff (ord ch)) buf
-- infixl 6 |>&&
-- -- Actual array writer
-- unsafeAppendCodePoint :: A.MArray s -> Int -> Int -> ST s Int
-- unsafeAppendCodePoint marr off cp = do
--   A.unsafeWrite marr off 0x55       -- ‘U’
--   A.unsafeWrite marr (off + 1) 0x2b -- ‘+’
--   case cp of
--     0 -> unsafeReplicate0 marr (off + 2) 4 *> pure 6 -- U+0000
--     _ -> do
--       unsafeReplicate0 marr (off + 2) padding -- Padding with ‘0’
--       go (off + 1 + padding + len) cp         -- Write nibbles
--       where
--         !len = 1 + shiftR (finiteBitSize cp - countLeadingZeros cp - 1) 2
--         !padding = max 0 (4 - len)
--         go !_ 0 = pure (2 + len + padding)
--         go !o m = do
--           let nibble = m .&. 0x0f
--           writeNibbleAsHex marr o (fromIntegral nibble)
--           go (o - 1) (shiftR m 4)
-- {-# INLINEABLE unsafeAppendCodePoint #-}
-- -- Replicate ‘0’ for padding
-- unsafeReplicate0 :: A.MArray s -> Int -> Int -> ST s ()
-- unsafeReplicate0 (A.MutableByteArray dst#) (I# dstOff#) (I# count#) =
--   ST (\s# -> (# setByteArray# dst# dstOff# count# 0x30# s#, () #))
-- {-# INLINE unsafeReplicate0 #-}
-- -- Convert a number to its hexadecimal digit upper character and write it
-- writeNibbleAsHex :: A.MArray s -> Int -> Int -> ST s ()
-- writeNibbleAsHex marr off n@(I# n#) = A.unsafeWrite marr off (fromIntegral hex)
--   where hex = 0x30 + n + I# (n# ># 9#) * (0x40 - 0x39)
-- :}
--
-- >>> runBuffer (\b -> b |># "Test: "# |>&& '\xff' |># "; "# |>&& '\x1ffff')
-- "Test: U+00FF; U+1FFFF"