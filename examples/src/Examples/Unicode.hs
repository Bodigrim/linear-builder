-- |
-- Copyright:   (c) 2023 Pierre Le Marre
-- Licence:     BSD3
--
-- This module illustrates how to implement an efficient function that displays
-- the [Unicode code point](https://en.wikipedia.org/wiki/Unicode#Codespace_and_code_points)
-- of a character using the normative notation @U+NNNN@, where @NNNN@ is the Unicode code
-- in /upper-case/ hexadecimal format, between 4 to 6 characters, /right-justified/
-- with “0”. Examples:
--
-- +-------------------------------------+-----------------------------+
-- | Character                           | Unicode code point notation |
-- +=====================================+=============================+
-- | @\'\\0\'@ (@minBound \@Char@)       | @U+0000@                    |
-- +-------------------------------------+-----------------------------+
-- | @\'Z\'@                             | @U+005A@                    |
-- +-------------------------------------+-----------------------------+
-- | @\'\\x1ffff\'@                      | @U+1FFFF@                   |
-- +-------------------------------------+-----------------------------+
-- | @\'\\x10ffff\'@ (@maxBound \@Char@) | @U+10FFFF@                  |
-- +-------------------------------------+-----------------------------+
module Examples.Unicode (
  (|>&&),
) where

import Data.Bits (Bits (..), FiniteBits (..))
import Data.Char (ord)
import Data.Text.Array qualified as A
import Data.Text.Builder.Linear.Core
import GHC.Exts
import GHC.ST (ST (..))

-- | Append a Unicode code point
(|>&&) ∷ Buffer ⊸ Char → Buffer
(|>&&) buf ch =
  appendBounded 8 (\dst dstOff → unsafeAppendCodePoint dst dstOff (ord ch)) buf

infixl 6 |>&&

-- Actual array writer
unsafeAppendCodePoint ∷ A.MArray s → Int → Int → ST s Int
unsafeAppendCodePoint marr off cp = do
  A.unsafeWrite marr off 0x55 -- ‘U’
  A.unsafeWrite marr (off + 1) 0x2b -- ‘+’
  case cp of
    0 → unsafeReplicate0 marr (off + 2) 4 *> pure 6 -- U+0000
    _ → do
      unsafeReplicate0 marr (off + 2) padding -- Padding with ‘0’
      go (off + 1 + padding + len) cp -- Write nibbles
      where
        !len = 1 + shiftR (finiteBitSize cp - countLeadingZeros cp - 1) 2
        !padding = max 0 (4 - len)
        go !_ 0 = pure (2 + len + padding)
        go !o m = do
          let nibble = m .&. 0x0f
          writeNibbleAsHex marr o (fromIntegral nibble)
          go (o - 1) (shiftR m 4)
{-# INLINEABLE unsafeAppendCodePoint #-}

-- Replicate ‘0’ for padding
unsafeReplicate0 ∷ A.MArray s → Int → Int → ST s ()
unsafeReplicate0 (A.MutableByteArray dst#) (I# dstOff#) (I# count#) =
  ST (\s# → (# setByteArray# dst# dstOff# count# 0x30# s#, () #))
{-# INLINE unsafeReplicate0 #-}

-- Convert a number to its hexadecimal digit upper character and write it
writeNibbleAsHex ∷ A.MArray s → Int → Int → ST s ()
writeNibbleAsHex marr off n@(I# n#) = A.unsafeWrite marr off (fromIntegral hex)
  where
    hex = 0x30 + n + I# (n# ># 9#) * (0x40 - 0x39)
