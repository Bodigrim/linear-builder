-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
module Data.Text.Builder.Linear.Hex (
  (|>&),
  (&<|),
) where

import Data.Bits (Bits (..), FiniteBits (..))
import Data.Text.Array qualified as A
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Exts (Int (..), (>#))
import GHC.ST (ST)

import Data.Text.Builder.Linear.Core

-- | Append the lower-case hexadecimal representation of a /bounded/ integral
-- number.
--
-- Negative numbers are interpreted as their corresponding unsigned number:
--
-- >>> :set -XOverloadedStrings -XLinearTypes
-- >>> import Data.Int (Int8, Int16)
-- >>> runBuffer (\b -> b |>& (-1 :: Int8)) == "ff"
-- True
-- >>> runBuffer (\b -> b |>& (-1 :: Int16)) == "ffff"
-- True
(|>&) ∷ (Integral a, FiniteBits a) ⇒ Buffer ⊸ a → Buffer

infixl 6 |>&
buffer |>& n =
  appendBounded
    (maxHexLen n)
    (\dst dstOff → unsafeAppendHex dst dstOff n)
    buffer
{-# INLINEABLE (|>&) #-}

-- | Prepend the lower-case hexadecimal representation of a /bounded/ integral
-- number.
--
-- Negative numbers are interpreted as their corresponding unsigned number:
--
-- >>> :set -XOverloadedStrings -XLinearTypes
-- >>> import Data.Int (Int8, Int16)
-- >>> runBuffer (\b -> (-1 :: Int8) &<| b) == "ff"
-- True
-- >>> runBuffer (\b -> (-1 :: Int16) &<| b) == "ffff"
-- True
(&<|) ∷ (Integral a, FiniteBits a) ⇒ a → Buffer ⊸ Buffer

infixr 6 &<|
n &<| buffer =
  prependBounded
    (maxHexLen n)
    (\dst dstOff → unsafePrependHex dst dstOff n)
    (\dst dstOff → unsafeAppendHex dst dstOff n)
    buffer
{-# INLINEABLE (&<|) #-}

-- | Compute the number of nibbles that an integral type can hold, rounded up.
maxHexLen ∷ (Integral a, FiniteBits a) ⇒ a → Int
maxHexLen n = 1 + ((finiteBitSize n - 1) `shiftR` 2)
{-# INLINEABLE maxHexLen #-}

unsafeAppendHex ∷ (Integral a, FiniteBits a) ⇒ A.MArray s → Int → a → ST s Int
unsafeAppendHex marr !off 0 =
  A.unsafeWrite marr off 0x30 >> pure 1
unsafeAppendHex marr !off n = go (off + len - 1) n
  where
    len = lengthAsHex n

    go !_ 0 = pure len
    go !o m = do
      let nibble = m .&. 0x0f
      writeNibbleAsHex marr o (fromIntegral nibble)
      go (o - 1) (dropNibble m)
{-# INLINEABLE unsafeAppendHex #-}

unsafePrependHex ∷ (Integral a, FiniteBits a) ⇒ A.MArray s → Int → a → ST s Int
unsafePrependHex marr !off 0 =
  A.unsafeWrite marr (off - 1) 0x30 >> pure 1
unsafePrependHex marr !off n = go (off - 1) n
  where
    go !o 0 = pure (off - 1 - o)
    go !o m = do
      let nibble = m .&. 0x0f
      writeNibbleAsHex marr o (fromIntegral nibble)
      go (o - 1) (dropNibble m)
{-# INLINEABLE unsafePrependHex #-}

-- | The usual 'shiftR' performs sign extension on signed number types,
-- filling the top bits with 1 if the argument is negative.
-- We don't want this behaviour here.
--
-- It would suffice to clean the sign bit only once
-- instead of doing it on every iteration of unsafe{Ap,Pre}pendHex.go,
-- but the performance impact is likely negligible.
dropNibble ∷ (Integral a, FiniteBits a) ⇒ a → a
dropNibble x = case (isSigned x, finiteBitSize x) of
  -- This is morally 'iShiftRL#', 'uncheckedIShiftRA64#', etc.,
  -- but there is no polymorphic interface to access them.
  (True, 8) → fromIntegral @Word8 (shiftR (fromIntegral x) 4)
  (True, 16) → fromIntegral @Word16 (shiftR (fromIntegral x) 4)
  (True, 32) → fromIntegral @Word32 (shiftR (fromIntegral x) 4)
  (True, 64) → fromIntegral @Word64 (shiftR (fromIntegral x) 4)
  (True, _) → shiftR x 4 .&. ((1 `shiftL` (finiteBitSize x - 4)) - 1)
  _ → shiftR x 4
{-# INLINE dropNibble #-}

-- | This assumes n /= 0. Round the number of nibbles up, as in 'maxHexLen'.
lengthAsHex ∷ FiniteBits a ⇒ a → Int
lengthAsHex n = 1 + shiftR (finiteBitSize n - countLeadingZeros n - 1) 2
{-# INLINEABLE lengthAsHex #-}

writeNibbleAsHex ∷ A.MArray s → Int → Int → ST s ()
writeNibbleAsHex marr off n@(I# n#) = A.unsafeWrite marr off (fromIntegral hex)
  where
    hex = 0x30 + n + I# (n# ># 9#) * (0x60 - 0x39)
