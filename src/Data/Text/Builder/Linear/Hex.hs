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
import GHC.Exts (Int (..), (>#))
import GHC.ST (ST)

import Data.Text.Builder.Linear.Core

-- | Append the lower-case hexadecimal represensation of a /non-negative/ number.
--
-- __Warning:__ this function has an undefined behaviour for negative numbers.
(|>&) ∷ (Integral a, FiniteBits a) ⇒ Buffer ⊸ a → Buffer

infixl 6 |>&
buffer |>& n =
  appendBounded
    (finiteBitSize n `shiftR` 2)
    (\dst dstOff → unsafeAppendHex dst dstOff n)
    buffer
{-# INLINEABLE (|>&) #-}

-- | Prepend the lower-case hexadecimal representation of a /non-negative/ number.
--
-- __Warning:__ this function has an undefined behaviour for negative numbers.
(&<|) ∷ (Integral a, FiniteBits a) ⇒ a → Buffer ⊸ Buffer

infixr 6 &<|
n &<| buffer =
  prependBounded
    (finiteBitSize n `shiftR` 2)
    (\dst dstOff → unsafePrependHex dst dstOff n)
    (\dst dstOff → unsafeAppendHex dst dstOff n)
    buffer
{-# INLINEABLE (&<|) #-}

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
-- instead of doing it on every iteration of unsafe{Ap,Pre}pernHex.go,
-- but the performance impact is likely negligible.
dropNibble ∷ (Integral a, FiniteBits a) ⇒ a → a
dropNibble x
  -- This is morally 'iShiftRL#', 'uncheckedIShiftRA64#', etc.,
  -- but there is no polymorphic interface to access them.
  | isSigned x = shiftR x 4 .&. ((1 `shiftL` (finiteBitSize x - 4)) - 1)
  | otherwise = shiftR x 4
{-# INLINE dropNibble #-}

-- | This assumes n /= 0.
lengthAsHex ∷ FiniteBits a ⇒ a → Int
lengthAsHex n = (finiteBitSize n `shiftR` 2) - (countLeadingZeros n `shiftR` 2)
{-# INLINEABLE lengthAsHex #-}

writeNibbleAsHex ∷ A.MArray s → Int → Int → ST s ()
writeNibbleAsHex marr off n@(I# n#) = A.unsafeWrite marr off (fromIntegral hex)
  where
    hex = 0x30 + n + I# (n# ># 9#) * (0x60 - 0x39)
