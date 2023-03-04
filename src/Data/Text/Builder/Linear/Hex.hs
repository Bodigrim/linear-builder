-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
module Data.Text.Builder.Linear.Hex (
  (|>&),
  (&<|),
) where

import Data.Bits (Bits (..), FiniteBits (..))
import Data.Foldable (forM_)
import Data.Text.Array qualified as A
import GHC.Exts (Int (..), (<=#), (>#))
import GHC.ST (ST)

import Data.Text.Builder.Linear.Core

-- | Append hexadecimal number.
(|>&) ∷ (Integral a, FiniteBits a) ⇒ Buffer ⊸ a → Buffer

infixl 6 |>&
buffer |>& n =
  appendBounded
    (finiteBitSize n `shiftR` 2)
    (\dst dstOff → unsafeAppendHex dst dstOff n)
    buffer
{-# INLINEABLE (|>&) #-}

-- | Prepend hexadecimal number.
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
unsafeAppendHex marr !off n = do
  let len = lengthAsHex n
  forM_ [0 .. len - 1] $ \i →
    let nibble = (n `shiftR` ((len - 1 - i) `shiftL` 2)) .&. 0xf
     in writeNibbleAsHex marr (off + i) (fromIntegral nibble)
  pure len
{-# INLINEABLE unsafeAppendHex #-}

unsafePrependHex ∷ (Integral a, FiniteBits a) ⇒ A.MArray s → Int → a → ST s Int
unsafePrependHex marr !off n = do
  let len = lengthAsHex n
  forM_ [0 .. len - 1] $ \i →
    let nibble = (n `shiftR` (i `shiftL` 2)) .&. 0xf
     in writeNibbleAsHex marr (off - 1 - i) (fromIntegral nibble)
  pure len
{-# INLINEABLE unsafePrependHex #-}

lengthAsHex ∷ FiniteBits a ⇒ a → Int
lengthAsHex n = max1 $ (finiteBitSize n `shiftR` 2) - (countLeadingZeros n `shiftR` 2)
{-# INLINEABLE lengthAsHex #-}

-- Branchless equivalent for max 1 n.
max1 ∷ Int → Int
max1 n@(I# n#) = n `xor` I# (n# <=# 0#)

writeNibbleAsHex ∷ A.MArray s → Int → Int → ST s ()
writeNibbleAsHex marr off n@(I# n#) = A.unsafeWrite marr off (fromIntegral hex)
  where
    hex = 48 + n + I# (n# ># 9#) * 39
