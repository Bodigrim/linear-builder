{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
#ifdef aarch64_HOST_ARCH
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds #-}
#endif

module Data.Text.Builder.Linear.Dec (
  -- * Bounded
  (|>$),
  ($<|),

  -- * Unbounded
  (|>$$),
  ($$<|),
) where

#include "MachDeps.h"

import Data.Bits (Bits (..), FiniteBits (..))
import Data.Int (Int16, Int32, Int8)
import Data.Text.Array qualified as A
import Data.Word (Word16, Word32, Word8)
import Foreign.C.String (CString)
import GHC.Exts (Int (..), Ptr (..), Word (..), dataToTag#, isTrue#, (>#), (>=#))
import GHC.Num.BigNat qualified as BN
import GHC.Num.Integer qualified as I
import GHC.Ptr (plusPtr)
import GHC.ST (ST)
import Numeric.QuoteQuot (assumeNonNegArg, astQuot, quoteAST, quoteQuot)

import Data.Text.Builder.Linear.Core

--------------------------------------------------------------------------------
-- Bounded
--------------------------------------------------------------------------------

-- | Append the decimal representation of a /bounded/ integral number.
(|>$) ∷ (Integral a, FiniteBits a) ⇒ Buffer ⊸ a → Buffer

infixl 6 |>$
buffer |>$ n =
  appendBounded
    (maxDecLen n)
    (\dst dstOff → unsafeAppendBoundedDec dst dstOff n)
    buffer
{-# INLINEABLE (|>$) #-}

-- | Prepend the decimal representation of a /bounded/ integral number.
($<|) ∷ (Integral a, FiniteBits a) ⇒ a → Buffer ⊸ Buffer

infixr 6 $<|
n $<| buffer =
  prependBounded
    (maxDecLen n)
    (\dst dstOff → unsafePrependBoundedDec dst dstOff n)
    (\dst dstOff → unsafeAppendBoundedDec dst dstOff n)
    buffer
{-# INLINEABLE ($<|) #-}

-- | ceiling (fbs a * logBase 10 2) < ceiling (fbs a * 5 / 16) < 1 + floor (fbs a * 5 / 16)
maxDecLen ∷ FiniteBits a ⇒ a → Int
maxDecLen a
  | isSigned a = 2 + (finiteBitSize a * 5) `shiftR` 4
  | otherwise = 1 + (finiteBitSize a * 5) `shiftR` 4
{-# INLINEABLE maxDecLen #-}

exactDecLen ∷ (Integral a, FiniteBits a) ⇒ a → Int
exactDecLen n
  | n < 0 =
      go 2 (complement n + fromIntegral (I# (dataToTag# (n > bit (finiteBitSize n - 1)))))
  | otherwise =
      go 1 n
  where
    go ∷ (Integral a, FiniteBits a) ⇒ Int → a → Int
    go acc k
      | finiteBitSize k >= if isSigned k then 31 else 30, k >= 1e9 = go (acc + 9) (quotBillion k)
      | otherwise = acc + exactIntDecLen (fromIntegral k)
{-# INLINEABLE exactDecLen #-}

exactIntDecLen ∷ Int → Int
exactIntDecLen l@(I# l#)
  | l >= 1e5 = 5 + I# (l# >=# 100_000_000#) + I# (l# >=# 10_000_000#) + I# (l# >=# 1_000_000#)
  | otherwise = I# (l# >=# 10_000#) + I# (l# >=# 1_000#) + I# (l# >=# 100#) + I# (l# >=# 10#)

unsafeAppendBoundedDec ∷ (Integral a, FiniteBits a) ⇒ A.MArray s → Int → a → ST s Int
unsafeAppendBoundedDec marr off n = unsafePrependBoundedDec marr (off + exactDecLen n) n
{-# INLINEABLE unsafeAppendBoundedDec #-}

unsafePrependBoundedDec ∷ ∀ s a. (Integral a, FiniteBits a) ⇒ A.MArray s → Int → a → ST s Int
unsafePrependBoundedDec marr !off n
  | n < 0
  , n == bit (finiteBitSize n - 1) = do
      A.unsafeWrite marr (off - 1) (fromIntegral (0x30 + minBoundLastDigit n))
      go (off - 2) (abs (bit (finiteBitSize n - 1) `quot` 10)) >>= sign
  | n == 0 = do
      A.unsafeWrite marr (off - 1) 0x30 >> pure 1
  | otherwise = go (off - 1) (abs n) >>= sign
  where
    sign !o
      | n > 0 = pure (off - o)
      | otherwise = do
          A.unsafeWrite marr (o - 1) 0x2d -- '-'
          pure (off - o + 1)

    go ∷ Int → a → ST s Int
    go o k
      | k >= 10 = do
          let (q, r) = quotRem100 k
          A.copyFromPointer marr (o - 1) (digits `plusPtr` (fromIntegral r `shiftL` 1)) 2
          if k < 100 then pure (o - 1) else go (o - 2) q
      | otherwise = do
          A.unsafeWrite marr o (fromIntegral (0x30 + k))
          pure o
{-# INLINEABLE unsafePrependBoundedDec #-}

digits ∷ CString
digits = Ptr "00010203040506070809101112131415161718192021222324252627282930313233343536373839404142434445464748495051525354555657585960616263646566676869707172737475767778798081828384858687888990919293949596979899"#

-- Compute rem minBound 10 efficiently. Given that:
-- • minBound = 1 `shiftL` (finiteBitSize a - 1) = -2^(finiteBitSize a - 1)
-- • the last digit of 2^k forms a cycle for k≥1: 2,4,8,6
-- Then it is enough to pattern-match rem (finiteBitSize a) 4,
-- i.e. finiteBitSize a .&. 3
minBoundLastDigit ∷ FiniteBits a ⇒ a → Int
minBoundLastDigit a = case finiteBitSize a .&. 3 of
  0 → 8
  1 → 6
  2 → 2
  _ → 4
{-# INLINEABLE minBoundLastDigit #-}

quotRem100 ∷ (Integral a, FiniteBits a) ⇒ a → (a, a)

-- https://gitlab.haskell.org/ghc/ghc/-/issues/22933
#ifdef aarch64_HOST_ARCH
quotRem100 a = a `quotRem` 100
#else
quotRem100 a = let q = quot100 a in (q, a - 100 * q)
#endif
{-# INLINEABLE quotRem100 #-}

quot100 ∷ (Integral a, FiniteBits a) ⇒ a → a
quot100 a = case (finiteBitSize a, isSigned a) of
  (64, True)
    | finiteBitSize (0 ∷ Int) == 64 →
        cast $$(quoteAST $ assumeNonNegArg $ astQuot (100 ∷ Int))
  (64, False)
    | finiteBitSize (0 ∷ Word) == 64 →
        cast $$(quoteQuot (100 ∷ Word))
  (32, True) → cast $$(quoteAST $ assumeNonNegArg $ astQuot (100 ∷ Int32))
  (32, False) → cast $$(quoteQuot (100 ∷ Word32))
  (16, True) → cast $$(quoteAST $ assumeNonNegArg $ astQuot (100 ∷ Int16))
  (16, False) → cast $$(quoteQuot (100 ∷ Word16))
  (8, True) → cast $$(quoteAST $ assumeNonNegArg $ astQuot (100 ∷ Int8))
  (8, False) → cast $$(quoteQuot (100 ∷ Word8))
  _ → a `quot` 100
  where
    cast ∷ (Integral a, Integral b) ⇒ (b → b) → a
    cast f = fromIntegral (f (fromIntegral a))
{-# INLINEABLE quot100 #-}

quotBillion ∷ (Integral a, FiniteBits a) ⇒ a → a
#ifdef aarch64_HOST_ARCH
quotBillion a = a `quot` 1e9
#else
quotBillion a = case (finiteBitSize a, isSigned a) of
  (64, True)
    | finiteBitSize (0 :: Int) == 64
    → cast $$(quoteAST $ assumeNonNegArg $ astQuot (1e9 :: Int))
  (64, False)
    | finiteBitSize (0 :: Word) == 64
    → cast $$(quoteQuot (1e9 :: Word))
  (32, True)  → cast $$(quoteAST $ assumeNonNegArg $ astQuot (1e9 :: Int32))
  (32, False) → cast $$(quoteQuot (1e9 :: Word32))
  _ → a `quot` 1e9
  where
    cast :: (Integral a, Integral b) => (b → b) → a
    cast f = fromIntegral (f (fromIntegral a))
#endif
{-# INLINEABLE quotBillion #-}

--------------------------------------------------------------------------------
-- Unbounded
--------------------------------------------------------------------------------

-- | Append the decimal representation of an /unbounded/ integral number.
(|>$$) ∷ Integral a ⇒ Buffer ⊸ a → Buffer

infixl 6 |>$$
buffer |>$$ n = case toInteger n of
  n' →
    appendBounded
      (maxIntegerDecLen n')
      (\dst dstOff → unsafeAppendUnboundedDec dst dstOff n')
      buffer
{-# INLINEABLE (|>$$) #-}

-- | Prepend the decimal representation of an /unbounded/ integral number.
($$<|) ∷ Integral a ⇒ a → Buffer ⊸ Buffer

infixr 6 $$<|
n $$<| buffer = case toInteger n of
  n' →
    prependBounded
      (maxIntegerDecLen n')
      (\dst dstOff → unsafePrependUnboundedDec dst dstOff n')
      (\dst dstOff → unsafeAppendUnboundedDec dst dstOff n')
      buffer
{-# INLINEABLE ($$<|) #-}

-- | ceiling (fbs a * logBase 10 2) < ceiling (fbs a * 5 / 16) < 1 + floor (fbs a * 5 / 16)
maxIntegerDecLen ∷ Integer → Int
maxIntegerDecLen a = case a of
  I.IS i# → maxDecLen (I# i#)
  I.IP n# → 1 + (fromIntegral (BN.bigNatSize n#) * finiteBitSize (0 ∷ Word) * 5) `shiftR` 4
  I.IN n# → 2 + (fromIntegral (BN.bigNatSize n#) * finiteBitSize (0 ∷ Word) * 5) `shiftR` 4
{-# INLINEABLE maxIntegerDecLen #-}

exactIntegerDecLen ∷ Integer → Int
exactIntegerDecLen n = case n of
  I.IS i# → exactDecLen (I# i#)
  I.IP n# → exactBigNatDecLen 1 n#
  I.IN n# → exactBigNatDecLen 2 n#
{-# INLINEABLE exactIntegerDecLen #-}

exactBigNatDecLen ∷ Int → BN.BigNat# → Int
exactBigNatDecLen = go
  where
    go !acc n# = case BN.bigNatCompareWord# n# 1_000_000_000## of
      LT → acc + exactIntDecLen (BN.bigNatToInt n#)
      _ → go (acc + 9) (BN.bigNatQuotWord# n# 1_000_000_000##)

unsafeAppendUnboundedDec ∷ A.MArray s → Int → Integer → ST s Int
unsafeAppendUnboundedDec marr off n = unsafePrependUnboundedDec marr (off + exactIntegerDecLen n) n
{-# INLINEABLE unsafeAppendUnboundedDec #-}

unsafePrependUnboundedDec ∷ ∀ s. A.MArray s → Int → Integer → ST s Int
unsafePrependUnboundedDec marr !off n = case toInteger n of
  I.IS i# → unsafePrependBoundedDec marr off (I# i#)
  I.IP n# → go (off - 1) n# >>= \o → pure (off - o)
  I.IN n# → go (off - 1) n# >>= sign
  where
    sign !o = do
      A.unsafeWrite marr (o - 1) 0x2d -- '-'
      pure (off - o + 1)

    -- TODO: remove once best implementation found
    -- go ∷ Int → BN.BigNat# → ST s Int
    -- go o n# = case BN.bigNatCompareWord# n# 10## of
    --   LT → do
    --     A.unsafeWrite marr o (fromIntegral (0x30 + BN.bigNatToWord n#))
    --     pure o
    --   _ → do
    --     let !(# q#, r## #) = BN.bigNatQuotRemWord# n# 100##
    --     A.copyFromPointer marr (o - 1) (digits `plusPtr` (fromIntegral (W# r##) `shiftL` 1)) 2
    --     case BN.bigNatCompareWord# n# 100## of
    --       LT → pure (o - 1)
    --       _ → go (o - 2) q#

    go ∷ Int → BN.BigNat# → ST s Int
    go o n# = do
      let !(# q#, r## #) = BN.bigNatQuotRemWord# n# 100##
      A.copyFromPointer marr (o - 1) (digits `plusPtr` (fromIntegral (W# r##) `shiftL` 1)) 2
      -- TODO: remove once best implementation found
      -- case BN.bigNatToWordMaybe# q# of
      --   (# | w# #) → goWord (o - 2) (W# w#)
      --   _ → go (o - 2) q#
      if BN.bigNatIsZero q#
        then pure (o - 2)
        else
          if isTrue# (BN.bigNatSize# q# ># 1#)
            then go (o - 2) q#
            else goWord (o - 2) (W# (BN.bigNatIndex# q# 0#))

    goWord ∷ Int → Word → ST s Int
    goWord o k
      | k >= 10 = do
          let (q, r) = quotRem100 k
          A.copyFromPointer marr (o - 1) (digits `plusPtr` (fromIntegral r `shiftL` 1)) 2
          if k < 100 then pure (o - 1) else goWord (o - 2) q
      | otherwise = do
          A.unsafeWrite marr o (fromIntegral (0x30 + k))
          pure o
{-# INLINEABLE unsafePrependUnboundedDec #-}
