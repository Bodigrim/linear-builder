-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>

{-# LANGUAGE TemplateHaskell #-}

module Data.Text.Builder.Linear.Dec
  ( (|>$)
  , ($<|)
  ) where

import Data.Bifunctor
import Data.Bits
import Data.Int
import Data.Text ()
import qualified Data.Text.Array as A
import Data.Word
import GHC.ST
import Numeric.QuoteQuot

import Data.Text.Builder.Linear.Core

-- | Append decimal number.
(|>$) :: (Integral a, FiniteBits a) => Buffer ⊸ a -> Buffer
infixl 6 |>$
buffer |>$ n = appendBounded
  (maxDecLen n)
  (\dst dstOff -> unsafeAppendDec dst dstOff n)
  buffer

-- | Prepend decimal number.
($<|) :: (Integral a, FiniteBits a) => a -> Buffer ⊸ Buffer
infixr 6 $<|
n $<| buffer = prependBounded
  (maxDecLen n)
  (\dst dstOff -> unsafePrependDec dst dstOff n)
  (\dst dstOff -> unsafeAppendDec dst dstOff n)
  buffer

-- | ceiling (fbs a * logBase 10 2) < ceiling (fbs a * 5 / 16) < 1 + floor (fbs a * 5 / 16)
maxDecLen :: FiniteBits a => a -> Int
maxDecLen a
  | isSigned a = 2 + (finiteBitSize a * 5) `shiftR` 4
  | otherwise  = 1 + (finiteBitSize a * 5) `shiftR` 4

exactDecLen :: (Integral a, FiniteBits a) => a -> Int
exactDecLen n
  | isSigned n, n == bit (finiteBitSize n - 1)
  = 1 + go (complement n)
  | isSigned n, n < 0
  = 1 + go (negate n)
  | otherwise
  = go n
  where
    go k
      | k < 10 = 1
      | otherwise = 1 + go (k `quot` 10)
      -- TODO this is very slow

unsafeAppendDec :: (Integral a, FiniteBits a) => A.MArray s -> Int -> a -> ST s Int
unsafeAppendDec marr off n = unsafePrependDec marr (off + exactDecLen n) n

unsafePrependDec :: (Integral a, FiniteBits a) => A.MArray s -> Int -> a -> ST s Int
unsafePrependDec marr off n = go (off - 1) (abs n) >>= sign
  where
    sign o
      | n >= 0 = pure (off - o)
      | otherwise = do
        A.unsafeWrite marr (o - 1) 0x2d -- '-'
        pure (off - o + 1)

    go o k = do
      A.unsafeWrite marr o (fromIntegral (48 + r))
      if q == 0 then pure o else go (o - 1) q
      where
        (q, r) = quotRem10 k

quotRem10 :: (Integral a, FiniteBits a) => a -> (a, a)
quotRem10 a = case (finiteBitSize a, isSigned a) of
  (64, True)  -> cast $$(quoteQuotRem (10 :: Int64))
  (64, False) -> cast $$(quoteQuotRem (10 :: Word64))
  (32, True)  -> cast $$(quoteQuotRem (10 :: Int32))
  (32, False) -> cast $$(quoteQuotRem (10 :: Word32))
  (16, True)  -> cast $$(quoteQuotRem (10 :: Int16))
  (16, False) -> cast $$(quoteQuotRem (10 :: Word16))
  ( 8, True)  -> cast $$(quoteQuotRem (10 :: Int8))
  ( 8, False) -> cast $$(quoteQuotRem (10 :: Word8))
  _ -> a `quotRem` 10
  where
    cast :: (Integral a, Integral b) => (b -> (b, b)) -> (a, a)
    cast f = bimap fromIntegral fromIntegral (f (fromIntegral a))
