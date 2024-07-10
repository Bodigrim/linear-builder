-- |
-- Copyright:   (c) 2024 Pierre Le Marre
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
module Data.Text.Builder.Linear.Dec.Unbounded (
  (|>$$),
  ($$<|),
  -- prependUnboundedDecimal,
  -- Strategy (..),
)
where

import Data.Bits (Bits (..), FiniteBits (..))
import Data.Text.Array qualified as A
import Data.Word (Word64)
import GHC.Exts (
  Int (..),
  Int#,
  State#,
  Word (..),
  Word#,
  word2Int#,
  (-#),
 )
import GHC.Num.BigNat qualified as BN
import GHC.Num.Integer qualified as I
import GHC.Num.Natural qualified as N
import GHC.Ptr (plusPtr)
import GHC.ST (ST (..))

import Data.Text.Builder.Linear.Array (unsafeReplicate)
import Data.Text.Builder.Linear.Core (Buffer)
import Data.Text.Builder.Linear.Dec.Bounded (digits, maxDecLen, quotRem100)
import Data.Text.Builder.Linear.Dec.Bounded qualified as Bounded
import Data.Text.Builder.Linear.Internal (appendBounded', prependBounded')

--------------------------------------------------------------------------------
-- Append
--------------------------------------------------------------------------------

-- | Append the decimal representation of an /unbounded/ integral number.
--
-- @since 0.1.3
(|>$$) ∷ Integral a ⇒ Buffer ⊸ a → Buffer

infixl 6 |>$$
buffer |>$$ n = case toInteger n of
  !n' →
    appendBounded'
      (maxIntegerDecLen n')
      (unsafeAppendDec n')
      buffer
{-# INLINEABLE (|>$$) #-}

-- • For small 'Integers', `unsafeAppendDec`
-- • For 'BigNat's, use a buffer with `unsafePrependUnboundedDec`, then copy it.
--
-- For *bounded* integers we used the exact size of the decimal representation to
-- compute the offset from which we can use the prepend action to actually append.
--
-- But the exact size of an (unbounded) 'Integer' could be expensive to compute.
-- So it is faster to use a buffer and then copy it.
unsafeAppendDec
  ∷ ∀ s x
   . Integer
  → ((A.MArray s → Int → ST s Int) → ST s x)
  → ((A.MArray s → Int → ST s Int) → ST s x)
  → ST s x
unsafeAppendDec n = case n of
  I.IS i# → \append _ → append (\marr off → Bounded.unsafeAppendDec marr off (I# i#))
  _ → \_ prepend → prepend (\marr off → unsafePrependDec marr off n)
{-# INLINEABLE unsafeAppendDec #-}

--------------------------------------------------------------------------------
-- Prepend
--------------------------------------------------------------------------------

-- | Prepend the decimal representation of an /unbounded/ integral number.
--
-- @since 0.1.3
($$<|) ∷ Integral a ⇒ a → Buffer ⊸ Buffer

infixr 6 $$<|
n $$<| buffer = case toInteger n of
  !n' →
    prependBounded'
      (maxIntegerDecLen n')
      (\dst dstOff → unsafePrependDec dst dstOff n')
      buffer
{-# INLINEABLE ($$<|) #-}

unsafePrependDec ∷ ∀ s. A.MArray s → Int → Integer → ST s Int
unsafePrependDec marr off@(I# off#) n = case n of
  I.IS i# → Bounded.unsafePrependDec marr off (I# i#)
  _ → unsafePrependBigNatDec marr (off# -# 1#) (integerToBigNat# n) >>= prependSign
    where
      prependSign !off' =
        if n < 0
          then do
            A.unsafeWrite marr (off' - 1) 0x2d -- '-'
            pure (off - off' + 1)
          else pure (off - off')
{-# INLINEABLE unsafePrependDec #-}

type DigitsWriter s = Int# → BN.BigNat# → ST s Int

-- Use the fastest writer depending on the BigNat size
unsafePrependBigNatDec ∷ ∀ s. A.MArray s → DigitsWriter s
unsafePrependBigNatDec marr !off0 !n0
  | BN.bigNatSize n0 < hugeSizeThreshold = prependSmallNat marr off0 n0
  | otherwise = prependHugeNat marr off0 n0
  where
    hugeSizeThreshold ∷ Word
    hugeSizeThreshold = 80

-- Writer for “small” 'BigNat's.
--
-- Divide repeatedly by poweredBase.
prependSmallNat ∷ ∀ s. A.MArray s → DigitsWriter s
prependSmallNat marr = go
  where
    !(# power, poweredBase, _poweredBase² #) = selectPower (# #)

    go ∷ DigitsWriter s
    go !o1 !n = case n `BN.bigNatQuotRemWord#` poweredBase of
      (# q, r #) → do
        !o2 ← unsafePrependWordDec marr (I# o1) (W# r)
        if BN.bigNatIsZero q
          then pure o2
          else do
            let !o3 = o1 -# (word2Int# power -# 1#)
            padWithZeros marr (I# o3) (o2 - I# o3)
            go (o3 -# 1#) q

-- Use the raw state in order to avoid boxed Int in `scaleWriter`
type DigitsWriter# s = Int# → BN.BigNat# → State# s → (# State# s, Int# #)

-- Writer for “huge” 'BigNat's.
--
-- Algorithm used in bytestring-0.12.1 (simplified):
--
-- 1. Find k0 = min k such that pow10 ^ (2 ^ (k + 1)) > n0
-- 2. Set k to k0 and n to n0
-- 3. Set (q, r) = n `quotRem` (pow10 ^ (2 ^ k))
-- 4. if k = 0, then write decimal representation of q and r
--    else repeat recursively 3 and 4 with n = {q,r} and k = k - 1
prependHugeNat ∷ ∀ s. A.MArray s → DigitsWriter s
prependHugeNat marr off n = ST $ \s1 →
  case go prependTiny# poweredBase² off n s1 of
    (# s2, off'# #) → (# s2, I# off'# #)
  where
    !(# power, poweredBase, poweredBase² #) = selectPower (# #)

    go ∷ (Bool → DigitsWriter# s) → BN.BigNat# → DigitsWriter# s
    go !write !pow10 !o !n# =
      if BN.bigNatLt n# pow10
        then write True o n#
        else go (scaleWriter write pow10) (BN.bigNatMul pow10 pow10) o n#

    scaleWriter ∷ (Bool → DigitsWriter# s) → BN.BigNat# → Bool → DigitsWriter# s
    scaleWriter !write !pow10 = \ !high !o1 !n# s1 →
      case BN.bigNatQuotRem# n# pow10 of
        (# q, r #)
          | high && BN.bigNatIsZero q → write high o1 r s1
          | otherwise → case write False o1 r s1 of
              (# s2, o2 #) → write high (o2 -# 1#) q s2

    prependTiny# ∷ Bool → DigitsWriter# s
    prependTiny# !high !o1 !n# = case prependTiny high o1 n# of
      ST f → \s1 → case f s1 of
        (# s2, I# o2 #) → (# s2, o2 #)

    -- Use ST instead of raw state as the utils functions do.
    -- `prependTiny` must inline to leave no boxing/unboxing roundtrip.
    {-# INLINE prependTiny #-}
    prependTiny ∷ Bool → DigitsWriter s
    prependTiny !high !o1 !n# =
      case BN.bigNatQuotRemWord# n# poweredBase of
        (# q, r #) → do
          !o2 ← unsafePrependWordDec marr (I# o1) (W# r)
          if high && BN.bigNatIsZero q
            then pure o2
            else do
              let !o3 = I# o1 - (fromIntegral (W# power) - 1)
              padWithZeros marr o3 (o2 - o3)
              !o4 ← unsafePrependWordDec marr (o3 - 1) (BN.bigNatToWord q)
              if high
                then pure o4
                else do
                  let !o5 = o3 - fromIntegral (W# power)
                  padWithZeros marr o5 (o4 - o5)
                  pure o5

--------------------------------------------------------------------------------
-- Prepend word
--------------------------------------------------------------------------------

unsafePrependWordDec ∷ ∀ s. A.MArray s → Int → Word → ST s Int
unsafePrependWordDec = f
  where
    f marr !o !k
      | k >= 10 = do
          let (q, r) = quotRem100 k
          A.copyFromPointer marr (o - 1) (digits `plusPtr` (fromIntegral r `shiftL` 1)) 2
          if k < 100 then pure (o - 1) else f marr (o - 2) q
      | otherwise = do
          A.unsafeWrite marr o (fromIntegral (0x30 + k))
          pure o

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

maxIntegerDecLen ∷ Integer → Int
maxIntegerDecLen a = case a of
  I.IS i# → maxDecLen (I# i#)
  I.IP n# → maxBitNatDecLen n#
  I.IN n# → 1 + maxBitNatDecLen n#
{-# INLINEABLE maxIntegerDecLen #-}

-- | ceiling (fbs a * logBase 10 2) < ceiling (fbs a * 5 / 16) < 1 + floor (fbs a * 5 / 16)
--
-- We approximate @fbs a@ to @bigNatSize a * word_size@.
maxBitNatDecLen ∷ BN.BigNat# → Int
maxBitNatDecLen n#
  -- This can overflow in theory, but in practice it would overflow for a BigNat#
  -- of at least:
  --
  -- • On 32 bits platform: 6.4 GiB, out of max 4 GiB RAM
  --   → BN.bigNatSize n# = 214748364 =
  --       (maxBound @Int32 - 1) `div` fromIntegral (shiftR (finiteBitSize @Word32 0 * 5) 4)
  -- • On 64 bits platform: 3276 PiB
  --   → BN.bigNatSize n# = 461168601842738790 =
  --       (maxBound @Int64 - 1) `div` fromIntegral (shiftR (finiteBitSize @Word64 0 * 5) 4)
  --
  -- These thresholds are too big to be realistic (32 bits: more than available RAM, 64
  -- bits: integer size in petabytes), so it is perfectly reasonable to have no
  -- special handling of overflow here.

  -- Word bit size is multiple of 16 (e.g. 32 and 64 bits arch)
  | rem (finiteBitSize @Word 0) 16 == 0 =
      1 + fromIntegral (BN.bigNatSize n# * shiftR (fromIntegral (finiteBitSize @Word 0) * 5) 4)
  -- Other cases (non-standard arch)
  | otherwise =
      1
        + fromIntegral @Word64
          ( (fromIntegral (BN.bigNatSize n#) * fromIntegral (finiteBitSize @Word 0) * 5)
              `shiftR` 4
          )
{-# INLINEABLE maxBitNatDecLen #-}

integerToBigNat# ∷ Integer → BN.BigNat#
integerToBigNat# n = case I.integerToBigNatSign# n of
  (# _, n# #) → n#
{-# INLINE integerToBigNat# #-}

-- Maximal power of 10 fitting into a 'Word':
-- • 10 ^ 9  for 32 bit words  (32 * log 2 / log 10 ≈  9.63)
-- • 10 ^ 19 for 64 bit words  (64 * log 2 / log 10 ≈ 19.27)
--
-- Why (# #)? We can't have top-level unlifted bindings
-- (see: https://gitlab.haskell.org/ghc/ghc/-/issues/17521). So we use a function
-- that take an empty argument (# #) that will be discarded at compile time.
selectPower ∷ (# #) → (# Word#, Word#, BN.BigNat# #)
selectPower _ = case finiteBitSize @Word 0 of
  64 → (# 19##, 10000000000000000000##, N.naturalToBigNat# tenPower38 #)
  -- Not 64 bits: assume 32 bits
  _ → (# 9##, 1000000000##, N.naturalToBigNat# tenPower18 #)

-- NOTE: ensure to not inline the following numbers, in order to avoid allocations.

tenPower18 ∷ N.Natural
tenPower18 = 1e18
{-# NOINLINE tenPower18 #-}

tenPower38 ∷ N.Natural
tenPower38 = 1e38
{-# NOINLINE tenPower38 #-}

padWithZeros ∷ ∀ s. A.MArray s → Int → Int → ST s ()
padWithZeros marr off count = unsafeReplicate marr off count 0x30
{-# INLINE padWithZeros #-}

--------------------------------------------------------------------------------
-- For testing purpose only
--------------------------------------------------------------------------------

-- data Strategy = SmallOnly | HugeOnly

-- prependUnboundedDecimal ∷ Integral a ⇒ Strategy → a → Buffer ⊸ Buffer
-- prependUnboundedDecimal strategy n buffer = case toInteger n of
--   !n' →
--     prependBounded'
--       (maxIntegerDecLen n')
--       (\dst dstOff → unsafePrependDec' strategy dst dstOff n')
--       buffer

-- unsafePrependDec' ∷ ∀ s. Strategy → A.MArray s → Int → Integer → ST s Int
-- unsafePrependDec' s marr off@(I# off#) n' = case n' of
--   I.IS i# → Bounded.unsafePrependDec marr off (I# i#)
--   _ → unsafePrependBigNatDec' s marr (off# -# 1#) (integerToBigNat# n') >>= prependSign
--     where
--       prependSign !off' =
--         if n' < 0
--           then do
--             A.unsafeWrite marr (off' - 1) 0x2d -- '-'
--             pure (off - off' + 1)
--           else pure (off - off')
-- {-# INLINEABLE unsafePrependDec' #-}

-- unsafePrependBigNatDec' ∷ ∀ s. Strategy → A.MArray s → DigitsWriter s
-- unsafePrependBigNatDec' strategy marr !off0 !n0 = case strategy of
--   SmallOnly → prependSmallNat marr off0 n0
--   HugeOnly → prependHugeNat marr off0 n0
