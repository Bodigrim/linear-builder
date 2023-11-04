{-# LANGUAGE CPP #-}

-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
--              (c) 2023 Pierre Le Marre
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Low-level routines for 'A.MArray' manipulations.
module Data.Text.Builder.Linear.Array (
  unsafeThaw,
  sizeofByteArray,
  isPinned,
  unsafeTile,
  unsafeReplicate,
) where

import Data.Text.Array qualified as A
import GHC.Exts (Int (..), isByteArrayPinned#, isTrue#, setByteArray#, sizeofByteArray#)
import GHC.ST (ST (..))

#if __GLASGOW_HASKELL__ >= 909
import GHC.Exts (unsafeThawByteArray#)
#else
import GHC.Exts (unsafeCoerce#)
#endif

unsafeThaw ∷ A.Array → ST s (A.MArray s)
#if __GLASGOW_HASKELL__ >= 909
unsafeThaw (A.ByteArray a) = ST $ \s# → case unsafeThawByteArray# a s# of
  (# s'#, ma #) -> (# s'#, A.MutableByteArray ma #)
#else
unsafeThaw (A.ByteArray a) = ST $ \s# →
  (# s#, A.MutableByteArray (unsafeCoerce# a) #)
#endif

sizeofByteArray ∷ A.Array → Int
sizeofByteArray (A.ByteArray a) = I# (sizeofByteArray# a)

isPinned ∷ A.Array → Bool
isPinned (A.ByteArray a) = isTrue# (isByteArrayPinned# a)

-- | Replicate an ASCII character
--
-- __Warning:__ it is the responsibility of the caller to ensure that the 'Int'
-- is a valid ASCII character.
unsafeReplicate
  ∷ A.MArray s
  -- ^ Mutable array
  → Int
  -- ^ Offset
  → Int
  -- ^ Count
  → Int
  -- ^ ASCII character
  → ST s ()
unsafeReplicate (A.MutableByteArray dst#) (I# dstOff#) (I# count#) (I# w#) =
  ST (\s# → (# setByteArray# dst# dstOff# count# w# s#, () #))
{-# INLINE unsafeReplicate #-}

-- | Duplicate a portion of an array in-place.
--
-- Example of use:
--
-- @
-- -- Write @count@ times the char @c@
-- let cLen = utf8Length c; totalLen = cLen * count
-- in unsafeWrite dst dstOff ch *> 'unsafeTile' dst dstOff totalLen cLen
-- @
unsafeTile
  ∷ A.MArray s
  -- ^ Mutable array
  → Int
  -- ^ Start of the portion to duplicate
  → Int
  -- ^ Total length of the duplicate
  → Int
  -- ^ Length of the portion to duplicate
  → ST s ()
unsafeTile dest destOff totalLen = go
  where
    -- Adapted from Data.Text.Array.tile
    go l
      | 2 * l > totalLen = A.copyM dest (destOff + l) dest destOff (totalLen - l)
      | otherwise = A.copyM dest (destOff + l) dest destOff l *> go (2 * l)
{-# INLINE unsafeTile #-}
