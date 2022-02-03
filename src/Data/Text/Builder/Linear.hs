-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>

module Data.Text.Builder.Linear
  ( Builder
  , unBuilder
  , runBuilder
  , dupBuilder
  , (.<>)
  , (<>.)
  , (.<>.)
  ) where

import Data.Text ()
import qualified Data.Text as T
import Data.Text.Array (Array(..), MArray(..))
import qualified Data.Text.Array as A
import Data.Text.Internal (Text(..))
import GHC.Exts
import GHC.ST
import Unsafe.Coerce

-- | Builder for Text based on linear types.
--
-- >>> :set -XOverloadedStrings -XLinearTypes
-- >>> runBuilder $ \b -> unBuilder ("foo" <>. (b .<> "bar"))
-- "foobar"
--
newtype Builder = Builder Text

unBuilder ∷ Builder ⊸ Text
unBuilder (Builder x) = x

-- | Run a function on an empty builder, producing text.
runBuilder ∷ (Builder ⊸ a) → a
runBuilder f = f (Builder mempty)

dupBuilder ∷ Builder ⊸ (Builder, Builder)
dupBuilder = unsafeCoerce dup

dup ∷ Builder → (Builder, Builder)
dup (Builder x) = (Builder x, Builder (T.copy x))

-- | Append text to a buffer.
(.<>) ∷ Builder ⊸ Text → Builder
(.<>) = unsafeCoerce unsafeAppend
infixl 6 .<>

unsafeAppend ∷ Builder → Text → Builder
unsafeAppend (Builder (Text dst@(ByteArray dst#) dstOff dstLen)) (Text src srcOff srcLen) = runST $ do
  let dstFullLen = I# (sizeofByteArray# dst#)
      newLen = dstLen + srcLen
      newFullLen = dstOff + 2 * newLen
  newM ← if dstOff + newLen <= dstFullLen
    then unsafeThaw dst
    else do
      tmpM ← A.new newFullLen
      A.copyI dstLen tmpM dstOff dst dstOff
      pure tmpM
  A.copyI srcLen newM (dstOff + dstLen) src srcOff
  new ← A.unsafeFreeze newM
  pure $ Builder $ Text new dstOff newLen

-- | Prepend text to a buffer.
(<>.) ∷ Text → Builder ⊸ Builder
(<>.) = unsafeCoerce unsafePrepend
infixr 6 <>.

unsafePrepend ∷ Text → Builder → Builder
unsafePrepend (Text src srcOff srcLen) (Builder (Text dst@(ByteArray dst#) dstOff dstLen))
  | srcLen <= dstOff = runST $ do
    newM ← unsafeThaw dst
    A.copyI srcLen newM (dstOff - srcLen) src srcOff
    new ← A.unsafeFreeze newM
    pure $ Builder $ Text new (dstOff - srcLen) (srcLen + dstLen)
  | otherwise = runST $ do
    let dstFullLen = I# (sizeofByteArray# dst#)
        newLen = dstLen + srcLen
        newFullLen = 2 * newLen + (dstFullLen - dstOff - dstLen)
    newM ← A.new newFullLen
    A.copyI srcLen newM newLen src srcOff
    A.copyI dstLen newM (newLen + srcLen) dst dstOff
    new ← A.unsafeFreeze newM
    pure $ Builder $ Text new newLen newLen

unsafeThaw ∷ Array → ST s (MArray s)
unsafeThaw (ByteArray a) = ST $ \s# →
  (# s#, MutableByteArray (unsafeCoerce# a) #)

-- | Concatenate buffers.
(.<>.) ∷ Builder ⊸ Builder ⊸ Builder
(.<>.) = unsafeCoerce undefined
infix 6 .<>.
