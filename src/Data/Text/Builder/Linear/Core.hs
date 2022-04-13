-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Low-level routines for 'Buffer' manipulations.

module Data.Text.Builder.Linear.Core
  ( Buffer
  , runBuffer
  , dupBuffer
  , consumeBuffer
  , eraseBuffer
  , appendBounded
  , appendExact
  , prependBounded
  , prependExact
  , (><)
  ) where

import qualified Data.Text as T
import Data.Text.Array (Array(..), MArray(..))
import qualified Data.Text.Array as A
import Data.Text.Internal (Text(..))
import GHC.Exts
import GHC.ST

-- | Internally 'Buffer' is a mutable buffer.
-- If a client gets hold of a variable of type 'Buffer',
-- they'd be able to pass a mutable buffer to concurrent threads.
-- That's why API below is carefully designed to prevent such possibility:
-- clients always work with linear functions 'Buffer' ⊸ 'Buffer' instead
-- and run them on an empty 'Buffer' to extract results.
--
-- In terms of @linear-base@ 'Buffer' is @Consumable@ (see 'consumeBuffer')
-- and @Dupable@ (see 'dupBuffer'), but not @Movable@.
--
-- >>> :set -XOverloadedStrings -XLinearTypes
-- >>> runBuffer (\b -> '!' .<| "foo" <| (b |> "bar" |>. '.'))
-- "!foobar."
--
-- Remember: this is a strict builder, so on contrary to "Data.Text.Lazy.Builder"
-- for optimal performance you should use strict left folds instead of lazy right ones.
--
-- Starting from GHC 9.2, 'Buffer' is an unlifted datatype,
-- so you can put it into an unboxed tuple @(# ..., ... #)@,
-- but not into @(..., ...)@.
--
#if MIN_VERSION_base(4,16,0)
data Buffer :: TYPE ('BoxedRep 'Unlifted) where
#else
data Buffer where
#endif
  Buffer :: {-# UNPACK #-} !Text -> Buffer

-- | Unwrap 'Buffer', no-op.
-- Most likely, this is not the function you're looking for
-- and you need 'runBuffer' instead.
unBuffer ∷ Buffer ⊸ Text
unBuffer (Buffer x) = x

-- | Run a linear function on an empty 'Buffer', producing 'Text'.
--
-- Be careful to write @runBuffer (\b -> ...)@ instead of @runBuffer $ \b -> ...@,
-- because current implementation of linear types lacks special support for '($)'.
-- Another option is to enable @{-# LANGUAGE BlockArguments #-}@
-- and write @runBuffer \b -> ...@.
-- Alternatively, you can import @Prelude.Linear.($)@ from @linear-base@.
--
-- 'runBuffer' is similar in spirit to mutable arrays API in @Data.Array.Mutable.Linear@,
-- which provides functions like @fromList@ ∷ [@a@] -> (@Vector@ @a@ ⊸ @Ur@ b) ⊸ @Ur@ @b@.
-- Here the initial buffer is always empty and @b@ is 'Text'. Since 'Text' is @Movable@,
-- 'Text' and @Ur@ 'Text' are equivalent.
--
runBuffer ∷ (Buffer ⊸ Buffer) ⊸ Text
runBuffer f = unBuffer (f (Buffer mempty))

-- | Duplicate builder. Feel free to process results in parallel threads.
-- Similar to @Data.Unrestricted.Linear.Dupable@ from @linear-base@.
--
-- It is a bit tricky to use because of
-- <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/linear_types.html#limitations current limitations>
-- of linear types with regards to @let@ and @where@. E. g., one cannot write
--
-- > let (# b1, b2 #) = dupBuffer b in ("foo" <| b1) >< (b2 |> "bar")
--
-- Instead write:
--
-- >>> :set -XOverloadedStrings -XLinearTypes -XUnboxedTuples
-- >>> runBuffer (\b -> (\(# b1, b2 #) -> ("foo" <| b1) >< (b2 |> "bar")) (dupBuffer b))
-- "foobar"
--
-- Note the unboxed tuple: starting from GHC 9.2, 'Buffer' is an unlifted datatype,
-- so it cannot be put into @(..., ...)@.
--
dupBuffer ∷ Buffer ⊸ (# Buffer, Buffer #)
dupBuffer (Buffer x) = (# Buffer x, Buffer (T.copy x) #)

-- | Consume buffer linearly,
-- similar to @Data.Unrestricted.Linear.Consumable@ from @linear-base@.
consumeBuffer :: Buffer ⊸ ()
consumeBuffer Buffer{} = ()

-- | Erase buffer's content, replacing it with an empty 'Text'.
eraseBuffer :: Buffer ⊸ Buffer
eraseBuffer Buffer{} = Buffer mempty

-- | Low-level routine to append data of unknown size to a 'Buffer'.
appendBounded
  :: Int
  -- ^ Upper bound for the number of bytes, written by an action
  -> (forall s. MArray s -> Int -> ST s Int)
  -- ^ Action, which writes bytes starting from the given offset
  -- and returns an actual number of bytes written.
  -> Buffer
  ⊸ Buffer
appendBounded maxSrcLen appender (Buffer (Text dst dstOff dstLen)) = Buffer $ runST $ do
  let dstFullLen = sizeofByteArray dst
      newFullLen = dstOff + 2 * (dstLen + maxSrcLen)
  newM ← if dstOff + dstLen + maxSrcLen <= dstFullLen
    then unsafeThaw dst
    else do
      tmpM ← A.new newFullLen
      A.copyI dstLen tmpM dstOff dst dstOff
      pure tmpM
  srcLen ← appender newM (dstOff + dstLen)
  new ← A.unsafeFreeze newM
  pure $ Text new dstOff (dstLen + srcLen)
{-# INLINE appendBounded #-}

-- | Low-level routine to append data of known size to a 'Buffer'.
appendExact
  :: Int
  -- ^ Exact number of bytes, written by an action
  -> (forall s. MArray s -> Int -> ST s ())
  -- ^ Action, which writes bytes starting from the given offset
  -> Buffer
  ⊸ Buffer
appendExact srcLen appender = appendBounded
  srcLen
  (\dst dstOff -> appender dst dstOff >> pure srcLen)
{-# INLINE appendExact #-}

-- | Low-level routine to prepend data of unknown size to a 'Buffer'.
prependBounded
  :: Int
  -- ^ Upper bound for the number of bytes, written by an action
  -> (forall s. MArray s -> Int -> ST s Int)
  -- ^ Action, which writes bytes finishing before the given offset
  -- and returns an actual number of bytes written.
  -> (forall s. MArray s -> Int -> ST s Int)
  -- ^ Action, which writes bytes starting from the given offset
  -- and returns an actual number of bytes written.
  -> Buffer
   ⊸ Buffer
prependBounded maxSrcLen prepender appender (Buffer (Text dst dstOff dstLen))
  | maxSrcLen <= dstOff = Buffer $ runST $ do
    newM ← unsafeThaw dst
    srcLen ← prepender newM dstOff
    new ← A.unsafeFreeze newM
    pure $ Text new (dstOff - srcLen) (srcLen + dstLen)
  | otherwise = Buffer $ runST $ do
    let dstFullLen = sizeofByteArray dst
        newOff = dstLen + maxSrcLen
        newFullLen = 2 * newOff + (dstFullLen - dstOff - dstLen)
    newM ← A.new newFullLen
    srcLen ← appender newM newOff
    A.copyI dstLen newM (newOff + srcLen) dst dstOff
    new ← A.unsafeFreeze newM
    pure $ Text new newOff (dstLen + srcLen)
{-# INLINE prependBounded #-}

-- | Low-level routine to append data of unknown size to a 'Buffer'.
prependExact
  :: Int
  -- ^ Exact number of bytes, written by an action
  -> (forall s. MArray s -> Int -> ST s ())
  -- ^ Action, which writes bytes finishing before the given offset
  -> Buffer
  ⊸ Buffer
prependExact srcLen appender = prependBounded
  srcLen
  (\dst dstOff -> appender dst (dstOff - srcLen) >> pure srcLen)
  (\dst dstOff -> appender dst dstOff >> pure srcLen)
{-# INLINE prependExact #-}

unsafeThaw ∷ Array → ST s (MArray s)
unsafeThaw (ByteArray a) = ST $ \s# →
  (# s#, MutableByteArray (unsafeCoerce# a) #)

sizeofByteArray :: Array -> Int
sizeofByteArray (ByteArray a) = I# (sizeofByteArray# a)

-- | Concatenate two 'Buffer's, potentially mutating both of them.
--
-- You likely need to use 'dupBuffer' to get hold on two builders at once:
--
-- >>> :set -XOverloadedStrings -XLinearTypes -XUnboxedTuples
-- >>> runBuffer (\b -> (\(# b1, b2 #) -> ("foo" <| b1) >< (b2 |> "bar")) (dupBuffer b))
-- "foobar"
--
(><) ∷ Buffer ⊸ Buffer ⊸ Buffer
infix 6 ><
Buffer (Text left leftOff leftLen) >< Buffer (Text right rightOff rightLen) = Buffer $ runST $ do
  let leftFullLen = sizeofByteArray left
      rightFullLen = sizeofByteArray right
      canCopyToLeft = leftOff + leftLen + rightLen <= leftFullLen
      canCopyToRight = leftLen <= rightOff
      shouldCopyToLeft = canCopyToLeft && (not canCopyToRight || leftLen >= rightLen)
  if shouldCopyToLeft then do
    newM ← unsafeThaw left
    A.copyI rightLen newM (leftOff + leftLen) right rightOff
    new ← A.unsafeFreeze newM
    pure $ Text new leftOff (leftLen + rightLen)
  else if canCopyToRight then do
    newM ← unsafeThaw right
    A.copyI leftLen newM (rightOff - leftLen) left leftOff
    new ← A.unsafeFreeze newM
    pure $ Text new (rightOff - leftLen) (leftLen + rightLen)
  else do
    let fullLen = leftOff + leftLen + rightLen + (rightFullLen - rightOff - rightLen)
    newM ← A.new fullLen
    A.copyI leftLen newM leftOff left leftOff
    A.copyI rightLen newM (leftOff + leftLen) right rightOff
    new ← A.unsafeFreeze newM
    pure $ Text new leftOff (leftLen + rightLen)
