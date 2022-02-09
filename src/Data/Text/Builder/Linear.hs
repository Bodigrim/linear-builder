-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- 'Buffer' for strict 'Text', based on linear types.
-- It's consistently outperforms
-- 'Data.Text.Lazy.toStrict' . 'Data.Text.Lazy.Builder.toLazyText'
-- and scales better:
--
-- @
-- 1
--   Data.Text.Lazy.Builder:
--     73.9 ns ± 6.8 ns
--   Data.Text.Builder.Linear:
--     33.0 ns ± 2.2 ns, 0.45x
-- 10
--   Data.Text.Lazy.Builder:
--     702  ns ±  70 ns
--   Data.Text.Builder.Linear:
--     178  ns ±  16 ns, 0.25x
-- 100
--   Data.Text.Lazy.Builder:
--     6.57 μs ± 292 ns
--   Data.Text.Builder.Linear:
--     1.58 μs ± 116 ns, 0.24x
-- 1000
--   Data.Text.Lazy.Builder:
--     74.7 μs ± 2.9 μs
--   Data.Text.Builder.Linear:
--     13.9 μs ± 1.2 μs, 0.19x
-- 10000
--   Data.Text.Lazy.Builder:
--     1.63 ms ±  64 μs
--   Data.Text.Builder.Linear:
--     220  μs ±  16 μs, 0.13x
-- 100000
--   Data.Text.Lazy.Builder:
--     26.2 ms ± 2.5 ms
--   Data.Text.Builder.Linear:
--     3.25 ms ± 294 μs, 0.12x
-- 1000000
--   Data.Text.Lazy.Builder:
--     302  ms ± 8.9 ms
--   Data.Text.Builder.Linear:
--     30.8 ms ± 3.1 ms, 0.10x
-- @

module Data.Text.Builder.Linear
  ( -- * Buffer
    Buffer
  , runBuffer
  , dupBuffer
  , (|>)
  , (|>.)
  , (|>#)
  , (<|)
  , (.<|)
  , (<|#)
  , (><)
  , liftText
    -- * Builder
  , Builder(..)
  , runBuilder
  , fromText
  , fromChar
  , fromAddr
  ) where

import Data.Text ()
import qualified Data.Text as T
import Data.Text.Array (Array(..), MArray(..))
import qualified Data.Text.Array as A
import Data.Text.Internal (Text(..))
import Data.Text.Internal.Encoding.Utf8 (utf8Length, ord2, ord3, ord4)
import Data.Text.Internal.Unsafe.Char (unsafeWrite, ord)
import GHC.Exts
import GHC.ST

-- | Thin wrapper over 'Buffer' with a handy 'Semigroup' instance.
--
-- >>> :set -XOverloadedStrings -XMagicHash
-- >>> fromText "foo" <> fromChar '_' <> fromAddr "bar"#
-- "foo_bar"
--
newtype Builder = Builder { unBuilder :: Buffer ⊸ Buffer }

-- | Run 'Builder' computation on an empty 'Buffer', returning 'Text'.
--
-- >>> :set -XOverloadedStrings -XMagicHash
-- >>> runBuilder (fromText "foo" <> fromChar '_' <> fromAddr "bar"#)
-- "foo_bar"
--
-- Be careful to write @runBuilder (\b -> ...)@ instead of @runBuilder $ \b -> ...@,
-- because current implementation of linear types lacks special support for '($)'.
--
runBuilder :: Builder ⊸ Text
runBuilder (Builder f) = runBuffer f
{-# INLINE runBuilder #-}

instance Show Builder where
  show (Builder f) = show (runBuffer f)

instance Semigroup Builder where
  Builder f <> Builder g = Builder $ \b -> g (f b)
  {-# INLINE (<>) #-}

instance Monoid Builder where
  mempty = Builder (\b -> b)
  {-# INLINE mempty #-}

-- | Create 'Builder', containing a given 'Text'.
--
-- >>> :set -XOverloadedStrings
-- >>> fromText "foo" <> fromText "bar"
-- "foobar"
--
fromText :: Text -> Builder
fromText x = Builder $ \b -> b |> x
{-# INLINE fromText #-}

-- | Create 'Builder', containing a given 'Char'.
--
-- >>> fromChar 'x' <> fromChar 'y'
-- "xy"
--
fromChar :: Char -> Builder
fromChar x = Builder $ \b -> b |>. x
{-# INLINE fromChar #-}

-- | Create 'Builder', containing a null-terminated UTF-8 string, specified by 'Addr#'.
--
-- >>> :set -XMagicHash
-- >>> fromAddr "foo"# <> fromAddr "bar"#
-- "foobar"
--
fromAddr :: Addr# -> Builder
fromAddr x = Builder $ \b -> b |># x
{-# INLINE fromAddr #-}

-- | Internally 'Buffer' is a mutable buffer.
-- If a client gets hold of a variable of type 'Buffer',
-- they'd be able to pass a mutable buffer to concurrent threads.
-- That's why API below is carefully designed to prevent such possibility:
-- clients always work with linear functions 'Buffer' ⊸ 'Buffer' instead
-- and run them on an empty 'Buffer' to extract results.
--
-- >>> :set -XOverloadedStrings -XLinearTypes
-- >>> runBuffer (\b -> '!' .<| "foo" <| (b |> "bar" |>. '.'))
-- "!foobar."
--
-- Remember: this is a strict builder, so on contrary to 'Data.Text.Lazy.Builder.Buffer'
-- for optimal performance you should use strict left folds instead of lazy right ones.
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
--
runBuffer ∷ (Buffer ⊸ Buffer) ⊸ Text
runBuffer f = unBuffer (f (Buffer mempty))

-- | Duplicate builder. Feel free to process results in parallel threads.
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
dupBuffer ∷ Buffer ⊸ (# Buffer, Buffer #)
dupBuffer (Buffer x) = (# Buffer x, Buffer (T.copy x) #)

-- | Append 'Text' suffix to a 'Buffer' by mutating it.
-- If a suffix is statically known, consider using '(|>#)' for optimal performance.
--
-- >>> :set -XOverloadedStrings -XLinearTypes
-- >>> runBuffer (\b -> b |> "foo" |> "bar")
-- "foobar"
--
(|>) ∷ Buffer ⊸ Text → Buffer
infixl 6 |>
Buffer (Text dst dstOff dstLen) |> (Text src srcOff srcLen) = Buffer $ runST $ do
  let dstFullLen = sizeofByteArray dst
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
  pure $ Text new dstOff newLen

-- | Append 'Char' to a 'Buffer' by mutating it.
--
-- >>> :set -XLinearTypes
-- >>> runBuffer (\b -> b |>. 'q' |>. 'w')
-- "qw"
--
(|>.) ∷ Buffer ⊸ Char → Buffer
infixl 6 |>.
Buffer (Text dst dstOff dstLen) |>. ch = Buffer $ runST $ do
  let dstFullLen = sizeofByteArray dst
      maxSrcLen = 4
      newFullLen = dstOff + 2 * (dstLen + maxSrcLen)
  newM ← if dstOff + dstLen + maxSrcLen <= dstFullLen
    then unsafeThaw dst
    else do
      tmpM ← A.new newFullLen
      A.copyI dstLen tmpM dstOff dst dstOff
      pure tmpM
  srcLen ← unsafeWrite newM (dstOff + dstLen) ch
  new ← A.unsafeFreeze newM
  pure $ Text new dstOff (dstLen + srcLen)

-- | Prepend 'Text' prefix to a 'Buffer' by mutating it.
-- If a prefix is statically known, consider using '(<|#)' for optimal performance.
--
-- >>> :set -XOverloadedStrings -XLinearTypes
-- >>> runBuffer (\b -> "foo" <| "bar" <| b)
-- "foobar"
--
(<|) ∷ Text → Buffer ⊸ Buffer
infixr 6 <|
Text src srcOff srcLen <| Buffer (Text dst dstOff dstLen) = Buffer $ case () of
  () | srcLen <= dstOff ->
    (\new -> Text new (dstOff - srcLen) (srcLen + dstLen)) $ runST $ do
      newM ← unsafeThaw dst
      A.copyI srcLen newM (dstOff - srcLen) src srcOff
      A.unsafeFreeze newM
  () | otherwise -> let newLen = dstLen + srcLen in
    (\new -> Text new newLen newLen) $ runST $ do
      let dstFullLen = sizeofByteArray dst
          newFullLen = 2 * newLen + (dstFullLen - dstOff - dstLen)
      newM ← A.new newFullLen
      A.copyI srcLen newM newLen src srcOff
      A.copyI dstLen newM (newLen + srcLen) dst dstOff
      A.unsafeFreeze newM

-- | Prepend 'Char' to a 'Buffer' by mutating it.
--
-- >>> :set -XLinearTypes
-- >>> runBuffer (\b -> 'q' .<| 'w' .<| b)
-- "qw"
--
(.<|) ∷ Char → Buffer ⊸ Buffer
infixr 6 .<|
ch .<| Buffer (Text dst dstOff dstLen)
  | maxSrcLen <= dstOff = Buffer $ runST $ do
    newM ← unsafeThaw dst
    srcLen ← unsafePrependCharM newM dstOff ch
    new ← A.unsafeFreeze newM
    pure $ Text new (dstOff - srcLen) (srcLen + dstLen)
  | otherwise = Buffer $ runST $ do
    let dstFullLen = sizeofByteArray dst
        newOff = dstLen + maxSrcLen
        newFullLen = 2 * newOff + (dstFullLen - dstOff - dstLen)
    newM ← A.new newFullLen
    srcLen ← unsafeWrite newM newOff ch
    A.copyI dstLen newM (newOff + srcLen) dst dstOff
    new ← A.unsafeFreeze newM
    pure $ Text new newOff (dstLen + srcLen)
  where
    maxSrcLen = 4

unsafeThaw ∷ Array → ST s (MArray s)
unsafeThaw (ByteArray a) = ST $ \s# →
  (# s#, MutableByteArray (unsafeCoerce# a) #)

sizeofByteArray :: Array -> Int
sizeofByteArray (ByteArray a) = I# (sizeofByteArray# a)

-- | Similar to 'Data.Text.Internal.Unsafe.Char.unsafeWrite',
-- but writes _before_ a given offset.
unsafePrependCharM :: A.MArray s -> Int -> Char -> ST s Int
unsafePrependCharM marr i c = case utf8Length c of
  1 -> do
    let n0 = fromIntegral (ord c)
    A.unsafeWrite marr (i - 1) n0
    pure 1
  2 -> do
    let (n0, n1) = ord2 c
    A.unsafeWrite marr (i - 2) n0
    A.unsafeWrite marr (i - 1) n1
    pure 2
  3 -> do
    let (n0, n1, n2) = ord3 c
    A.unsafeWrite marr (i - 3) n0
    A.unsafeWrite marr (i - 2) n1
    A.unsafeWrite marr (i - 1) n2
    pure 3
  _ -> do
    let (n0, n1, n2, n3) = ord4 c
    A.unsafeWrite marr (i - 4) n0
    A.unsafeWrite marr (i - 3) n1
    A.unsafeWrite marr (i - 2) n2
    A.unsafeWrite marr (i - 1) n3
    pure 4

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

-- | Lift a linear function on 'Text' to 'Buffer's.
-- This is not very useful at the moment, because @text@ does not provide
-- any linear functions at all.
liftText ∷ (Text ⊸ Text) → (Buffer ⊸ Buffer)
liftText f (Buffer x) = Buffer (f x)

-- | Append a null-terminated UTF-8 string
-- to a 'Buffer' by mutating it. E. g.,
--
-- >>> :set -XOverloadedStrings -XLinearTypes -XMagicHash
-- >>> runBuffer (\b -> b |># "foo"# |># "bar"#)
-- "foobar"
--
-- The literal string must not contain zero bytes @\\0@, this condition is not checked.
(|>#) ∷ Buffer ⊸ Addr# → Buffer
infixl 6 |>#
Buffer (Text dst dstOff dstLen) |># addr# = Buffer $ runST $ do
  let dstFullLen = sizeofByteArray dst
      srcLen = I# (cstringLength# addr#)
      newLen = dstLen + srcLen
      newFullLen = dstOff + 2 * newLen
  newM ← if dstOff + newLen <= dstFullLen
    then unsafeThaw dst
    else do
      tmpM ← A.new newFullLen
      A.copyI dstLen tmpM dstOff dst dstOff
      pure tmpM
  A.copyFromPointer newM (dstOff + dstLen) (Ptr addr#) srcLen
  new ← A.unsafeFreeze newM
  pure $ Text new dstOff newLen

-- | Prepend a null-terminated UTF-8 string
-- to a 'Buffer' by mutating it. E. g.,
--
-- >>> :set -XOverloadedStrings -XLinearTypes -XMagicHash
-- >>> runBuffer (\b -> "foo"# <|# "bar"# <|# b)
-- "foobar"
--
-- The literal string must not contain zero bytes @\\0@, this condition is not checked.
(<|#) ∷ Addr# → Buffer ⊸ Buffer
infixr 6 <|#
addr# <|# Buffer (Text dst dstOff dstLen) = let srcLen = I# (cstringLength# addr#) in Buffer $ case () of
  () | srcLen <= dstOff ->
    (\new -> Text new (dstOff - srcLen) (srcLen + dstLen)) $ runST $ do
      newM ← unsafeThaw dst
      A.copyFromPointer newM (dstOff - srcLen) (Ptr addr#) srcLen
      A.unsafeFreeze newM
  () | otherwise -> let newLen = dstLen + srcLen in
    (\new -> Text new newLen newLen) $ runST $ do
      let dstFullLen = sizeofByteArray dst
          newFullLen = 2 * newLen + (dstFullLen - dstOff - dstLen)
      newM ← A.new newFullLen
      A.copyFromPointer newM newLen (Ptr addr#) srcLen
      A.copyI dstLen newM (newLen + srcLen) dst dstOff
      A.unsafeFreeze newM
