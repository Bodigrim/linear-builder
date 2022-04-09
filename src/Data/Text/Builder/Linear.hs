{-# LANGUAGE RankNTypes #-}
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
  , (|>$)
  , ($<|)
  , (|>%)
  , (%<|)
  , (|>&)
  , (&<|)
  , liftText
    -- * Builder
  , Builder(..)
  , runBuilder
  , fromText
  , fromChar
  , fromAddr
  , fromDec
  , fromHex
  , fromDouble
  ) where

import Data.Bits
import Data.Foldable (forM_)
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

fromDec :: (Integral a, FiniteBits a) => a -> Builder
fromDec x = Builder $ \b -> b |>$ x
{-# INLINE fromDec #-}

fromHex :: (Integral a, FiniteBits a) => a -> Builder
fromHex x = Builder $ \b -> b |>& x
{-# INLINE fromHex #-}

fromDouble :: Double -> Builder
fromDouble x = Builder $ \b -> b |>% x
{-# INLINE fromDouble #-}

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

appendBounded :: Int -> (forall s. MArray s -> Int -> ST s Int) -> Buffer ⊸ Buffer
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

appendExact :: Int -> (forall s. MArray s -> Int -> ST s ()) -> Buffer ⊸ Buffer
appendExact srcLen appender = appendBounded
  srcLen
  (\dst dstOff -> appender dst dstOff >> pure srcLen)
{-# INLINE appendExact #-}

prependBounded
  :: Int
  -> (forall s. MArray s -> Int -> ST s Int)
  -> (forall s. MArray s -> Int -> ST s Int)
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

prependExact :: Int -> (forall s. MArray s -> Int -> ST s ()) -> Buffer ⊸ Buffer
prependExact srcLen appender = prependBounded
  srcLen
  (\dst dstOff -> appender dst (dstOff - srcLen) >> pure srcLen)
  (\dst dstOff -> appender dst dstOff >> pure srcLen)
{-# INLINE prependExact #-}

-- | Append 'Text' suffix to a 'Buffer' by mutating it.
-- If a suffix is statically known, consider using '(|>#)' for optimal performance.
--
-- >>> :set -XOverloadedStrings -XLinearTypes
-- >>> runBuffer (\b -> b |> "foo" |> "bar")
-- "foobar"
--
(|>) ∷ Buffer ⊸ Text → Buffer
infixl 6 |>
buffer |> (Text src srcOff srcLen) = appendExact
  srcLen
  (\dst dstOff -> A.copyI srcLen dst dstOff src srcOff)
  buffer

-- | Append 'Char' to a 'Buffer' by mutating it.
--
-- >>> :set -XLinearTypes
-- >>> runBuffer (\b -> b |>. 'q' |>. 'w')
-- "qw"
--
(|>.) ∷ Buffer ⊸ Char → Buffer
infixl 6 |>.
buffer |>. ch = appendBounded 4 (\dst dstOff -> unsafeWrite dst dstOff ch) buffer

-- | Prepend 'Text' prefix to a 'Buffer' by mutating it.
-- If a prefix is statically known, consider using '(<|#)' for optimal performance.
--
-- >>> :set -XOverloadedStrings -XLinearTypes
-- >>> runBuffer (\b -> "foo" <| "bar" <| b)
-- "foobar"
--
(<|) ∷ Text → Buffer ⊸ Buffer
infixr 6 <|
Text src srcOff srcLen <| buffer = prependExact
  srcLen
  (\dst dstOff -> A.copyI srcLen dst dstOff src srcOff)
  buffer

-- | Prepend 'Char' to a 'Buffer' by mutating it.
--
-- >>> :set -XLinearTypes
-- >>> runBuffer (\b -> 'q' .<| 'w' .<| b)
-- "qw"
--
(.<|) ∷ Char → Buffer ⊸ Buffer
infixr 6 .<|
ch .<| buffer = prependBounded
  4
  (\dst dstOff -> unsafePrependCharM dst dstOff ch)
  (\dst dstOff -> unsafeWrite dst dstOff ch)
  buffer

unsafeThaw ∷ Array → ST s (MArray s)
unsafeThaw (ByteArray a) = ST $ \s# →
  (# s#, MutableByteArray (unsafeCoerce# a) #)

sizeofByteArray :: Array -> Int
sizeofByteArray (ByteArray a) = I# (sizeofByteArray# a)

-- | Similar to 'Data.Text.Internal.Unsafe.Char.unsafeWrite',
-- but writes _before_ a given offset.
unsafePrependCharM :: A.MArray s -> Int -> Char -> ST s Int
unsafePrependCharM marr off c = case utf8Length c of
  1 -> do
    let n0 = fromIntegral (ord c)
    A.unsafeWrite marr (off - 1) n0
    pure 1
  2 -> do
    let (n0, n1) = ord2 c
    A.unsafeWrite marr (off - 2) n0
    A.unsafeWrite marr (off - 1) n1
    pure 2
  3 -> do
    let (n0, n1, n2) = ord3 c
    A.unsafeWrite marr (off - 3) n0
    A.unsafeWrite marr (off - 2) n1
    A.unsafeWrite marr (off - 1) n2
    pure 3
  _ -> do
    let (n0, n1, n2, n3) = ord4 c
    A.unsafeWrite marr (off - 4) n0
    A.unsafeWrite marr (off - 3) n1
    A.unsafeWrite marr (off - 2) n2
    A.unsafeWrite marr (off - 1) n3
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
buffer |># addr# = appendExact
  srcLen
  (\dst dstOff -> A.copyFromPointer dst dstOff (Ptr addr#) srcLen)
  buffer
  where
    srcLen = I# (cstringLength# addr#)

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
addr# <|# buffer = prependExact
  srcLen
  (\dst dstOff -> A.copyFromPointer dst dstOff (Ptr addr#) srcLen)
  buffer
  where
    srcLen = I# (cstringLength# addr#)

-- | Append decimal number.
(|>$) :: FiniteBits a => Buffer ⊸ a -> Buffer
infixl 6 |>$
(|>$) = undefined

-- | Prepend decimal number.
($<|) :: FiniteBits a => a -> Buffer ⊸ Buffer
infixr 6 $<|
($<|) = undefined

-- | Append double.
(|>%) :: Buffer ⊸ Double -> Buffer
infixl 6 |>%
(|>%) = undefined

-- | Prepend double
(%<|) :: Double -> Buffer ⊸ Buffer
infixr 6 %<|
(%<|) = undefined

-- | Append hexadecimal number.
(|>&) :: (Integral a, FiniteBits a) => Buffer ⊸ a -> Buffer
infixl 6 |>&
buffer |>& n = appendBounded
  (finiteBitSize n `shiftR` 2)
  (\dst dstOff -> unsafeAppendHex dst dstOff n)
  buffer

-- | Prepend hexadecimal number.
(&<|) :: (Integral a, FiniteBits a) => a -> Buffer ⊸ Buffer
infixr 6 &<|
n &<| buffer = prependBounded
  (finiteBitSize n `shiftR` 2)
  (\dst dstOff -> unsafePrependHex dst dstOff n)
  (\dst dstOff -> unsafeAppendHex dst dstOff n)
  buffer

unsafeAppendHex :: (Integral a, FiniteBits a) => A.MArray s -> Int -> a -> ST s Int
unsafeAppendHex marr off n = do
  let len = lengthAsHex n
  forM_ [0 .. len - 1] $ \i ->
    let nibble = (n `shiftR` ((len - 1 - i) `shiftL` 2)) .&. 0xf in
      writeNibbleAsHex marr (off + i) (fromIntegral nibble)
  pure len

unsafePrependHex :: (Integral a, FiniteBits a) => A.MArray s -> Int -> a -> ST s Int
unsafePrependHex marr off n = do
  let len = lengthAsHex n
  forM_ [0 .. len - 1] $ \i ->
    let nibble = (n `shiftR` (i `shiftL` 2)) .&. 0xf in
      writeNibbleAsHex marr (off - 1 - i) (fromIntegral nibble)
  pure len

lengthAsHex :: FiniteBits a => a -> Int
lengthAsHex n = max1 $ (finiteBitSize n `shiftR` 2) - (countLeadingZeros n `shiftR` 2)

-- Branchless equivalent for max 1 n.
max1 :: Int -> Int
max1 n@(I# n#) = n `xor` I# (n# <=# 0#)

writeNibbleAsHex :: A.MArray s -> Int -> Int -> ST s ()
writeNibbleAsHex marr off n@(I# n#) = A.unsafeWrite marr off (fromIntegral hex)
  where
    hex = 48 + n + I# (n# ># 9#) * 39
