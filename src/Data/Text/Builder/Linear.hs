-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- 'Builder' for strict 'Text', based on linear types.
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
  ( Builder
  , runBuilder
  , dupBuilder
  , (|>)
  , (|>.)
  , (|>#)
  , (<|)
  , (.<|)
  , (<|#)
  , (><)
  , liftText
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

-- | Internally 'Builder' is a mutable buffer.
-- If a client gets hold of a variable of type 'Builder',
-- they'd be able to pass a mutable buffer to concurrent threads.
-- That's why API below is carefully designed to prevent such possibility:
-- clients always work with linear functions 'Builder' ⊸ 'Builder' instead
-- and run them on an empty 'Builder' to extract results.
--
-- >>> :set -XOverloadedStrings -XLinearTypes
-- >>> runBuilder $ \b → '!' .<| "foo" <| (b |> "bar" |>. '.')
-- "!foobar."
--
data Builder where
  Builder :: !Text -> Builder

-- | Unwrap 'Builder', no-op.
-- Most likely, this is not the function you're looking for
-- and you need 'runBuilder' instead.
unBuilder ∷ Builder ⊸ Text
unBuilder (Builder x) = x

-- | Run a linear function on an empty 'Builder', producing 'Text'.
runBuilder ∷ (Builder ⊸ Builder) -> Text
runBuilder f = unBuilder (f (Builder mempty))

-- | Duplicate builder. Feel free to process results in parallel threads.
--
-- It is a bit tricky to use because of
-- <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/linear_types.html#limitations current limitations>
-- of linear types with regards to @let@ and @where@. E. .g, one cannot write
--
-- > let (b1, b2) = dupBuilder b in ("foo" <| b1) >< (b2 |> "bar")
--
-- Instead write:
--
-- >>> :set -XOverloadedStrings -XLinearTypes
-- >>> runBuilder $ \b → (\(b1, b2) -> ("foo" <| b1) >< (b2 |> "bar")) (dupBuilder b)
-- "foobar"
--
dupBuilder ∷ Builder ⊸ (Builder, Builder)
dupBuilder (Builder x) = (Builder x, Builder (T.copy x))

-- | Append 'Text' to a 'Builder' by mutating it.
(|>) ∷ Builder ⊸ Text → Builder
infixl 6 |>
Builder (Text dst dstOff dstLen) |> (Text src srcOff srcLen) = Builder $ runST $ do
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

-- | Append 'Char' to a 'Builder' by mutating it.
(|>.) ∷ Builder ⊸ Char → Builder
infixl 6 |>.
Builder (Text dst dstOff dstLen) |>. ch = runST $ do
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
  pure $ Builder $ Text new dstOff (dstLen + srcLen)

-- | Prepend 'Text' to a 'Builder' by mutating it.
(<|) ∷ Text → Builder ⊸ Builder
infixr 6 <|
Text src srcOff srcLen <| Builder (Text dst dstOff dstLen) = Builder $ case () of
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

-- | Prepend 'Char' to a 'Builder' by mutating it.
(.<|) ∷ Char → Builder ⊸ Builder
infixr 6 .<|
ch .<| Builder (Text dst dstOff dstLen)
  | maxSrcLen <= dstOff = runST $ do
    newM ← unsafeThaw dst
    srcLen ← unsafePrependCharM newM dstOff ch
    new ← A.unsafeFreeze newM
    pure $ Builder $ Text new (dstOff - srcLen) (srcLen + dstLen)
  | otherwise = runST $ do
    let dstFullLen = sizeofByteArray dst
        newOff = dstLen + maxSrcLen
        newFullLen = 2 * newOff + (dstFullLen - dstOff - dstLen)
    newM ← A.new newFullLen
    srcLen ← unsafeWrite newM newOff ch
    A.copyI dstLen newM (newOff + srcLen) dst dstOff
    new ← A.unsafeFreeze newM
    pure $ Builder $ Text new newOff (dstLen + srcLen)
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

-- | Concatenate two 'Builder's, potentially mutating both of them.
--
-- You likely need to use 'dupBuilder' to get hold on two builders at once:
--
-- >>> :set -XOverloadedStrings -XLinearTypes
-- >>> runBuilder $ \b → (\(b1, b2) -> ("foo" <| b1) >< (b2 |> "bar")) (dupBuilder b)
-- "foobar"
--
(><) ∷ Builder ⊸ Builder ⊸ Builder
infix 6 ><
Builder (Text left leftOff leftLen) >< Builder (Text right rightOff rightLen) = runST $ do
  let leftFullLen = sizeofByteArray left
      rightFullLen = sizeofByteArray right
      canCopyToLeft = leftOff + leftLen + rightLen <= leftFullLen
      canCopyToRight = leftLen <= rightOff
      shouldCopyToLeft = canCopyToLeft && (not canCopyToRight || leftLen >= rightLen)
  if shouldCopyToLeft then do
    newM ← unsafeThaw left
    A.copyI rightLen newM (leftOff + leftLen) right rightOff
    new ← A.unsafeFreeze newM
    pure $ Builder $ Text new leftOff (leftLen + rightLen)
  else if canCopyToRight then do
    newM ← unsafeThaw right
    A.copyI leftLen newM (rightOff - leftLen) left leftOff
    new ← A.unsafeFreeze newM
    pure $ Builder $ Text new (rightOff - leftLen) (leftLen + rightLen)
  else do
    let fullLen = leftOff + leftLen + rightLen + (rightFullLen - rightOff - rightLen)
    newM ← A.new fullLen
    A.copyI leftLen newM leftOff left leftOff
    A.copyI rightLen newM (leftOff + leftLen) right rightOff
    new ← A.unsafeFreeze newM
    pure $ Builder $ Text new leftOff (leftLen + rightLen)

-- | Lift a linear function on 'Text' to 'Builder's.
-- This is not very useful at the moment, because @text@ does not provide
-- any linear functions at all.
liftText ∷ (Text ⊸ Text) → (Builder ⊸ Builder)
liftText f (Builder x) = Builder (f x)

-- | Append a statically known null-terminated ASCII string (cf. 'unpackCString#')
-- to a 'Builder' by mutating it. E. g.,
--
-- >>> :set -XOverloadedStrings -XLinearTypes -XMagicHash
-- >>> let bar# = "bar"# in runBuilder $ \b → "foo" <| (b |># bar#)
-- "foobar"
--
(|>#) ∷ Builder ⊸ Addr# → Builder
infixl 6 |>#
Builder (Text dst dstOff dstLen) |># addr# = Builder $ runST $ do
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

-- | Prepend a statically known null-terminated ASCII string (cf. 'unpackCString#')
-- to a 'Builder' by mutating it. E. g.,
--
-- >>> :set -XOverloadedStrings -XLinearTypes -XMagicHash
-- >>> let foo# = "foo"# in runBuilder $ \b → foo# <|# (b |> "bar")
-- "foobar"
--
(<|#) ∷ Addr# → Builder ⊸ Builder
infixr 6 <|#
addr# <|# Builder (Text dst dstOff dstLen) = let srcLen = I# (cstringLength# addr#) in  Builder $ case () of
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
