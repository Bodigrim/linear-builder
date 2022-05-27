-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- 'Buffer' for strict 'Text', based on linear types.

module Data.Text.Builder.Linear.Buffer
  ( Buffer
  , runBuffer
  , dupBuffer
  , consumeBuffer
  , eraseBuffer
  , foldlIntoBuffer
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
  , (|>…)
  , (…<|)
  ) where

import qualified Data.Text.Array as A
import Data.Text.Internal (Text(..))
import GHC.Exts (cstringLength#, Addr#, Int(..), Ptr(..), setByteArray#)
import GHC.ST (ST(..))

import Data.Text.Builder.Linear.Char
import Data.Text.Builder.Linear.Core
import Data.Text.Builder.Linear.Dec
import Data.Text.Builder.Linear.Double
import Data.Text.Builder.Linear.Hex

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
  (\dst dstOff → A.copyI srcLen dst dstOff src srcOff)
  buffer

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
  (\dst dstOff → A.copyI srcLen dst dstOff src srcOff)
  buffer

-- | Append a null-terminated UTF-8 string
-- to a 'Buffer' by mutating it. E. g.,
--
-- >>> :set -XOverloadedStrings -XLinearTypes -XMagicHash
-- >>> runBuffer (\b -> b |># "foo"# |># "bar"#)
-- "foobar"
--
-- The literal string must not contain zero bytes @\\0@, this condition is not checked.
--
-- Note the inconsistency in naming: unfortunately, GHC parser does not allow for @#<|@.
--
(|>#) ∷ Buffer ⊸ Addr# → Buffer
infixl 6 |>#
buffer |># addr# = appendExact
  srcLen
  (\dst dstOff → A.copyFromPointer dst dstOff (Ptr addr#) srcLen)
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
  (\dst dstOff → A.copyFromPointer dst dstOff (Ptr addr#) srcLen)
  buffer
  where
    srcLen = I# (cstringLength# addr#)

-- | Append given number of spaces.
(|>…) ∷ Buffer ⊸ Word → Buffer
infixr 6 |>…
buf |>… 0 = buf
buffer |>… (fromIntegral -> spaces@(I# spaces#)) = appendExact
  spaces
  (\(A.MutableByteArray dst#) (I# dstOff#) -> ST (\s# ->
    (# setByteArray# dst# dstOff# spaces# 32# s#, () #)))
  buffer

-- | Prepend given number of spaces.
(…<|) ∷ Word → Buffer ⊸ Buffer
infixr 6 …<|
0 …<| buf = buf
(fromIntegral -> spaces@(I# spaces#)) …<| buffer = prependExact
  spaces
  (\(A.MutableByteArray dst#) (I# dstOff#) -> ST (\s# ->
    (# setByteArray# dst# dstOff# spaces# 32# s#, () #)))
  buffer

-- | This is just a normal 'Data.List.foldl'', but with a linear arrow
-- and potentially unlifted accumulator.
foldlIntoBuffer ∷ (Buffer ⊸ a → Buffer) → Buffer ⊸ [a] → Buffer
foldlIntoBuffer f = go
  where
    go !acc [] = acc
    go !acc (x : xs) = go (f acc x) xs
