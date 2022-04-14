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
  ) where

import qualified Data.Text.Array as A
import Data.Text.Internal (Text(..))
import GHC.Exts

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
