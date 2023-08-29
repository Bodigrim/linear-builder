-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
module Data.Text.Builder.Linear.Char (
  -- * Buffer
  (|>.),
  (.<|),
) where

import Data.Text.Array qualified as A
import Data.Text.Internal.Encoding.Utf8 (ord2, ord3, ord4, utf8Length)
import Data.Text.Internal.Unsafe.Char (ord, unsafeWrite)
import GHC.ST (ST)

import Data.Text.Builder.Linear.Core

-- | Append 'Char' to a 'Buffer' by mutating it.
--
-- >>> :set -XLinearTypes
-- >>> runBuffer (\b -> b |>. 'q' |>. 'w')
-- "qw"
--
-- __Warning:__ In contrast to 'Data.Text.Lazy.Builder.singleton', it is the
-- responsibility of the caller to sanitize surrogate code points with
-- 'Data.Text.Internal.safe'.
(|>.) ∷ Buffer ⊸ Char → Buffer

infixl 6 |>.
buffer |>. ch = appendBounded 4 (\dst dstOff → unsafeWrite dst dstOff ch) buffer

-- | Prepend 'Char' to a 'Buffer' by mutating it.
--
-- >>> :set -XLinearTypes
-- >>> runBuffer (\b -> 'q' .<| 'w' .<| b)
-- "qw"
--
-- __Warning:__ In contrast to 'Data.Text.Lazy.Builder.singleton', it is the
-- responsibility of the caller to sanitize surrogate code points with
-- 'Data.Text.Internal.safe'.
(.<|) ∷ Char → Buffer ⊸ Buffer

infixr 6 .<|
ch .<| buffer =
  prependBounded
    4
    (\dst dstOff → unsafePrependCharM dst dstOff ch)
    (\dst dstOff → unsafeWrite dst dstOff ch)
    buffer

-- | Similar to 'Data.Text.Internal.Unsafe.Char.unsafeWrite',
-- but writes _before_ a given offset.
unsafePrependCharM ∷ A.MArray s → Int → Char → ST s Int
unsafePrependCharM marr off c = case utf8Length c of
  1 → do
    let n0 = fromIntegral (ord c)
    A.unsafeWrite marr (off - 1) n0
    pure 1
  2 → do
    let (n0, n1) = ord2 c
    A.unsafeWrite marr (off - 2) n0
    A.unsafeWrite marr (off - 1) n1
    pure 2
  3 → do
    let (n0, n1, n2) = ord3 c
    A.unsafeWrite marr (off - 3) n0
    A.unsafeWrite marr (off - 2) n1
    A.unsafeWrite marr (off - 1) n2
    pure 3
  _ → do
    let (n0, n1, n2, n3) = ord4 c
    A.unsafeWrite marr (off - 4) n0
    A.unsafeWrite marr (off - 3) n1
    A.unsafeWrite marr (off - 2) n2
    A.unsafeWrite marr (off - 1) n3
    pure 4
