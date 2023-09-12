-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
module Data.Text.Builder.Linear.Char (
  -- * Single character
  (|>.),
  (.<|),

  -- * Multiple characters
  prependChars,
  appendChars,

  -- * Padding
  justifyLeft,
  justifyRight,
  center,
) where

import Data.Char (isAscii)
import Data.Text.Array qualified as A
import Data.Text.Internal.Encoding.Utf8 (ord2, ord3, ord4, utf8Length)
import Data.Text.Internal.Unsafe.Char (ord, unsafeWrite)
import GHC.ST (ST)
import Unsafe.Coerce (unsafeCoerce)

import Data.Text.Builder.Linear.Array (unsafeReplicate, unsafeTile)
import Data.Text.Builder.Linear.Core

--------------------------------------------------------------------------------
-- Single char
--------------------------------------------------------------------------------

-- | Append 'Char' to a 'Buffer' by mutating it.
--
-- >>> :set -XLinearTypes
-- >>> runBuffer (\b -> b |>. 'q' |>. 'w')
-- "qw"
--
-- Prefer '(|>@)' if the character is ASCII.
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
-- Prefer '(@<|)' if the character is ASCII.
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

--------------------------------------------------------------------------------
-- Multiple chars
--------------------------------------------------------------------------------

-- | Prepend a given count of a 'Char' to a 'Buffer'.
--
-- >>> :set -XLinearTypes
-- >>> runBuffer (\b -> prependChars 3 'x' (b |>. 'A'))
-- "xxxA"
prependChars ∷ Word → Char → Buffer ⊸ Buffer
prependChars count ch buff
  | count == 0 = buff
  | otherwise =
      case utf8Length ch of
        cLen → case cLen * fromIntegral count of
          totalLen →
            prependExact
              totalLen
              ( if isAscii ch
                  then \dst dstOff → unsafeReplicate dst dstOff (fromIntegral count) (ord ch)
                  else \dst dstOff → unsafeWrite dst dstOff ch *> unsafeTile dst dstOff totalLen cLen
              )
              buff

-- | Apppend a given count of a 'Char' to a 'Buffer'.
--
-- >>> :set -XLinearTypes
-- >>> runBuffer (\b -> appendChars 3 'x' (b |>. 'A'))
-- "Axxx"
appendChars ∷ Word → Char → Buffer ⊸ Buffer
appendChars count ch buff
  | count == 0 = buff
  | otherwise =
      case utf8Length ch of
        cLen → case cLen * fromIntegral count of
          totalLen →
            appendExact
              totalLen
              ( if isAscii ch
                  then \dst dstOff → unsafeReplicate dst dstOff (fromIntegral count) (ord ch)
                  else \dst dstOff → unsafeWrite dst dstOff ch *> unsafeTile dst dstOff totalLen cLen
              )
              buff

--------------------------------------------------------------------------------
-- Padding
--------------------------------------------------------------------------------

-- | Pad a builder from the /left/ side to the specified length with the specified
-- character.
--
-- >>> :set -XLinearTypes
-- >>> runBuffer (\b -> justifyRight 10 'x' (appendChars 3 'A' b))
-- "xxxxxxxAAA"
-- >>> runBuffer (\b -> justifyRight 5 'x' (appendChars 6 'A' b))
-- "AAAAAA"
--
-- Note that 'newEmptyBuffer' is needed in some situations. The following example creates
-- a utility function that justify a text and then append it to a buffer.
--
-- >>> :set -XOverloadedStrings -XLinearTypes -XUnboxedTuples
-- >>> import Data.Text.Builder.Linear.Buffer
-- >>> import Data.Text (Text)
-- >>> :{
-- appendJustified :: Buffer %1 -> Text -> Buffer
-- appendJustified b t = case newEmptyBuffer b of
--   -- Note that we need to create a new buffer from the text, in order
--   -- to justify only the text and not the input buffer.
--   (# b', empty #) -> b' >< justifyRight 12 ' ' (empty |> t)
-- :}
--
-- >>> runBuffer (\b -> (b |> "Test:") `appendJustified` "foo" `appendJustified` "bar")
-- "Test:         foo         bar"
justifyRight ∷ Word → Char → Buffer ⊸ Buffer
justifyRight n ch buff = case lengthOfBuffer buff of
  (# buff', len #) →
    toLinearWord
      (\l b → if n <= l then b else prependChars (n - l) ch b)
      len
      buff'

-- | Pad a builder from the /right/ side to the specified length with the specified
-- character.
--
-- >>> :set -XLinearTypes
-- >>> runBuffer (\b -> justifyLeft 10 'x' (appendChars 3 'A' b))
-- "AAAxxxxxxx"
-- >>> runBuffer (\b -> justifyLeft 5 'x' (appendChars 6 'A' b))
-- "AAAAAA"
--
-- Note that 'newEmptyBuffer' is needed in some situations. See 'justifyRight'
-- for an example.
justifyLeft ∷ Word → Char → Buffer ⊸ Buffer
justifyLeft n ch buff = case lengthOfBuffer buff of
  (# buff', len #) →
    toLinearWord
      (\l b → if n <= l then b else appendChars (n - l) ch b)
      len
      buff'

-- | Center a builder to the specified length with the specified character.
--
-- >>> :set -XLinearTypes
-- >>> runBuffer (\b -> center 10 'x' (appendChars 3 'A' b))
-- "xxxxAAAxxx"
-- >>> runBuffer (\b -> center 5 'x' (appendChars 6 'A' b))
-- "AAAAAA"
--
-- Note that 'newEmptyBuffer' is needed in some situations. See 'justifyRight'
-- for an example.
center ∷ Word → Char → Buffer ⊸ Buffer
center n ch buff = case lengthOfBuffer buff of
  (# buff', len #) →
    toLinearWord
      ( \l b →
          if n <= l
            then b
            else case n - l of
              !d → case d `quot` 2 of
                !r → appendChars r ch (prependChars (d - r) ch b)
      )
      len
      buff'

-- Despite the use of unsafeCoerce, this is safe.
toLinearWord ∷ (Word → a) → (Word ⊸ a)
toLinearWord = unsafeCoerce
