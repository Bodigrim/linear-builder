-- |
-- Copyright:   (c) 2023 Pierre Le Marre
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
module Data.Text.Builder.Linear.Char.ASCII (
  -- * Multiple ASCI characters
  prependAsciiChars,
  appendAsciiChars,

  -- * Padding
  justifyLeftAscii,
  justifyRightAscii,
  centerAscii,
) where

import Data.Text.Builder.Linear.Array (unsafeReplicate)
import Data.Text.Builder.Linear.Core
import Data.Word (Word8)
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
-- Multiple chars
--------------------------------------------------------------------------------

-- | Prepend a given count of a ASCII 'Char' as 'Word8' to a 'Buffer'.
--
-- >>> :set -XLinearTypes
-- >>> runBuffer (\b -> prependAsciiChars 3 120 (b |>@ 65))
-- "xxxA"
--
-- __Warning:__ In contrast to 'Data.Text.Lazy.Builder.singleton', it is the
-- responsibility of the caller to ensure that the 'Word8' is a valid ASCII character.
prependAsciiChars ∷ Word → Word8 → Buffer ⊸ Buffer
prependAsciiChars count ch buff
  | count == 0 = buff
  | otherwise =
      prependExact
        (fromIntegral count)
        (\dst dstOff → unsafeReplicate dst dstOff (fromIntegral count) (fromIntegral ch))
        buff

-- | Append a given count of a ASCII 'Char' as 'Word8' to a 'Buffer'.
--
-- >>> :set -XLinearTypes
-- >>> runBuffer (\b -> appendAsciiChars 3 120 (b |>@ 65))
-- "Axxx"
--
-- __Warning:__ In contrast to 'Data.Text.Lazy.Builder.singleton', it is the
-- responsibility of the caller to ensure that the 'Word8' is a valid ASCII character.
appendAsciiChars ∷ Word → Word8 → Buffer ⊸ Buffer
appendAsciiChars count ch buff
  | count == 0 = buff
  | otherwise =
      appendExact
        (fromIntegral count)
        (\dst dstOff → unsafeReplicate dst dstOff (fromIntegral count) (fromIntegral ch))
        buff

--------------------------------------------------------------------------------
-- Padding
--------------------------------------------------------------------------------

-- | Pad a builder from the /left/ side to the specified length with the specified
-- ASCII character.
--
-- >>> :set -XLinearTypes
-- >>> runBuffer (\b -> justifyRightAscii 10 120 (appendAsciiChars 3 65 b))
-- "xxxxxxxAAA"
-- >>> runBuffer (\b -> justifyRightAscii 5 120 (appendAsciiChars 6 65 b))
-- "AAAAAA"
justifyRightAscii ∷ Word → Word8 → Buffer ⊸ Buffer
justifyRightAscii n ch buff = case lengthOfBuffer buff of
  (# buff', len #) →
    toLinearWord
      (\l b → if n <= l then b else prependAsciiChars (n - l) ch b)
      len
      buff'

-- | Pad a builder from the /right/ side to the specified length with the specified
-- ASCII character.
--
-- >>> :set -XLinearTypes
-- >>> runBuffer (\b -> justifyLeftAscii 10 120 (appendAsciiChars 3 65 b))
-- "AAAxxxxxxx"
-- >>> runBuffer (\b -> justifyLeftAscii 5 120 (appendAsciiChars 6 65 b))
-- "AAAAAA"
justifyLeftAscii ∷ Word → Word8 → Buffer ⊸ Buffer
justifyLeftAscii n ch buff = case lengthOfBuffer buff of
  (# buff', len #) →
    toLinearWord
      (\l b → if n <= l then b else appendAsciiChars (n - l) ch b)
      len
      buff'

-- | Center a builder to the specified length with the specified ASCII character.
--
-- >>> :set -XLinearTypes
-- >>> runBuffer (\b -> centerAscii 10 120 (appendAsciiChars 3 65 b))
-- "xxxxAAAxxx"
-- >>> runBuffer (\b -> centerAscii 5 120 (appendAsciiChars 6 65 b))
-- "AAAAAA"
centerAscii ∷ Word → Word8 → Buffer ⊸ Buffer
centerAscii n ch buff = case lengthOfBuffer buff of
  (# buff', len #) →
    toLinearWord
      ( \l b →
          if n <= l
            then b
            else case n - l of
              !d → case d `quot` 2 of
                !r → appendAsciiChars r ch (prependAsciiChars (d - r) ch b)
      )
      len
      buff'

-- Despite the use of unsafeCoerce, this is safe.
toLinearWord ∷ (Word → a) → (Word ⊸ a)
toLinearWord = unsafeCoerce
