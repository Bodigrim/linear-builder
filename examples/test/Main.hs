{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
module Main where

import Data.Char (ord)
import Data.Foldable
import Data.Text qualified as T
import Data.Text.Builder.Linear.Buffer
import Data.Text.Internal (Text (..))
import Data.Text.Lazy (toStrict)

-- import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int qualified as T
import Examples.Unicode ((|>&&))
import GHC.Generics
import Test.Tasty
import Test.Tasty.QuickCheck hiding ((.&.), (><))

newtype Action
  = AppendUnicode Char
  deriving (Eq, Ord, Show, Generic)

instance Arbitrary Action where
  arbitrary =
    oneof
      [ AppendUnicode <$> arbitraryUnicodeChar
      ]

  shrink = genericShrink

interpretOnText ∷ [Action] → Text → Text
interpretOnText xs z = foldl' go z xs
  where
    go ∷ Text → Action → Text
    go b (AppendUnicode ch) = b <> T.pack "U+" <> T.justifyRight 4 '0' (T.toUpper (toStrict (toLazyText (T.hexadecimal (ord ch)))))

interpretOnBuffer ∷ [Action] → Buffer ⊸ Buffer
interpretOnBuffer xs z = foldlIntoBuffer go z xs
  where
    go ∷ Buffer ⊸ Action → Buffer
    go b (AppendUnicode ch) = b |>&& ch

main ∷ IO ()
main =
  defaultMain $
    testGroup
      "All"
      [ testProperty "sequence of actions" prop1
      ]

prop1 ∷ [Action] → Property
prop1 acts =
  interpretOnText acts mempty
    === runBuffer (\b → interpretOnBuffer acts b)
