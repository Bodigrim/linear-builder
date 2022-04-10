-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>

module Main where

import Data.Foldable
import qualified Data.Text as T
import Data.Text.Builder.Linear.Buffer
import Data.Text.Internal (Text(..))
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal, hexadecimal)
import Data.Text.Lazy.Builder.RealFloat (realFloat)
import GHC.Generics
import Test.Tasty
import Test.Tasty.QuickCheck hiding ((><))

instance Arbitrary Text where
  arbitrary = do
    xs ← T.pack <$> arbitrary
    d ← (`mod` (T.length xs + 1)) <$> arbitrary
    pure $ T.drop d xs
  shrink t@(Text arr off len)
    =  map (T.drop d . T.pack) (shrink ys)
    ++ map (\d' → T.drop d' $ T.pack $ drop (d - d') ys) (shrink d)
    where
      xs = T.unpack t
      ys = T.unpack (Text arr 0 (off + len))
      d  = length ys - length xs

data Action
  = AppendText Text
  | PrependText Text
  | AppendChar Char
  | PrependChar Char
  | AppendHex Word
  | PrependHex Word
  | AppendDec Int
  | PrependDec Int
  | AppendDouble Double
  | PrependDouble Double
  deriving (Eq, Ord, Show, Generic)

instance Arbitrary Action where
  arbitrary = oneof
    [ AppendText    <$> arbitrary
    , PrependText   <$> arbitrary
    , AppendChar    <$> arbitraryUnicodeChar
    , PrependChar   <$> arbitraryUnicodeChar
    , AppendHex     <$> arbitraryBoundedIntegral
    , PrependHex    <$> arbitraryBoundedIntegral
    , AppendDec     <$> arbitraryBoundedIntegral
    , PrependDec    <$> arbitraryBoundedIntegral
    , pure $ AppendHex minBound
    , pure $ AppendHex maxBound
    -- TODO , pure $ AppendDec minBound
    , pure $ AppendDec maxBound
    , AppendDouble  <$> arbitrary
    , PrependDouble <$> arbitrary
    ]
  shrink = genericShrink

interpretOnText ∷ [Action] → Text → Text
interpretOnText xs z = foldl' go z xs
  where
    go ∷ Text → Action → Text
    go b (AppendText    x) = b <> x
    go b (PrependText   x) = x <> b
    go b (AppendChar    x) = T.snoc b x
    go b (PrependChar   x) = T.cons x b
    go b (AppendHex     x) = b <> toStrict (toLazyText (hexadecimal x))
    go b (PrependHex    x) = toStrict (toLazyText (hexadecimal x)) <> b
    go b (AppendDec     x) = b <> toStrict (toLazyText (decimal x))
    go b (PrependDec    x) = toStrict (toLazyText (decimal x)) <> b
    go b (AppendDouble  x) = b <> toStrict (toLazyText (realFloat x))
    go b (PrependDouble x) = toStrict (toLazyText (realFloat x)) <> b

interpretOnBuffer ∷ [Action] → Buffer ⊸ Buffer
interpretOnBuffer xs z = linearFoldl' go z xs
  where
    go ∷ Buffer ⊸ Action → Buffer
    go b (AppendText    x) = b |> x
    go b (PrependText   x) = x <| b
    go b (AppendChar    x) = b |>. x
    go b (PrependChar   x) = x .<| b
    go b (AppendHex     x) = b |>& x
    go b (PrependHex    x) = x &<| b
    go b (AppendDec     x) = b |>$ x
    go b (PrependDec    x) = x $<| b
    go b (AppendDouble  x) = b |>% x
    go b (PrependDouble x) = x %<| b

linearFoldl' ∷ (Buffer ⊸ a → Buffer) → Buffer ⊸ [a] → Buffer
linearFoldl' f = go
  where
    go !acc [] = acc
    go !acc (x : xs) = go (f acc x) xs

main ∷ IO ()
main = defaultMain $ testGroup "All"
  [ testProperty "sequence of actions" prop1
  , testProperty "two sequences of actions" prop2
  , testProperty "append addr#" prop3
  , testProperty "prepend addr#" prop4
  ]

prop1 ∷ [Action] → Property
prop1 acts = interpretOnText acts mempty ===
  runBuffer (\b → interpretOnBuffer acts b)

prop2 ∷ [Action] → [Action] → Property
prop2 acts1 acts2 = interpretOnText acts1 mempty <> interpretOnText acts2 mempty ===
  runBuffer (\b → go (dupBuffer b))
  where
    go ∷ (# Buffer, Buffer #) ⊸ Buffer
    go (# b1, b2 #) = interpretOnBuffer acts1 b1 >< interpretOnBuffer acts2 b2

prop3 :: [Action] -> Property
prop3 acts = runBuffer f1 === runBuffer f2
  where
    addr# = "foo"#
    f1, f2 :: Buffer ⊸ Buffer
    f1 = \b -> interpretOnBuffer acts b |># addr#
    f2 = \b -> interpretOnBuffer acts b |> T.pack "foo"

prop4 :: [Action] -> Property
prop4 acts = runBuffer f1 === runBuffer f2
  where
    addr# = "foo"#
    f1, f2 :: Buffer ⊸ Buffer
    f1 = \b -> addr# <|# interpretOnBuffer acts b
    f2 = \b -> T.pack "foo" <| interpretOnBuffer acts b
