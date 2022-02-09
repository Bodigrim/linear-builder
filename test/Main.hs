-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>

module Main where

import Data.Text.Builder.Linear
import Data.Foldable
import qualified Data.Text as T
import Data.Text.Internal (Text(..))
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
  deriving (Eq, Ord, Show)

instance Arbitrary Action where
  arbitrary = oneof
    [ AppendText  <$> arbitrary
    , PrependText <$> arbitrary
    , AppendChar  <$> arbitraryUnicodeChar
    , PrependChar <$> arbitraryUnicodeChar
    ]

interpretOnText ∷ [Action] → Text
interpretOnText = foldl' go mempty
  where
    go ∷ Text → Action → Text
    go b (AppendText  x) = b <> x
    go b (PrependText x) = x <> b
    go b (AppendChar  x) = T.snoc b x
    go b (PrependChar x) = T.cons x b

interpretOnBuffer ∷ [Action] → Buffer ⊸ Buffer
interpretOnBuffer xs z = linearFoldl' go z xs
  where
    go ∷ Buffer ⊸ Action → Buffer
    go b (AppendText  x) = b |> x
    go b (PrependText x) = x <| b
    go b (AppendChar  x) = b |>. x
    go b (PrependChar x) = x .<| b

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
prop1 acts = interpretOnText acts ===
  runBuffer (\b → interpretOnBuffer acts b)

prop2 ∷ [Action] → [Action] → Property
prop2 acts1 acts2 = interpretOnText acts1 <> interpretOnText acts2 ===
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
