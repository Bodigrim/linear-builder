-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>

module Main where

import Data.Bits (Bits(..), FiniteBits(..))
import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text.Builder.Linear.Buffer
import Data.Text.Internal (Text(..))
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal, hexadecimal)
import Data.Text.Lazy.Builder.RealFloat (realFloat)
import GHC.Generics
import Test.Tasty
import Test.Tasty.QuickCheck hiding ((><), (.&.))

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
  | AppendDec30 Int30
  | PrependDec30 Int30
  | AppendDouble Double
  | PrependDouble Double
  | AppendSpaces Word
  | PrependSpaces Word
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
    , AppendDec30   <$> arbitraryBoundedIntegral
    , PrependDec30  <$> arbitraryBoundedIntegral
    , pure $ AppendHex minBound
    , pure $ AppendHex maxBound
    , pure $ AppendDec minBound
    , pure $ AppendDec maxBound
    , pure $ AppendDec 0
    , AppendDouble  <$> arbitrary
    , PrependDouble <$> arbitrary
    , AppendSpaces . getNonNegative <$> arbitrary
    , PrependSpaces . getNonNegative <$> arbitrary
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
    go b (AppendDec30   x) = b <> toStrict (toLazyText (decimal x))
    go b (PrependDec30  x) = toStrict (toLazyText (decimal x)) <> b
    go b (AppendDouble  x) = b <> toStrict (toLazyText (realFloat x))
    go b (PrependDouble x) = toStrict (toLazyText (realFloat x)) <> b
    go b (AppendSpaces  n) = b <> T.replicate (fromIntegral n) (T.singleton ' ')
    go b (PrependSpaces n) = T.replicate (fromIntegral n) (T.singleton ' ') <> b

interpretOnBuffer ∷ [Action] → Buffer ⊸ Buffer
interpretOnBuffer xs z = foldlIntoBuffer go z xs
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
    go b (AppendDec30   x) = b |>$ x
    go b (PrependDec30  x) = x $<| b
    go b (AppendDouble  x) = b |>% x
    go b (PrependDouble x) = x %<| b
    go b (AppendSpaces  n) = b |>… n
    go b (PrependSpaces n) = n …<| b

main ∷ IO ()
main = defaultMain $ testGroup "All"
  [ testProperty "sequence of actions" prop1
  , testProperty "two sequences of actions" prop2
  , testProperty "append addr#" prop3
  , testProperty "prepend addr#" prop4
  , testProperty "bytestring builder" prop5
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

prop3 :: [Action] → Property
prop3 acts = runBuffer f1 === runBuffer f2
  where
    addr# = "foo"#
    f1, f2 :: Buffer ⊸ Buffer
    f1 = \b → interpretOnBuffer acts b |># addr#
    f2 = \b → interpretOnBuffer acts b |> T.pack "foo"

prop4 :: [Action] → Property
prop4 acts = runBuffer f1 === runBuffer f2
  where
    addr# = "foo"#
    f1, f2 :: Buffer ⊸ Buffer
    f1 = \b → addr# #<| interpretOnBuffer acts b
    f2 = \b → T.pack "foo" <| interpretOnBuffer acts b

prop5 ∷ [Action] → Property
prop5 acts = T.encodeUtf8 (interpretOnText acts mempty) ===
  runBufferBS (\b → interpretOnBuffer acts b)

-------------------------------------------------------------------------------

newtype Int30 = Int30' Int
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum, Real, Integral)

pattern Int30 :: Int -> Int30
pattern Int30 x <- Int30' x where
  Int30 x = Int30' (x .&. ((1 `shiftL` 30) - 1))
{-# COMPLETE Int30 #-}

instance Arbitrary Int30 where
  arbitrary = Int30 <$> arbitrary
  shrink (Int30 x) = Int30 <$> shrink x

instance Bounded Int30 where
  minBound = negate (1 `shiftL` 30)
  maxBound = (1 `shiftL` 30) - 1

instance Num Int30 where
  Int30 x + Int30 y = Int30 (x + y)
  Int30 x * Int30 y = Int30 (x * y)
  abs (Int30 x) = Int30 (abs x)
  signum = undefined
  negate  (Int30 x) = Int30 (negate x)
  fromInteger x = Int30 (fromInteger x)

instance Bits Int30 where
  (.&.) = undefined
  (.|.) = undefined
  xor = undefined
  complement = undefined
  shift (Int30 x) i = Int30 (shift x i)
  rotate = undefined
  bitSize = const 30
  bitSizeMaybe = const (Just 30)
  isSigned = const True
  testBit = undefined
  bit = undefined
  popCount = undefined

instance FiniteBits Int30 where
  finiteBitSize = const 30
