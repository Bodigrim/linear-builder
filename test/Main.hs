{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>

module Main where

import Data.Bits (Bits(..), FiniteBits(..), bitDefault)
import Data.Foldable
import Data.Int
import Data.List (intersperse)
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text.Builder.Linear.Buffer
import Data.Text.Internal (Text(..))
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal, hexadecimal)
import Data.Text.Lazy.Builder.RealFloat (realFloat)
import Data.Word
import GHC.Generics
import GHC.TypeLits (KnownNat, OrderingI (..), SomeNat (..), cmpNat, natVal, sameNat, someNatVal)
import Numeric.Natural (Natural)
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
  | AppendChars Word Char
  | PrependChars Word Char
  | JustifyLeft Word Char
  | JustifyRight Word Char
  | Center Word Char
  | HexInt Int8 Int16 (IntN 30) (IntN 31) Int32 (IntN 33) Int64
  | HexWord Word8 Word16 Word32 Word64
  | AppendHexI SomeIntN
  | PrependHexI SomeIntN
  | AppendHexW SomeWordN
  | PrependHexW SomeWordN
  | DecInt Int8 Int16 (IntN 30) (IntN 31) Int32 (IntN 33) Int64
  | DecWord Word8 Word16 (WordN 30) (WordN 31) Word32 (WordN 33) Word64
  | AppendDecW Word
  | PrependDecW Word
  | AppendDecI Int
  | PrependDecI Int
  | AppendDecI30 (IntN 30)
  | PrependDecI30 (IntN 30)
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
    , AppendChars   <$> arbitraryCharCount <*> arbitraryUnicodeChar
    , PrependChars  <$> arbitraryCharCount <*> arbitraryUnicodeChar
    , JustifyLeft   <$> arbitraryTotalLength <*> arbitraryUnicodeChar
    , JustifyRight  <$> arbitraryTotalLength <*> arbitraryUnicodeChar
    , Center        <$> arbitraryTotalLength <*> arbitraryUnicodeChar
    , AppendHexI    <$> arbitrary
    , PrependHexI   <$> arbitrary
    , AppendHexW    <$> arbitrary
    , PrependHexW   <$> arbitrary
    , AppendDecW    <$> arbitraryBoundedIntegral
    , PrependDecW   <$> arbitraryBoundedIntegral
    , AppendDecI    <$> arbitraryBoundedIntegral
    , PrependDecI   <$> arbitraryBoundedIntegral
    , AppendDecI30  <$> arbitraryBoundedIntegral
    , PrependDecI30 <$> arbitraryBoundedIntegral
    , pure $ HexWord minBound minBound minBound minBound
    , pure $ HexWord maxBound maxBound maxBound maxBound
    , pure $ HexInt minBound minBound minBound minBound minBound minBound minBound
    , pure $ HexInt maxBound maxBound maxBound maxBound maxBound maxBound maxBound
    , pure $ HexInt 0 0 0 0 0 0 0
    , pure $ DecInt minBound minBound minBound minBound minBound minBound minBound
    , pure $ DecInt maxBound maxBound maxBound maxBound maxBound maxBound maxBound
    , pure $ DecInt 0 0 0 0 0 0 0
    , pure $ DecWord minBound minBound minBound minBound minBound minBound minBound
    , pure $ DecWord maxBound maxBound maxBound maxBound maxBound maxBound maxBound
    , AppendDouble  <$> arbitrary
    , PrependDouble <$> arbitrary
    , AppendSpaces . getNonNegative <$> arbitrary
    , PrependSpaces . getNonNegative <$> arbitrary
    ]
    where
      arbitraryCharCount = chooseBoundedIntegral (0, 6)
      arbitraryTotalLength = chooseBoundedIntegral (3, 20)

  shrink = genericShrink

interpretOnText ∷ [Action] → Text → Text
interpretOnText xs z = foldl' go z xs
  where
    go ∷ Text → Action → Text
    go b (AppendText     x) = b <> x
    go b (PrependText    x) = x <> b
    go b (AppendChar     x) = T.snoc b x
    go b (PrependChar    x) = T.cons x b
    go b (AppendChars  n x) = b <> T.replicate (fromIntegral n) (T.singleton x)
    go b (PrependChars n x) = T.replicate (fromIntegral n) (T.singleton x) <> b
    go b (JustifyLeft  n x) = T.justifyLeft  (fromIntegral n) x b
    go b (JustifyRight n x) = T.justifyRight (fromIntegral n) x b
    go b (Center       n x) = T.center (fromIntegral n) x b
    go b (HexInt r s t u v w x)
      = intersperseText
          [ hexadecimal (fromIntegral @Int16 @Word16 s)
          , hexadecimalI t
          , hexadecimal (fromIntegral @Int64 @Word64 x) ]
      <> b
      <> intersperseText
          [ hexadecimal (fromIntegral @Int8 @Word8 r)
          , hexadecimalI u
          , hexadecimal (fromIntegral @Int32 @Word32 v)
          , hexadecimalI w ]
    go b (HexWord u v w x)
      = intersperseText [hexadecimal u, hexadecimal x]
      <> b
      <> intersperseText [hexadecimal v, hexadecimal w ]
    go b (AppendHexI    x) = b <> toStrict (toLazyText (hexadecimalSI x))
    go b (PrependHexI   x) = toStrict (toLazyText (hexadecimalSI x)) <> b
    go b (AppendHexW    x) = b <> toStrict (toLazyText (hexadecimalSW x))
    go b (PrependHexW   x) = toStrict (toLazyText (hexadecimalSW x)) <> b
    go b (DecInt r s t u v w x)
                           = intersperseText [decimal s, decimal t, decimal x]
                           <> b
                           <> intersperseText [decimal r, decimal u, decimal v, decimal w]
    go b (DecWord r s t u v w x)
                           = intersperseText [decimal s, decimal t, decimal x]
                           <> b
                           <> intersperseText [decimal r, decimal u, decimal v, decimal w]
    go b (AppendDecW    x) = b <> toStrict (toLazyText (decimal x))
    go b (PrependDecW   x) = toStrict (toLazyText (decimal x)) <> b
    go b (AppendDecI    x) = b <> toStrict (toLazyText (decimal x))
    go b (PrependDecI   x) = toStrict (toLazyText (decimal x)) <> b
    go b (AppendDecI30  x) = b <> toStrict (toLazyText (decimal x))
    go b (PrependDecI30 x) = toStrict (toLazyText (decimal x)) <> b
    go b (AppendDouble  x) = b <> toStrict (toLazyText (realFloat x))
    go b (PrependDouble x) = toStrict (toLazyText (realFloat x)) <> b
    go b (AppendSpaces  n) = b <> T.replicate (fromIntegral n) (T.singleton ' ')
    go b (PrependSpaces n) = T.replicate (fromIntegral n) (T.singleton ' ') <> b

    hexadecimalSI (SomeIntN x) = hexadecimalI x

    hexadecimalI ∷ (KnownNat n) ⇒ IntN n → TB.Builder
    hexadecimalI x = if x >= 0
      then hexadecimal x
      else hexadecimal (fromIntegral @_ @Word64 x .&. (shiftL 1 (intSize x) - 1))

    hexadecimalSW (SomeWordN x) = hexadecimalW x

    hexadecimalW ∷ (KnownNat n) ⇒ WordN n → TB.Builder
    hexadecimalW x = if x >= 0
      then hexadecimal x
      else hexadecimal (fromIntegral @_ @Word64 x .&. (shiftL 1 (intSize x) - 1))

    intersperseText ∷ [TB.Builder] → Text
    intersperseText bs =
      toStrict (toLazyText (mconcat (intersperse (TB.singleton ';') bs)))

interpretOnBuffer ∷ [Action] → Buffer ⊸ Buffer
interpretOnBuffer xs z = foldlIntoBuffer go z xs
  where
    go ∷ Buffer ⊸ Action → Buffer
    go b (AppendText     x) = b |> x
    go b (PrependText    x) = x <| b
    go b (AppendChar     x) = b |>. x
    go b (PrependChar    x) = x .<| b
    go b (AppendChars   n x) = appendChars n x b
    go b (PrependChars  n x) = prependChars n x b
    go b (JustifyLeft   n x) = justifyLeft n x b
    go b (JustifyRight  n x) = justifyRight n x b
    go b (Center        n x) = center n x b
    go b (HexInt r s t u v w x) = s &<| ";"# #<| t &<| ";"# #<| x &<|
                                  (b |>& r |># ";"# |>& u |># ";"# |>& v |># ";"# |>& w)
    go b (HexWord u v w x) = u &<| ";"# #<| x &<| (b |>& v |># ";"# |>& w)
    go b (AppendHexI     x) = case x of {SomeIntN i → b |>& i}
    go b (PrependHexI    x) = case x of {SomeIntN i → i &<| b}
    go b (AppendHexW     x) = case x of {SomeWordN i → b |>& i}
    go b (PrependHexW    x) = case x of {SomeWordN i → i &<| b}
    go b (DecInt  r s t u v w x) = s $<| ";"# #<| t $<| ";"# #<| x $<| (b |>$ r |># ";"# |>$ u |># ";"# |>$ v |># ";"# |>$ w)
    go b (DecWord r s t u v w x) = s $<| ";"# #<| t $<| ";"# #<| x $<| (b |>$ r |># ";"# |>$ u |># ";"# |>$ v |># ";"# |>$ w)
    go b (AppendDecW     x) = b |>$ x
    go b (PrependDecW    x) = x $<| b
    go b (AppendDecI     x) = b |>$ x
    go b (PrependDecI    x) = x $<| b
    go b (AppendDecI30   x) = b |>$ x
    go b (PrependDecI30  x) = x $<| b
    go b (AppendDouble   x) = b |>% x
    go b (PrependDouble  x) = x %<| b
    go b (AppendSpaces   n) = b |>… n
    go b (PrependSpaces  n) = n …<| b

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

--------------------------------------------------------------------------------
-- IntN
--------------------------------------------------------------------------------

newtype IntN (n ∷ Natural) = IntN' {unIntN ∷ Int64}
  deriving stock (Eq, Ord)
  deriving newtype (Enum, Real, Integral)

instance (KnownNat n) ⇒ Show (IntN n) where
  showsPrec p (IntN x) = showParen (p > 10)
    (\s → mconcat ["IntN @", show (natVal (Proxy @n)), " ", show x, s])

pattern IntN ∷ forall n. (KnownNat n) => Int64 → IntN n
pattern IntN x ← IntN' x where
  IntN x = IntN' x'
    where
    -- If the nth bit is 1, then interpret the value as negative and fill the
    -- bits from nth position with 1s. Otherwise clear them to 0s.
    size = intSize (Proxy @n)
    x' = if testBit x (size - 1)
      then x .|. m1
      else x .&. m2
    m1 = complement ((1 `shiftL` (size - 1)) - 1)
    m2 = (1 `shiftL` size) - 1

{-# COMPLETE IntN #-}

intSize ∷ forall p n. (KnownNat n) => p n → Int
intSize _ = fromInteger (natVal (Proxy @n))

instance (KnownNat n) => Arbitrary (IntN n) where
  arbitrary =
    IntN <$> chooseBoundedIntegral (unIntN @n minBound, unIntN @n maxBound)
  shrink = shrinkIntegral

instance (KnownNat n) => Bounded (IntN n) where
  minBound = IntN (negate (1 `shiftL` (intSize (Proxy @n) - 1)))
  maxBound = IntN ((1 `shiftL` (intSize (Proxy @n) - 1)) - 1)

instance (KnownNat n) => Num (IntN n) where
  IntN x + IntN y = IntN (x + y)
  IntN x * IntN y = IntN (x * y)
  abs (IntN x) = IntN (abs x)
  signum = undefined
  negate (IntN x) = IntN (negate x)
  fromInteger x = IntN (fromInteger x)

instance (KnownNat n) => Bits (IntN n) where
  IntN a .&. IntN b = IntN (a .&. b)
  IntN a .|. IntN b = IntN (a .|. b)
  xor = undefined
  complement (IntN x) = IntN (complement x)
  shift (IntN x) i = IntN (shift x i)
  rotate = undefined
  bitSize = const (intSize (Proxy @n))
  bitSizeMaybe = const (Just (intSize (Proxy @n)))
  isSigned = const True
  testBit (IntN x) = testBit x
  bit = bitDefault
  popCount = undefined

instance (KnownNat n) => FiniteBits (IntN n) where
  finiteBitSize = const (intSize (Proxy @n))

data SomeIntN = forall n. (KnownNat n) ⇒ SomeIntN (IntN n)

instance Eq SomeIntN where
  SomeIntN (IntN @n1 i1) == SomeIntN (IntN @n2 i2) =
    case sameNat (Proxy @n1) (Proxy @n2) of
      Just _  → i1 == i2
      Nothing → False

instance Ord SomeIntN where
  SomeIntN (IntN @n1 i1) `compare` SomeIntN (IntN @n2 i2) =
    case cmpNat (Proxy @n1) (Proxy @n2) of
      LTI → LT
      EQI → compare i1 i2
      GTI → GT

instance Show SomeIntN where
  show (SomeIntN i) = show i

instance Arbitrary SomeIntN where
  arbitrary = do
    s <- chooseInt (8, 64)
    case someNatVal (toInteger s) of
      Just (SomeNat (Proxy ∷ Proxy n)) →
        SomeIntN <$> arbitraryBoundedIntegral @(IntN n)
      Nothing → error "impossible"
  shrink (SomeIntN i) = SomeIntN <$> shrinkIntegral i

--------------------------------------------------------------------------------
-- WordN
--------------------------------------------------------------------------------

newtype WordN (n ∷ Natural) = WordN' { unWordN :: Word64 }
  deriving stock (Eq, Ord)
  deriving newtype (Enum, Real, Integral)

instance (KnownNat n) ⇒ Show (WordN n) where
  showsPrec p (WordN x) = showParen (p > 10)
    (\s → mconcat ["WordN @", show (natVal (Proxy @n)), " ", show x, s])

pattern WordN ∷ forall n. (KnownNat n) => Word64 → WordN n
pattern WordN x ← WordN' x where
  WordN x = WordN' (x .&. ((1 `shiftL` intSize (Proxy @n)) - 1))

{-# COMPLETE WordN #-}

instance (KnownNat n) => Arbitrary (WordN n) where
  arbitrary =
    WordN <$> chooseBoundedIntegral (unWordN @n minBound, unWordN @n maxBound)
  shrink = shrinkIntegral

instance (KnownNat n) => Bounded (WordN n) where
  minBound = WordN' 0
  maxBound = WordN ((1 `shiftL` intSize (Proxy @n)) - 1)

instance (KnownNat n) => Num (WordN n) where
  WordN x + WordN y = WordN (x + y)
  WordN x * WordN y = WordN (x * y)
  abs = id
  signum = undefined
  negate (WordN x) = WordN (negate x)
  fromInteger x = WordN (fromInteger x)

instance (KnownNat n) => Bits (WordN n) where
  WordN a .&. WordN b = WordN (a .&. b)
  WordN a .|. WordN b = WordN (a .|. b)
  xor = undefined
  complement (WordN x) = WordN (complement x)
  shift (WordN x) i = WordN (shift x i)
  rotate = undefined
  bitSize = const (intSize (Proxy @n))
  bitSizeMaybe = const (Just (intSize (Proxy @n)))
  isSigned = const False
  testBit (WordN x) = testBit x
  bit = bitDefault
  popCount = undefined

instance (KnownNat n) => FiniteBits (WordN n) where
  finiteBitSize = const (intSize (Proxy @n))

data SomeWordN = forall n. (KnownNat n) ⇒ SomeWordN (WordN n)

instance Eq SomeWordN where
  SomeWordN (WordN @n1 i1) == SomeWordN (WordN @n2 i2) =
    case sameNat (Proxy @n1) (Proxy @n2) of
      Just _  → i1 == i2
      Nothing → False

instance Ord SomeWordN where
  SomeWordN (WordN @n1 i1) `compare` SomeWordN (WordN @n2 i2) =
    case cmpNat (Proxy @n1) (Proxy @n2) of
      LTI → LT
      EQI → compare i1 i2
      GTI → GT

instance Show SomeWordN where
  show (SomeWordN i) = show i

instance Arbitrary SomeWordN where
  arbitrary = do
    s <- chooseInt (8, 64)
    case someNatVal (toInteger s) of
      Just (SomeNat (Proxy ∷ Proxy n)) →
        SomeWordN <$> arbitraryBoundedIntegral @(WordN n)
      Nothing → error "impossible"
  shrink (SomeWordN i) = SomeWordN <$> shrinkIntegral i
