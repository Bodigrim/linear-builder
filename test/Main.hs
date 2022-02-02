-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>

{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Data.Text.Builder.Linear
import Data.Foldable
import qualified Data.Text as T
import Data.Text.Internal (Text(..))
import Test.Tasty
import Test.Tasty.QuickCheck

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

evalProgramOnText ∷ [Either Text Text] → Text
evalProgramOnText = foldl' (\acc x → either (<> acc) (acc <>) x) mempty

evalProgramOnBuilder ∷ [Either Text Text] → Text
evalProgramOnBuilder xs = runBuilder $ \b → linearFoldl' go b xs
  where
    go ∷ Builder ⊸ Either Text Text → Builder
    go b (Left x) = x <>. b
    go b (Right x) = b .<> x

linearFoldl' ∷ forall a b. (b ⊸ a → b) → b ⊸ [a] → b
linearFoldl' f = go
  where
    go ∷ b ⊸ [a] → b
    go !acc [] = acc
    go !acc (x : xs) = go (f acc x) xs

main ∷ IO ()
main = defaultMain $
  testProperty "evaluate" $
    \prog → evalProgramOnText prog === evalProgramOnBuilder prog
