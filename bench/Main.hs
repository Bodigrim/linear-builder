-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>

module Main where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Builder.Linear
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Test.Tasty.Bench
import Test.Tasty.QuickCheck

txt ∷ Text
txt = T.pack "Haskell + Linear Types = ♡"

benchText ∷ Int → Text
benchText = go txt
  where
    go !acc 0 = acc
    go !acc n = go (txt <> (acc <> txt)) (n - 1)

benchBuilder ∷ Int → Text
benchBuilder = TL.toStrict . TLB.toLazyText . go txtB
  where
    txtB = TLB.fromText txt
    go !acc 0 = acc
    go !acc n = go (txtB <> (acc <> txtB)) (n - 1)

benchLinearBuilder ∷ Int → Text
benchLinearBuilder n = runBuilder (\b → go (b .<> txt) n)
  where
    go ∷ Builder ⊸ Int → Builder
    go !acc 0 = acc
    go !acc n = go (txt <>. (acc .<> txt)) (n - 1)

main ∷ IO ()
main = defaultMain
  [ testProperty "sanity check" $
    \(NonNegative n) → benchText n === benchLinearBuilder n
  , bgroup "Data.Text" $
    map (\n -> bench (show n) $ nf benchText n) [1e2, 1e3, 1e4]
  , bgroup "Data.Text.Lazy.Builder" $
    map (\n -> bench (show n) $ nf benchBuilder n) [1e2, 1e3, 1e4, 1e5, 1e6]
  , bgroup "Data.Text.Builder.Linear" $
    map (\n -> bench (show n) $ nf benchLinearBuilder n) [1e2, 1e3, 1e4, 1e5, 1e6]
  ]
