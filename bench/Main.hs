-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>

module Main where

import qualified Data.Text as T
import Data.Text.Builder.Linear.Buffer
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText, fromText)
import Data.Text.Lazy.Builder.Int (decimal, hexadecimal)
import Data.Text.Lazy.Builder.RealFloat (realFloat)
import Test.Tasty.Bench

txt ∷ T.Text
txt = T.pack "Haskell + Linear Types = ♡"

chr :: Char
chr = '♡'

int :: Int
int = 123456789123456789

dbl :: Double
dbl = - pi * 1e300

benchBuilder ∷ Int → T.Text
benchBuilder = toStrict . toLazyText . go txtB
  where
    txtB = fromText txt
    go !acc 0 = acc
    go !acc n = go (txtB <> (acc <> txtB)) (n - 1)

benchLinearBuilder ∷ Int → T.Text
benchLinearBuilder m = runBuffer (\b → go (b |> txt) m)
  where
    go ∷ Buffer ⊸ Int → Buffer
    go !acc 0 = acc
    go !acc n = go (txt <| (acc |> txt)) (n - 1)

main ∷ IO ()
main = defaultMain $ map mkGroup [1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6]

mkGroup :: Int -> Benchmark
mkGroup n = bgroup (show n)
  [ bench "Data.Text.Lazy.Builder" $ nf benchBuilder n
  , bcompare ("$NF == \"Data.Text.Lazy.Builder\" && $(NF-1) == \"" ++ show n ++ "\"")
  $ bench "Data.Text.Builder.Linear" $ nf benchLinearBuilder n
  ]
