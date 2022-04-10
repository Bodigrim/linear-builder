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
import qualified Data.Text.Lazy.Builder.Int as TLBI
import qualified Data.Text.Lazy.Builder.RealFloat as TLBR
import Test.Tasty.Bench

txt ∷ Text
txt = T.pack "Haskell + Linear Types = ♡"

chr :: Char
chr = '♡'

int :: Int
int = 123456789123456789

dbl :: Double
dbl = - pi * 1e300

benchBuilder ∷ Int → Text
benchBuilder = TL.toStrict . TLB.toLazyText . go txtB
  where
    txtB = TLB.fromText txt
    go !acc 0 = acc
    go !acc n = go (txtB <> (acc <> txtB)) (n - 1)

benchLinearBuilder ∷ Int → Text
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
