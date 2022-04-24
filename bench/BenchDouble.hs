-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>

module BenchDouble (benchDouble) where

import qualified Data.Text as T
import Data.Text.Builder.Linear.Buffer
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.RealFloat (realFloat)
import Test.Tasty.Bench

#ifdef MIN_VERSION_text_builder
import qualified Text.Builder
#endif

dbl :: Double
dbl = - pi * 1e300

benchLazyBuilder ∷ Int → T.Text
benchLazyBuilder = toStrict . toLazyText . go mempty
  where
    go !acc 0 = acc
    go !acc n = let d = fromIntegral n * dbl in go (realFloat d <> (acc <> realFloat d)) (n - 1)

#ifdef MIN_VERSION_text_builder
benchStrictBuilder ∷ Int → T.Text
benchStrictBuilder = Text.Builder.run . go mempty
  where
    go !acc 0 = acc
    go !acc n = let d = fromIntegral n * dbl in go (Text.Builder.fixedDouble 17 d <> (acc <> Text.Builder.fixedDouble 17 d)) (n - 1)
#endif

benchLinearBuilder ∷ Int → T.Text
benchLinearBuilder m = runBuffer (\b → go b m)
  where
    go ∷ Buffer ⊸ Int → Buffer
    go !acc 0 = acc
    go !acc n = let d = fromIntegral n * dbl in go (d %<| (acc |>% d)) (n - 1)

benchDouble ∷ Benchmark
benchDouble = bgroup "Double" $ map mkGroup [1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6]

mkGroup :: Int → Benchmark
mkGroup n = bgroup (show n)
  [ bench "Data.Text.Lazy.Builder" $ nf benchLazyBuilder n
#ifdef MIN_VERSION_text_builder
  , bench "Text.Builder" $ nf benchStrictBuilder n
#endif
  , bench "Data.Text.Builder.Linear" $ nf benchLinearBuilder n
  ]
