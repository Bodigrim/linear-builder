module Main (main) where

import Data.Bits (shiftL)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Text.Builder.Linear.Buffer (runBuffer)
import Data.Text.Builder.Linear.Dec.Unbounded (Strategy(..), prependUnboundedDecimal)
import Test.Tasty (mkTimeout)
import Test.Tasty.Bench.Crossover (crossovers, mkCrossoverConfig, eqlTimeout)

mkFunc :: Strategy -> Word -> Text
mkFunc s = (\p -> runBuffer (\b -> prependUnboundedDecimal s ((1 :: Integer) `shiftL` fromIntegral (p * 64 - 1)) b))

main :: IO ()
main = do
  let cnf = mkCrossoverConfig (mkFunc SmallOnly) (mkFunc HugeOnly) (1, 10000)
      cnf' = cnf { eqlTimeout = mkTimeout 1e9 }
  crossovers cnf' >>= traverse_ print
