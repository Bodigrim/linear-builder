module Main (main) where

import Data.Bits
import Data.Foldable
import Data.Text (Text)
import Data.Text.Builder.Linear.Buffer
import Data.Text.Builder.Linear.Dec.Unbounded
import Test.Tasty.Bench.Equalize (equalize, mkEqualizeConfig)

mkFunc :: Strategy -> Word -> Text
mkFunc s = (\p -> runBuffer (\b -> prependUnboundedDecimal s ((1 :: Integer) `shiftL` fromIntegral (p * 64 - 1)) b))

main :: IO ()
main = do
  putStrLn "SmallOnly vs. BigOnly"
  xs <- equalize $ mkEqualizeConfig (mkFunc SmallOnly) (mkFunc BigOnly) (1, 10000)
  traverse_ print xs
  putStrLn "BigOnly vs. HugeOnly"
  ys <- equalize $ mkEqualizeConfig (mkFunc BigOnly) (mkFunc HugeOnly) (1, 10000)
  traverse_ print ys
