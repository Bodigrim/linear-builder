-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Generate a markdown table used in README
-- from benchmark results in CSV.

module Main
  ( main
  ) where

import Control.Applicative
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Word
import Text.Printf

data Title = Text | Char | Decimal | Hexadecimal | Double
  deriving (Eq, Ord, Show, Read)

data Group = Group
  { grpTextLazy   :: Maybe Word64
  , grpBSLazy     :: Maybe Word64
  , grpTextStrict :: Maybe Word64
  , grpBSStrict   :: Maybe Word64
  , grpLinear     :: Maybe Word64
  }

instance Monoid Group where
  mempty = Group Nothing Nothing Nothing Nothing Nothing

instance Semigroup Group where
  Group a b c d e <> Group a' b' c' d' e' =
    Group (a <|> a') (b <|> b') (c <|> c') (d <|> d') (e <|> e')

parseLine :: String -> Map Title (Map Int Group)
parseLine xs = M.singleton title (M.singleton size group)
  where
    (ys, _ : zs) = span (/= ',') xs
    time = read (takeWhile (/= ',') zs)
    (ts, _ : us) = span (/= '.') (drop (length "All.") ys)
    title = read ts
    (vs, _ : ws) = span (/= '.') us
    size = read vs
    group = case ws of
      "Data.Text.Lazy.Builder" -> mempty { grpTextLazy = Just time }
      "Data.ByteString.Builder" -> mempty { grpBSLazy = Just time }
      "Text.Builder" -> mempty { grpTextStrict = Just time }
      "ByteString.StrictBuilder" -> mempty { grpBSStrict = Just time }
      "Data.Text.Builder.Linear" -> mempty { grpLinear = Just time }

parseLines :: [String] -> Map Title (Map Int Group)
parseLines xss = M.unionsWith (M.unionWith (<>)) (map parseLine xss)

dumpTextTable :: Map Title (Map Int Group) -> String
dumpTextTable mp = unlines
  $ "|Group / size|`text`|`text-builder`|  |This package|  |"
  : "|------------|-----:|-------------:|-:|-----------:|-:|"
  : M.foldMapWithKey dumpBlock mp
  where
    dumpBlock :: Title -> Map Int Group -> [String]
    dumpBlock title sizes = mkHeader (show title) : M.foldMapWithKey dumpRows sizes

    dumpRows :: Int -> Group -> [String]
    dumpRows size gr = [ mkRow size (grpTextLazy gr) (grpTextStrict gr) (grpLinear gr) ]

dumpBSTable :: Map Title (Map Int Group) -> String
dumpBSTable mp = unlines
  $ "|Group / size|`bytestring`|`…-strict-builder`|  |This package|  |"
  : "|------------|-----------:|-----------------:|-:|-----------:|-:|"
  : M.foldMapWithKey dumpBlock mp
  where
    dumpBlock :: Title -> Map Int Group -> [String]
    dumpBlock title sizes = mkHeader (show title) : M.foldMapWithKey dumpRows sizes

    dumpRows :: Int -> Group -> [String]
    dumpRows size gr = [ mkRow size (grpBSLazy gr) (grpBSStrict gr) (grpLinear gr) ]

mkRow :: Int -> Maybe Word64 -> Maybe Word64 -> Maybe Word64 -> String
mkRow size mBase mRes1 mRes2 = row
  where
    baseTime = maybe "" showPicos4 mBase
    res1Time = maybe "" showPicos4 mRes1
    res2Time = maybe "" showPicos4 mRes2

    divide x y = (fromIntegral y / fromIntegral x) :: Double
    ratio1 = maybe "" (printf "%.2fx") (divide <$> mBase <*> mRes1)
    ratio2 = maybe "" (printf "%.2fx") (divide <$> mBase <*> mRes2)

    row = "|" ++ show size ++ "|" ++ baseTime ++ "|" ++ res1Time ++ "|" ++ ratio1 ++ "|" ++ res2Time ++ "|" ++ ratio2 ++ "|"

mkHeader :: String -> String
mkHeader xs = "| **" ++ xs ++ "** ||||||"

showPicos4 :: Word64 -> String
showPicos4 i
  | t < 995   = printf "%3.0f ps" t
  | t < 995e1 = printf "%4.2f ns" (t / 1e3)
  | t < 995e2 = printf "%4.1f ns" (t / 1e3)
  | t < 995e3 = printf "%3.0f ns" (t / 1e3)
  | t < 995e4 = printf "%4.2f μs" (t / 1e6)
  | t < 995e5 = printf "%4.1f μs" (t / 1e6)
  | t < 995e6 = printf "%3.0f μs" (t / 1e6)
  | t < 995e7 = printf "%4.2f ms" (t / 1e9)
  | t < 995e8 = printf "%4.1f ms" (t / 1e9)
  | t < 995e9 = printf "%3.0f ms" (t / 1e9)
  | otherwise = printf "%4.3f s"  (t / 1e12)
  where
    t = fromIntegral i :: Double

main :: IO ()
main = do
  csv <- getContents
  let mp = parseLines $ tail $ lines csv
  putStrLn $ dumpTextTable mp
  putStrLn $ dumpBSTable mp
