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

import Data.Function
import Data.List
import Data.Word
import Text.Printf

main :: IO ()
main = do
  csv <- getContents
  let ls = tail $ lines csv
      triples = mkTriples ls
      rows = map processTriple triples
      table = map (\xs -> mkHeader (fst (head xs)) : map snd xs) $ groupBy ((==) `on` fst) rows
  putStrLn "|Group / size|`text`|`text-builder`|  |This package|  |"
  putStrLn "|------------|-----:|-------------:|-:|-----------:|-:|"
  putStrLn $ unlines $ concat table

mkHeader :: String -> String
mkHeader xs = "| **" ++ xs ++ "** ||||||"

mkTriples :: [a] -> [(a, a, a)]
mkTriples (x : _ : y : z : ts) = (x, y, z) : mkTriples ts
mkTriples _ = []

parseCells :: String -> (String, Word64)
parseCells xs = (ys, read (takeWhile (/= ',') zs))
  where
    (ys, _ : zs) = span (/= ',') xs

parseName :: String -> (String, Int)
parseName xs = (ys, read (takeWhile (/= '.') zs))
  where
    (ys, _ : zs) = span (/= '.') (drop (length "All.") xs)

processTriple :: (String, String, String) -> (String, String)
processTriple (x, y, z) = (title, row)
  where
    (xName, xTime) = parseCells x
    (_, yTime) = parseCells y
    (_, zTime) = parseCells z
    (title, size) = parseName xName
    yRatio = fromIntegral yTime / fromIntegral xTime :: Double
    zRatio = fromIntegral zTime / fromIntegral xTime :: Double
    row = "|" ++ show size ++ "|" ++ showPicos4 xTime ++ "|" ++ showPicos4 yTime ++ "|" ++ printf "%.2fx" yRatio ++ "|" ++ showPicos4 zTime ++ "|" ++ printf "%.2fx" zRatio ++ "|"

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
