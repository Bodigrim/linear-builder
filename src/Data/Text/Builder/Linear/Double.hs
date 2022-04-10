-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>

module Data.Text.Builder.Linear.Double
  ( (|>%)
  , (%<|)
  ) where

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Internal as BBI
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy.Internal as BLI
import qualified Data.Text.Array as A
import GHC.Exts
import GHC.ForeignPtr (unsafeWithForeignPtr)
import GHC.IO (unsafeIOToST, unsafeSTToIO)
import GHC.ST

import Data.Text.Builder.Linear.Core

-- | Append double.
(|>%) :: Buffer ⊸ Double -> Buffer
infixl 6 |>%
buffer |>% x = appendBounded
  22 -- length "-3.141592653589793e300"
  (\dst dstOff -> unsafeAppendDouble dst dstOff x)
  buffer

-- | Prepend double
(%<|) :: Double -> Buffer ⊸ Buffer
infixr 6 %<|
x %<| buffer = prependBounded
  22 -- length "-3.141592653589793e300"
  (\dst dstOff -> unsafePrependDouble dst dstOff x)
  (\dst dstOff -> unsafeAppendDouble dst dstOff x)
  buffer

unsafeAppendDouble :: A.MArray s -> Int -> Double -> ST s Int
unsafeAppendDouble dst dstOff x = do
  let (fp, srcLen) = case BBI.toLazyByteStringWith (BBI.untrimmedStrategy 32 32) BLI.Empty (BB.doubleDec x) of
        BLI.Chunk bs _ -> BI.toForeignPtr0 bs
        _ -> undefined
  unsafeIOToST $ unsafeWithForeignPtr fp $ \(Ptr addr#) ->
    unsafeSTToIO $ A.copyFromPointer dst dstOff (Ptr addr#) srcLen
  pure srcLen

unsafePrependDouble :: A.MArray s -> Int -> Double -> ST s Int
unsafePrependDouble dst dstOff x = do
  let (fp, srcLen) = case BBI.toLazyByteStringWith (BBI.untrimmedStrategy 32 32) BLI.Empty (BB.doubleDec x) of
        BLI.Chunk bs _ -> BI.toForeignPtr0 bs
        _ -> undefined
  unsafeIOToST $ unsafeWithForeignPtr fp $ \(Ptr addr#) ->
    unsafeSTToIO $ A.copyFromPointer dst (dstOff - srcLen) (Ptr addr#) srcLen
  pure srcLen
