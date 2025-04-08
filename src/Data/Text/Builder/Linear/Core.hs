-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
--              (c) 2023 Pierre Le Marre
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Low-level routines for t'Buffer' manipulations.
module Data.Text.Builder.Linear.Core (
  -- * Type
  Buffer,

  -- * Basic interface
  runBuffer,
  runBufferBS,
  dupBuffer,
  consumeBuffer,
  eraseBuffer,
  byteSizeOfBuffer,
  lengthOfBuffer,
  dropBuffer,
  takeBuffer,
  newEmptyBuffer,

  -- * Text concatenation
  appendBounded,
  appendExact,
  prependBounded,
  prependExact,
  (><),
) where

import Data.Text.Builder.Linear.Internal
