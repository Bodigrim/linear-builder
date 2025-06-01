# text-builder-linear [![Hackage](http://img.shields.io/hackage/v/text-builder-linear.svg)](https://hackage.haskell.org/package/text-builder-linear) [![Stackage LTS](http://stackage.org/package/text-builder-linear/badge/lts)](http://stackage.org/lts/package/text-builder-linear) [![Stackage Nightly](http://stackage.org/package/text-builder-linear/badge/nightly)](http://stackage.org/nightly/package/text-builder-linear)

_Linear types for linear times!_

Builder for strict `Text` and `ByteString`, based on linear types. It consistently
outperforms lazy `Builder` from `text` as well as a strict builder from `text-builder`,
and scales better.

## Example

```haskell
> :set -XOverloadedStrings
> import Data.Text.Builder.Linear
> fromText "foo" <> fromChar '_' <> fromDec (42 :: Int)
"foo_42"
```

## Design

String builders in Haskell serve the same purpose as `StringBuilder` in Java to prevent
quadratic slow down in concatenation.

Classic builders such as `Data.Text.Lazy.Builder` are lazy and fundamentally are
[`dlist`](https://hackage.haskell.org/package/dlist) with bells and whistles:
instead of actually concatenating substrings we compose actions, which implement
concatenation, building a tree of thunks. The tree can be forced partially, left-to-right,
producing chunks of strict `Text`, combined into a lazy one. Neither input, nor output need to be materialized in full, which potentially allows for fusion. Such builders allow
linear time complexity, but constant factors are relatively high, because thunks are
expensive. To a certain degree this is mitigated by inlining, which massively reduces
number of nodes.

Strict builders such as [`text-builder`](https://hackage.haskell.org/package/text-builder)
offer another design: they first inspect their input in full to determine output length,
then allocate a buffer of required size and fill it in one go. If everything inlines nicely,
the length may be known in compile time, which gives blazingly fast runtime. In more
complex cases it still builds a tree of thunks and forces all inputs to be materialized.

This package offers two interfaces. One is a mutable `Buffer` with linear API,
which operates very similar to `StringBuilder` in Java. It allocates a buffer
with extra space at the ends to append new strings. If there is not enough free space
to insert new data, it allocates a twice larger buffer and copies itself there.
The dispatch happens in runtime, so we do not need to inspect and materialize all inputs
beforehand; and inlining is mostly irrelevant.
Exponential growth provides for amortized linear time.
Such structure can be implemented without linear types, but that would
greatly affect user experience by polluting everything with `ST` monad.
Users are encouraged to use `Buffer` API, and **built-in benchmarks refer to it.**

The second interface is more traditional `newtype Builder = Builder (Buffer ⊸ Buffer)`
with `Monoid` instance. This type provides easy migration from other builders,
but may suffer from insufficient inlining, allocating a tree of thunks. It is still
significantly faster than `Data.Text.Lazy.Builder`, as witnessed by benchmarks
for `blaze-builder` below.

## Case study

Let's benchmark builders, which concatenate all `Char` from `minBound` to `maxBound`, producing a large `Text`:

```haskell
#!/usr/bin/env cabal
{- cabal:
build-depends: base, tasty-bench, text, text-builder >= 1.0, text-builder-linear
ghc-options: -O2
-}

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified TextBuilder as TB
import qualified Data.Text.Builder.Linear as TBL
import System.Environment (getArgs)
import Test.Tasty.Bench

mkBench :: Monoid a => String -> (Char -> a) -> (a -> Int) -> Benchmark
mkBench s f g = bench s $ nf (g . foldMap f . enumFromTo minBound) maxBound
{-# INLINE mkBench #-}

main :: IO ()
main = defaultMain
  [ mkBench "text, lazy" TLB.singleton (fromIntegral . TL.length . TLB.toLazyText)
  , mkBench "text, strict" TLB.singleton (T.length . TL.toStrict . TLB.toLazyText)
  , mkBench "text-builder" TB.char (T.length . TB.toText)
  , mkBench "text-builder-linear" TBL.fromChar (T.length . TBL.runBuilder)
  ]
```

Running this program with `cabal run Main.hs -- +RTS -T` yields following results:

```
text, lazy:
  4.25 ms ± 107 μs,  11 MB allocated, 912 B  copied
text, strict:
  7.18 ms ± 235 μs,  24 MB allocated,  10 MB copied
text-builder:
  80.1 ms ± 3.0 ms, 218 MB allocated, 107 MB copied
text-builder-linear:
  5.37 ms ± 146 μs,  44 MB allocated,  78 KB copied
```

The first result seems the best both in time and memory and corresponds to the
usual `Text` builder, where we do not materialize the entire result at all.
It builds chunks of lazy `Text` lazily and consumes them at once by
`TL.length`. Thus there are 11 MB of allocations in nursery, none of which
survive generation 0 garbage collector, so nothing is copied.

The second result is again the usual `Text` builder, but emulates a strict
consumer: we materialize a strict `Text` before computing length. Allocation
are doubled, and half of them (corresponding to the strict `Text`) survive to
the heap. Time is also almost twice longer, but still quite good.

The third result is for `text-builder` and demonstrates how bad things could
go with strict builders, aiming to precompute the precise length of the
buffer: allocating a thunk per char is tremendously slow and expensive.

The last result corresponds to the current package. We generate a strict
`Text` by growing and reallocating the buffer, thus allocations are quite
high. Nevertheless, it is already faster than the usual `Text` builder with
strict consumer and does not strain the garbage collector.

As of GHC 9.10, things get very different if we remove `{-# INLINE mkBench #-}`:

```
text, lazy:
  36.9 ms ± 599 μs, 275 MB allocated,  30 KB copied
text, strict:
  44.7 ms ± 1.3 ms, 287 MB allocated,  25 MB copied
text-builder:
  77.6 ms ± 2.2 ms, 218 MB allocated, 107 MB copied
text-builder-linear:
  5.35 ms ± 212 μs,  44 MB allocated,  79 KB copied
```

Builders from `text` package degrade rapidly, 6-8x slower and 10-20x more
allocations. That's because their constant factors rely crucially on
everything getting inlined, which makes their performance fragile and
unreliable in large-scale applications. On the bright side of things, our
builder remains as fast as before and now is a clear champion.

## Benchmarks for `Text`

Measured with GHC 9.12 on aarch64:

|Group / size|`text`|`text-builder`|  |This package|  |
|------------|-----:|-------------:|-:|-----------:|-:|
| **Text** ||||||
|1|63.3 ns|30.8 ns|0.49x|60.5 ns|0.95x|
|10|764 ns|267 ns|0.35x|319 ns|0.42x|
|100|7.53 μs|2.48 μs|0.33x|2.61 μs|0.35x|
|1000|80.5 μs|26.7 μs|0.33x|23.1 μs|0.29x|
|10000|949 μs|319 μs|0.34x|242 μs|0.26x|
|100000|18.5 ms|8.22 ms|0.44x|2.36 ms|0.13x|
|1000000|216 ms|107 ms|0.49x|22.9 ms|0.11x|
| **Char** ||||||
|1|49.0 ns|34.8 ns|0.71x|38.1 ns|0.78x|
|10|365 ns|293 ns|0.80x|117 ns|0.32x|
|100|3.20 μs|2.38 μs|0.74x|804 ns|0.25x|
|1000|35.4 μs|18.4 μs|0.52x|7.68 μs|0.22x|
|10000|460 μs|265 μs|0.58x|86.5 μs|0.19x|
|100000|12.7 ms|6.96 ms|0.55x|930 μs|0.07x|
|1000000|175 ms|178 ms|1.02x|10.5 ms|0.06x|
| **Decimal** ||||||
|1|148 ns|490 ns|3.30x|126 ns|0.85x|
|10|1.34 μs|4.80 μs|3.57x|1.07 μs|0.80x|
|100|14.3 μs|53.6 μs|3.76x|10.8 μs|0.76x|
|1000|148 μs|738 μs|5.00x|106 μs|0.72x|
|10000|1.66 ms|19.8 ms|11.96x|1.05 ms|0.63x|
|100000|28.3 ms|251 ms|8.88x|10.7 ms|0.38x|
|1000000|334 ms|2.803 s|8.40x|108 ms|0.32x|
| **Hexadecimal** ||||||
|1|711 ns|81.2 ns|0.11x|74.2 ns|0.10x|
|10|7.06 μs|795 ns|0.11x|510 ns|0.07x|
|100|76.3 μs|8.04 μs|0.11x|4.62 μs|0.06x|
|1000|862 μs|141 μs|0.16x|44.6 μs|0.05x|
|10000|12.4 ms|1.73 ms|0.14x|451 μs|0.04x|
|100000|138 ms|20.4 ms|0.15x|4.52 ms|0.03x|
|1000000|1.502 s|228 ms|0.15x|45.9 ms|0.03x|
| **Double** ||||||
|1|13.4 μs|35.3 μs|2.63x|638 ns|0.05x|
|10|137 μs|393 μs|2.88x|6.52 μs|0.05x|
|100|1.35 ms|5.62 ms|4.15x|67.9 μs|0.05x|
|1000|14.2 ms|71.9 ms|5.05x|671 μs|0.05x|
|10000|143 ms|750 ms|5.25x|7.18 ms|0.05x|
|100000|1.435 s|7.941 s|5.53x|70.4 ms|0.05x|
|1000000|14.366 s|101.342 s|7.05x|689 ms|0.05x|

If you are not convinced by synthetic data,
here are benchmarks for
[`blaze-markup` after migration to `Data.Text.Builder.Linear`](https://github.com/Bodigrim/blaze-markup):

```
bigTable
  992  μs ±  80 μs, 49% less than baseline
basic
  4.35 μs ± 376 ns, 47% less than baseline
wideTree
  1.26 ms ±  85 μs, 53% less than baseline
wideTreeEscaping
  217  μs ± 7.8 μs, 58% less than baseline
deepTree
  242  μs ±  23 μs, 48% less than baseline
manyAttributes
  811  μs ±  79 μs, 58% less than baseline
customAttribute
  1.68 ms ± 135 μs, 56% less than baseline
```

## Benchmarks for `ByteString`

Somewhat surprisingly, `text-builder-linear` now offers rendering to strict `ByteString`
as well. It gets consistently faster than `bytestring`
in all benchmarks except `Double` ones
once a string gets over 32k
(which is `defaultChunkSize` for `bytestring` builder). For mid-sized strings
`bytestring` is slightly faster in certain disciplines, mostly by virtue of using
`cbits` via FFI, while this package remains 100% native Haskell.

Benchmarks below were measured with GHC 9.12 on aarch64 and include comparison
to [`bytestring-strict-builder`](https://hackage.haskell.org/package/bytestring-strict-builder):

|Group / size|`bytestring`|`…-strict-builder`|  |This package|  |
|------------|-----------:|-----------------:|-:|-----------:|-:|
| **Text** ||||||
|1|156 ns|55.9 ns|0.36x|60.5 ns|0.39x|
|10|552 ns|374 ns|0.68x|319 ns|0.58x|
|100|4.71 μs|3.25 μs|0.69x|2.61 μs|0.55x|
|1000|41.7 μs|31.9 μs|0.76x|23.1 μs|0.56x|
|10000|438 μs|366 μs|0.84x|242 μs|0.55x|
|100000|7.58 ms|6.52 ms|0.86x|2.36 ms|0.31x|
|1000000|112 ms|88.1 ms|0.78x|22.9 ms|0.20x|
| **Char** ||||||
|1|138 ns|30.9 ns|0.22x|38.1 ns|0.28x|
|10|408 ns|136 ns|0.33x|117 ns|0.29x|
|100|2.96 μs|1.25 μs|0.42x|804 ns|0.27x|
|1000|30.4 μs|14.2 μs|0.47x|7.68 μs|0.25x|
|10000|394 μs|218 μs|0.55x|86.5 μs|0.22x|
|100000|11.8 ms|4.00 ms|0.34x|930 μs|0.08x|
|1000000|161 ms|112 ms|0.69x|10.5 ms|0.07x|
| **Decimal** ||||||
|1|209 ns|295 ns|1.41x|126 ns|0.60x|
|10|1.10 μs|2.67 μs|2.43x|1.07 μs|0.98x|
|100|9.76 μs|29.8 μs|3.05x|10.8 μs|1.11x|
|1000|100 μs|340 μs|3.40x|106 μs|1.06x|
|10000|1.02 ms|7.32 ms|7.15x|1.05 ms|1.02x|
|100000|14.6 ms|103 ms|7.04x|10.7 ms|0.73x|
|1000000|179 ms|1.233 s|6.87x|108 ms|0.60x|
| **Hexadecimal** ||||||
|1|131 ns|||74.2 ns|0.57x|
|10|360 ns|||510 ns|1.42x|
|100|2.76 μs|||4.62 μs|1.68x|
|1000|28.6 μs|||44.6 μs|1.56x|
|10000|330 μs|||451 μs|1.37x|
|100000|7.30 ms|||4.52 ms|0.62x|
|1000000|103 ms|||45.9 ms|0.45x|
| **Double** ||||||
|1|456 ns|||638 ns|1.40x|
|10|3.58 μs|||6.52 μs|1.82x|
|100|36.2 μs|||67.9 μs|1.87x|
|1000|367 μs|||671 μs|1.83x|
|10000|5.17 ms|||7.18 ms|1.39x|
|100000|59.0 ms|||70.4 ms|1.19x|
|1000000|605 ms|||689 ms|1.14x|
