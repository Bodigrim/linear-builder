# text-builder-linear [![Hackage](http://img.shields.io/hackage/v/text-builder-linear.svg)](https://hackage.haskell.org/package/text-builder-linear) [![Stackage LTS](http://stackage.org/package/text-builder-linear/badge/lts)](http://stackage.org/lts/package/text-builder-linear) [![Stackage Nightly](http://stackage.org/package/text-builder-linear/badge/nightly)](http://stackage.org/nightly/package/text-builder-linear)

_Linear types for linear times!_

Builder for strict `Text`, based on linear types. It's consistently
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
Users are encouraged to use `Buffer` API, and built-in benchmarks refer to it.

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
build-depends: base, tasty-bench, text, text-builder, text-builder-linear
ghc-options: -O2
-}

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Text.Builder as TB
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
  , mkBench "text-builder" TB.char (T.length . TB.run)
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

Things get very different if we remove `{-# INLINE mkBench #-}`:

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

## Benchmarks

|Group / size|`text`|`text-builder`|Ratio|This package|Ratio|
|------------|-----:|-------------:|-:|-----------:|-:|
| **Text** ||||||
|1|69.2 ns|37.0 ns|0.53x|36.8 ns|0.53x|
|10|736 ns|344 ns|0.47x|190 ns|0.26x|
|100|7.07 μs|3.42 μs|0.48x|1.81 μs|0.26x|
|1000|74.2 μs|38.5 μs|0.52x|14.4 μs|0.19x|
|10000|1.10 ms|477 μs|0.43x|163 μs|0.15x|
|100000|23.1 ms|11.6 ms|0.50x|4.17 ms|0.18x|
|1000000|282 ms|166 ms|0.59x|40.4 ms|0.14x|
| **Char** ||||||
|1|83.2 ns|34.8 ns|0.42x|34.8 ns|0.42x|
|10|378 ns|302 ns|0.80x|123 ns|0.33x|
|100|3.14 μs|2.46 μs|0.78x|922 ns|0.29x|
|1000|34.9 μs|31.3 μs|0.90x|9.37 μs|0.27x|
|10000|494 μs|454 μs|0.92x|101 μs|0.20x|
|100000|15.9 ms|13.8 ms|0.87x|1.64 ms|0.10x|
|1000000|212 ms|227 ms|1.07x|14.5 ms|0.07x|
| **Decimal** ||||||
|1|147 ns|993 ns|6.76x|106 ns|0.72x|
|10|1.36 μs|10.1 μs|7.43x|845 ns|0.62x|
|100|13.5 μs|108 μs|7.97x|8.44 μs|0.62x|
|1000|136 μs|1.34 ms|9.84x|83.0 μs|0.61x|
|10000|1.85 ms|22.0 ms|11.86x|822 μs|0.44x|
|100000|33.9 ms|237 ms|7.00x|10.4 ms|0.31x|
|1000000|399 ms|2.504 s|6.28x|89.8 ms|0.23x|
| **Hexadecimal** ||||||
|1|599 ns|940 ns|1.57x|98.9 ns|0.17x|
|10|6.05 μs|9.89 μs|1.64x|916 ns|0.15x|
|100|66.4 μs|121 μs|1.82x|9.61 μs|0.14x|
|1000|807 μs|1.47 ms|1.82x|96.7 μs|0.12x|
|10000|13.0 ms|20.8 ms|1.60x|980 μs|0.08x|
|100000|152 ms|223 ms|1.47x|11.7 ms|0.08x|
|1000000|1.657 s|2.228 s|1.34x|104 ms|0.06x|
| **Double** ||||||
|1|11.9 μs|26.6 μs|2.23x|632 ns|0.05x|
|10|117 μs|270 μs|2.30x|6.32 μs|0.05x|
|100|1.20 ms|3.68 ms|3.06x|64.5 μs|0.05x|
|1000|12.8 ms|43.9 ms|3.44x|638 μs|0.05x|
|10000|126 ms|457 ms|3.63x|7.38 ms|0.06x|
|100000|1.266 s|4.717 s|3.73x|65.9 ms|0.05x|
|1000000|12.599 s|65.467 s|5.20x|653 ms|0.05x|

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
