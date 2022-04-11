# linear-builder [![Hackage](http://img.shields.io/hackage/v/linear-builder.svg)](https://hackage.haskell.org/package/linear-builder) [![Stackage LTS](http://stackage.org/package/linear-builder/badge/lts)](http://stackage.org/lts/package/linear-builder) [![Stackage Nightly](http://stackage.org/package/linear-builder/badge/nightly)](http://stackage.org/nightly/package/linear-builder)

_Linear types for linear times!_

Builder for strict `Text`, based on linear types. It's consistently
outperforms `Data.Text.Lazy.toStrict . Data.Text.Lazy.Builder.toLazyText`
and scales better:

```
1
  Data.Text.Lazy.Builder:
    73.9 ns ± 6.8 ns
  Data.Text.Builder.Linear:
    33.0 ns ± 2.2 ns, 0.45x
10
  Data.Text.Lazy.Builder:
    702  ns ±  70 ns
  Data.Text.Builder.Linear:
    178  ns ±  16 ns, 0.25x
100
  Data.Text.Lazy.Builder:
    6.57 μs ± 292 ns
  Data.Text.Builder.Linear:
    1.58 μs ± 116 ns, 0.24x
1000
  Data.Text.Lazy.Builder:
    74.7 μs ± 2.9 μs
  Data.Text.Builder.Linear:
    13.9 μs ± 1.2 μs, 0.19x
10000
  Data.Text.Lazy.Builder:
    1.63 ms ±  64 μs
  Data.Text.Builder.Linear:
    220  μs ±  16 μs, 0.13x
100000
  Data.Text.Lazy.Builder:
    26.2 ms ± 2.5 ms
  Data.Text.Builder.Linear:
    3.25 ms ± 294 μs, 0.12x
1000000
  Data.Text.Lazy.Builder:
    302  ms ± 8.9 ms
  Data.Text.Builder.Linear:
    30.8 ms ± 3.1 ms, 0.10x
```

And here are benchmarks for `blaze-markup` after migration to `Data.Text.Builder.Linear`:

```
bigTable
  992  μs ±  80 μs, 49% faster than baseline
basic
  4.35 μs ± 376 ns, 47% faster than baseline
wideTree
  1.26 ms ±  85 μs, 53% faster than baseline
wideTreeEscaping
  217  μs ± 7.8 μs, 58% faster than baseline
deepTree
  242  μs ±  23 μs, 48% faster than baseline
manyAttributes
  811  μs ±  79 μs, 58% faster than baseline
customAttribute
  1.68 ms ± 135 μs, 56% faster than baseline
```
