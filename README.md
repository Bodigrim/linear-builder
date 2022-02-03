# linear-builder [![Hackage](http://img.shields.io/hackage/v/linear-builder.svg)](https://hackage.haskell.org/package/linear-builder) [![Stackage LTS](http://stackage.org/package/linear-builder/badge/lts)](http://stackage.org/lts/package/linear-builder) [![Stackage Nightly](http://stackage.org/package/linear-builder/badge/nightly)](http://stackage.org/nightly/package/linear-builder)

_Linear types for linear times!_

Builder for strict `Text`, based on linear types. It's consistently outperforms `Data.Text.Lazy.toStrict . Data.Text.Lazy.Builder.toLazyText` and scales better:

```
100
  Data.Text.Lazy.Builder:
    6.63 μs ± 188 ns
  Data.Text.Builder.Linear:
    2.91 μs ±  68 ns, 0.44x
1000
  Data.Text.Lazy.Builder:
    76.7 μs ± 3.0 μs
  Data.Text.Builder.Linear:
    27.0 μs ± 572 ns, 0.35x
10000
  Data.Text.Lazy.Builder:
    1.70 ms ±  26 μs
  Data.Text.Builder.Linear:
    354  μs ±  11 μs, 0.21x
100000
  Data.Text.Lazy.Builder:
    27.1 ms ± 437 μs
  Data.Text.Builder.Linear:
    3.14 ms ±  58 μs, 0.12x
1000000
  Data.Text.Lazy.Builder:
    309  ms ± 9.5 ms
  Data.Text.Builder.Linear:
    33.8 ms ± 401 μs, 0.11x
```
