# linear-builder [![Hackage](http://img.shields.io/hackage/v/linear-builder.svg)](https://hackage.haskell.org/package/linear-builder) [![Stackage LTS](http://stackage.org/package/linear-builder/badge/lts)](http://stackage.org/lts/package/linear-builder) [![Stackage Nightly](http://stackage.org/package/linear-builder/badge/nightly)](http://stackage.org/nightly/package/linear-builder)

Strict `Text` builder. Linear types for linear times!

```
Data.Text
  100:
    23.1 μs ± 2.0 μs
  1000:
    2.28 ms ± 167 μs
  10000:
    250  ms ±  21 ms
Data.Text.Lazy.Builder
  100:
    7.15 μs ± 413 ns
  1000:
    81.3 μs ± 7.7 μs
  10000:
    1.67 ms ± 135 μs
  100000:
    28.3 ms ± 2.0 ms
  1000000:
    327  ms ±  21 ms
Data.Text.Builder.Linear
  100:
    3.00 μs ± 135 ns
  1000:
    28.5 μs ± 1.1 μs
  10000:
    405  μs ±  36 μs
  100000:
    3.33 ms ± 193 μs
  1000000:
    30.6 ms ± 2.7 ms
```
