# linear-builder [![Hackage](http://img.shields.io/hackage/v/linear-builder.svg)](https://hackage.haskell.org/package/linear-builder) [![Stackage LTS](http://stackage.org/package/linear-builder/badge/lts)](http://stackage.org/lts/package/linear-builder) [![Stackage Nightly](http://stackage.org/package/linear-builder/badge/nightly)](http://stackage.org/nightly/package/linear-builder)

_Linear types for linear times!_

Builder for strict `Text`, based on linear types. It's consistently
outperforms lazy `Builder` from `text` as well as a strict builder from `text-builder`,
and scales better.

## Design

WIP

## Benchmarks

|Group / size|`text`|`text-builder`|  |This package|  |
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
here are benchmarks for `blaze-markup` after migration to `Data.Text.Builder.Linear`:

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
