# future.batchtools: A Future for batchtools

The future.batchtools package implements the Future API on top of
batchtools such that futures can be resolved on for instance
high-performance compute (HPC) clusters via job schedulers. The Future
API is defined by the future package.

## Details

To use batchtools futures, load future.batchtools, and select the type
of future you wish to use via
[`future::plan()`](https://future.futureverse.org/reference/plan.html).

## See also

Useful links:

- <https://future.batchtools.futureverse.org>

- <https://github.com/futureverse/future.batchtools>

- Report bugs at
  <https://github.com/futureverse/future.batchtools/issues>

## Author

**Maintainer**: Henrik Bengtsson <henrikb@braju.com>
([ORCID](https://orcid.org/0000-0002-7579-5165)) \[copyright holder\]

Authors:

- Henrik Bengtsson <henrikb@braju.com>
  ([ORCID](https://orcid.org/0000-0002-7579-5165)) \[copyright holder\]

Other contributors:

- Michel Lang ([ORCID](https://orcid.org/0000-0001-9754-0393)) (Code
  adapted from the 'batchtools' package) \[contributor, copyright
  holder\]

- Bernd Bischl (Code adapted from the 'batchtools' package)
  \[contributor, copyright holder\]

## Examples

``` r
if (FALSE) { # interactive()
library(future)
plan(future.batchtools::batchtools_local)
demo("mandelbrot", package = "future", ask = FALSE)
}
```
