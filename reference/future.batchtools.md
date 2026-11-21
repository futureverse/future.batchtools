# future.batchtools: A Future for batchtools

The future.batchtools package implements the Future API on top of
batchtools such that futures can be resolved on for instance
high-performance compute (HPC) clusters via job schedulers. The Future
API is defined by the future package.

## Details

To use batchtools futures, attach future, and select the type of future
you wish to use via
[`future::plan()`](https://future.futureverse.org/reference/plan.html).

## Example

    library(futurize)                          # also attaches 'future'
    plan(future.batchtools::batchtools_slurm)  # parallelize via Slurm jobs

    # A slow function
    slow_fcn <- function(x) {
      Sys.sleep(1.0)
      x^2
    }

    # Example data
    xs <- 1:10

    # Sequential and parallel base R apply
    ys <- lapply(xs, slow_fcn)
    ys <- lapply(xs, slow_fcn) |> futurize()

    # Sequential and parallel purrr map
    library(purrr)
    ys <- xs |> map(slow_fcn)
    ys <- xs |> map(slow_fcn) |> futurize()

    # Sequential and parallel foreach calls
    library(foreach)
    ys <- foreach(x = xs) %do% slow_fcn(x)
    ys <- foreach(x = xs) %do% slow_fcn(x) |> futurize()

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

- Bernd Bischl ([ORCID](https://orcid.org/0000-0001-6002-6980)) (Code
  adapted from the 'batchtools' package) \[contributor, copyright
  holder\]
