# Batchtools multicore futures

A batchtools multicore future is an asynchronous multiprocess future
that will be evaluated in a background R session.\
\
*We highly recommend using
[future::multisession](https://future.futureverse.org/reference/multisession.html)
(sic!) futures of the future package instead of multicore batchtools
futures.*

## Usage

``` r
BatchtoolsMulticoreFutureBackend(
  workers = availableCores(constraints = "multicore"),
  fs.latency = 0,
  delete = getOption("future.batchtools.delete", "on-success"),
  ...
)
```

## Arguments

- workers:

  The number of multicore processes to be available for concurrent
  batchtools multicore futures. If a single worker is requested, which
  is the default when only one CPU core is available, the batchtools
  local backend is used instead. It is synchronous, that is, the main R
  process blocks while a job is running. To use an asynchronous
  batchtools multicore backend also with a single worker, protect the
  number of workers using
  [`base::I()`](https://rdrr.io/r/base/AsIs.html), e.g.
  `workers = I(1)`.

- fs.latency:

  \[`numeric(1)`\]\
  Expected maximum latency of the file system, in seconds. Set to a
  positive number for network file systems like NFS which enables more
  robust (but also more expensive) mechanisms to access files and
  directories. Usually safe to set to `0` to disable the heuristic, e.g.
  if you are working on a local file system.

- delete:

  Controls if and when the batchtools job registry folder is deleted. If
  `"on-success"` (default), it is deleted if the future was resolved
  successfully *and* the expression did not produce an error. If
  `"never"`, then it is never deleted. If `"always"`, then it is always
  deleted.

- ...:

  Additional arguments passed to
  [`BatchtoolsFutureBackend()`](https://future.batchtools.futureverse.org/reference/BatchtoolsFutureBackend.md).

## Value

An object of class `BatchtoolsMulticoreFuture`.

## Details

Batchtools multicore futures rely on the batchtools backend set up by
[`batchtools::makeClusterFunctionsMulticore()`](https://batchtools.mlr-org.com/reference/makeClusterFunctionsMulticore.html).
The batchtools multicore backend only works on operating systems
supporting the `ps` command-line tool, e.g. Linux and macOS.
