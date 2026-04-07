# A batchtools backend that resolves futures in parallel via forked background R processes

A batchtools backend that resolves futures in parallel via forked
background R processes

## Usage

``` r
batchtools_multicore(
  ...,
  workers = availableCores(constraints = "multicore"),
  fs.latency = 0,
  delete = getOption("future.batchtools.delete", "on-success")
)
```

## Arguments

- workers:

  The number of multicore processes to be available for concurrent
  batchtools multicore futures.

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

  Not used.

## Details

Batchtools multicore futures use batchtools cluster functions created by
[`batchtools::makeClusterFunctionsMulticore()`](https://batchtools.mlr-org.com/reference/makeClusterFunctionsMulticore.html)
with `ncpus = workers`.

An alternative to the batchtools multicore backend is to use
`plan(future::multicore)`.

## Examples

``` r
if (FALSE) { # interactive()
library(future)
plan(future.batchtools::batchtools_multicore, workers = 2)

message("Main process ID: ", Sys.getpid())

f <- future({
  data.frame(
    hostname = Sys.info()[["nodename"]],
          os = Sys.info()[["sysname"]],
       cores = unname(parallelly::availableCores()),
         pid = Sys.getpid(),
     modules = Sys.getenv("LOADEDMODULES")
  )
})
info <- value(f)
print(info)
}
```
