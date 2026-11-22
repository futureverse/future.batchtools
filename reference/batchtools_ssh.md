# A batchtools backend that resolves futures in parallel via background R sessions over SSH

A batchtools backend that resolves futures in parallel via background R
sessions over SSH

## Usage

``` r
batchtools_ssh(
  ...,
  workers = availableWorkers(),
  fs.latency = 65,
  delete = getOption("future.batchtools.delete", "on-success"),
  registry = list(),
  finalize = getOption("future.finalize", TRUE)
)
```

## Arguments

- workers:

  (optional) The maximum number of workers the batchtools backend may
  use at any time. Interactive and "local" backends can only process one
  future at the time (`workers = 1`), whereas HPC backends, where
  futures are resolved via separate jobs on a scheduler, can have
  multiple workers. In the latter, the default is `workers = NULL`,
  which will resolve to
  `getOption("`[`future.batchtools.workers`](https://future.batchtools.futureverse.org/reference/zzz-future.batchtools.options.md)`", 100)`.

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

- registry:

  (optional) A named list of settings applied to each future's
  batchtools
  [Registry](https://batchtools.mlr-org.com/reference/makeRegistry.html).
  This is a more convenient alternative to using argument `conf.file`.

- finalize:

  If TRUE, a future's batchtools
  [Registry](https://batchtools.mlr-org.com/reference/makeRegistry.html)
  is automatically deleted when the future is garbage collected,
  otherwise not.

## Value

Nothing.

## Details

The `batchtools_ssh` backend uses the batchtools backend set up by
[`batchtools::makeClusterFunctionsSSH()`](https://batchtools.mlr-org.com/reference/makeClusterFunctionsSSH.html),
which requires system commands `ssh` and `ps` as available on Linux and
macOS.

An alternative to `batchtools_ssh` is to use
[cluster](https://future.futureverse.org/reference/cluster.html) futures
of the future package with a single local background session, i.e.
`plan(cluster, workers = c("localhost"))`.
