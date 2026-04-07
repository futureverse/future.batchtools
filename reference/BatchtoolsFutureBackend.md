# A batchtools future is a future whose value will be resolved via batchtools

A batchtools local future is a synchronous uniprocess future that will
be evaluated in a background R session. A batchtools interactive future
is a synchronous uniprocess future that will be evaluated in the current
R session (and variables will be assigned to the calling environment
rather than to a local one). Both types of futures will block until the
futures are resolved.

A batchtools SSH future is an asynchronous multiprocess future that will
be evaluated in a background R session.\
\
*We highly recommend using
[future::multisession](https://future.futureverse.org/reference/multisession.html)
(sic!) futures of the future package instead of SSH batchtools futures.*

## Usage

``` r
BatchtoolsFutureBackend(
  workers = 1L,
  resources = list(),
  finalize = getOption("future.finalize", TRUE),
  cluster.functions = NULL,
  registry = list(),
  conf.file = findConfFile(),
  interrupts = TRUE,
  delete = getOption("future.batchtools.debug", "on-success"),
  ...
)

BatchtoolsCustomFutureBackend(...)

BatchtoolsInteractiveFutureBackend(fs.latency = 0, ...)

BatchtoolsLocalFutureBackend(fs.latency = 0, ...)

BatchtoolsSSHFutureBackend(workers = availableWorkers(), fs.latency = 65, ...)
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

- resources:

  (optional) A named list passed to the batchtools job-script template
  as variable `resources`. This is based on how
  [`batchtools::submitJobs()`](https://batchtools.mlr-org.com/reference/submitJobs.html)
  works, with the exception for specially reserved names defined by the
  future.batchtools package;

  - `resources[["details"]]`, if TRUE, results in the job script
    outputting job details and job summaries at the beginning and at the
    end.

  - `resources[["startup"]]` and `resources[["shutdown"]]` are character
    vectors of shell code to be injected to the job script as-is.

  - `resources[["modules"]]` is character vector of Linux environment
    modules to be loaded.

  - `resources[["envs"]]`, is an optional names character vector
    specifying environment variables to be set.

  - `resources[["rscript"]]` is an optional character vector specifying
    how the 'Rscript' is launched. The `resources[["rscript_args"]]`
    field is an optional character vector specifying the 'Rscript'
    command-line arguments.

  - `resources[["asis"]]` is a character vector that are passed as-is to
    the job script and are injected as job resource declarations.

  - All remaining `resources` named elements are injected as named
    resource specification for the scheduler.

- finalize:

  If TRUE, a future's batchtools
  [Registry](https://batchtools.mlr-org.com/reference/makeRegistry.html)
  is automatically deleted when the future is garbage collected,
  otherwise not.

- cluster.functions:

  (optional) Assigned as-is to the each future's batchtools
  [Registry](https://batchtools.mlr-org.com/reference/makeRegistry.html).

- registry:

  (optional) A named list of settings applied to each future's
  batchtools
  [Registry](https://batchtools.mlr-org.com/reference/makeRegistry.html).
  This is a more convenient alternative to using argument `conf.file`.

- conf.file:

  (optional) A "batchtools-configuration" R script, which is sourced
  when each future's batchtools
  [Registry](https://batchtools.mlr-org.com/reference/makeRegistry.html)
  is created. Any variables created by this script is assigned to the
  registry. The default file is the one found by
  [`batchtools::findConfFile()`](https://batchtools.mlr-org.com/reference/findConfFile.html),
  if any.

- interrupts:

  If FALSE, attempts to interrupt futures will not take place on this
  backend, even if the backend supports it. This is useful when, for
  instance, it takes a long time to interrupt a future.

- delete:

  Controls if and when the batchtools job registry folder is deleted. If
  `"on-success"` (default), it is deleted if the future was resolved
  successfully *and* the expression did not produce an error. If
  `"never"`, then it is never deleted. If `"always"`, then it is always
  deleted.

- fs.latency:

  \[`numeric(1)`\]\
  Expected maximum latency of the file system, in seconds. Set to a
  positive number for network file systems like NFS which enables more
  robust (but also more expensive) mechanisms to access files and
  directories. Usually safe to set to `0` to disable the heuristic, e.g.
  if you are working on a local file system.

- ...:

  Additional arguments passed to `BatchtoolsFutureBackend()`.

## Value

A
[future::FutureBackend](https://future.futureverse.org/reference/FutureBackend-class.html)
object of class BatchtoolsFutureBackend

An object of class `BatchtoolsFuture`.

An object of class `BatchtoolsUniprocessFuture`.

An object of class `BatchtoolsMulticoreFuture`.

## Details

Batchtools local futures rely on the batchtools backend set up by
[`batchtools::makeClusterFunctionsInteractive(external = TRUE)`](https://batchtools.mlr-org.com/reference/makeClusterFunctionsInteractive.html)
and batchtools interactive futures on the one set up by
[`batchtools::makeClusterFunctionsInteractive()`](https://batchtools.mlr-org.com/reference/makeClusterFunctionsInteractive.html).
These are supported by all operating systems.

An alternative to batchtools local futures is to use
[cluster](https://future.futureverse.org/reference/cluster.html) futures
of the future package with a single local background session, i.e.
`plan(cluster, workers = "localhost")`.

An alternative to batchtools interactive futures is to use
`plan(sequential, split = TRUE)` futures of the future package.

Batchtools SSH futures rely on the batchtools backend set up by
[`batchtools::makeClusterFunctionsSSH()`](https://batchtools.mlr-org.com/reference/makeClusterFunctionsSSH.html).
The batchtools SSH backend only works on operating systems supporting
the `ssh` and `ps` command-line tool, e.g. Linux and macOS.

## Examples

``` r
library(future)

## Create custom cluster functions (here same as "local")
cf <- batchtools::makeClusterFunctionsInteractive(external = TRUE)
print(cf)
#> ClusterFunctions for mode: Interactive
#>   List queued Jobs : FALSE
#>   List running Jobs: FALSE
#>   Kill Jobs        : FALSE
#>   Hooks            : -
str(cf)
#> List of 11
#>  $ name                : chr "Interactive"
#>  $ submitJob           :function (reg, jc)  
#>  $ killJob             : NULL
#>  $ listJobsQueued      : NULL
#>  $ listJobsRunning     : NULL
#>  $ array.var           : chr NA
#>  $ store.job.collection: logi TRUE
#>  $ store.job.files     : logi FALSE
#>  $ scheduler.latency   : num 0
#>  $ fs.latency          : num 0
#>  $ hooks               : list()
#>  - attr(*, "class")= chr "ClusterFunctions"

# Use custom batchtools backend
plan(future.batchtools::batchtools_custom, cluster.functions = cf)
print(plan())
#> batchtools_custom:
#> - args: function (..., cluster.functions = list(name = "Interactive", submitJob = function (reg, jc) { assertRegistry(reg, writeable = TRUE) assertClass(jc, "JobCollection") if (external) { runOSCommand(Rscript(), sprintf("-e \"batchtools::doJobCollection('%s', output = '%s')\"", jc$uri, jc$log.file)) } else { doJobCollection(jc, output = jc$log.file) } makeSubmitJobResult(status = 0L, batch.id = "cfInteractive") }, killJob = NULL, listJobsQueued = NULL, listJobsRunning = NULL, array.var = NA_character_, store.job.collection = TRUE, store.job.files = FALSE, scheduler.latency = 0, fs.latency = 0, hooks = list()), workers = "<NULL>")
#> - tweaked: TRUE
#> - call: plan(future.batchtools::batchtools_custom, cluster.functions = cf)
#> BatchtoolsCustomFutureBackend:
#> Inherits: BatchtoolsMultiprocessFutureBackend, BatchtoolsFutureBackend, MultiprocessFutureBackend, FutureBackend
#> UUID: 5e8f6eb9d8c1368a178c216547ffc460
#> Number of workers: 1
#> Number of free workers: 1
#> Available cores: 8
#> Automatic garbage collection: FALSE
#> Early signaling: FALSE
#> Interrupts are enabled: TRUE
#> Maximum total size of globals: +Inf
#> Maximum total size of value: +Inf
#> Number of active futures: 0
#> Number of futures since start: 0 (0 created, 0 launched, 0 finished)
#> Total runtime of futures: 0 secs (NaN secs/finished future)
#> batchtools configuration file: <NA>
#> batchtools cluster functions: ‘Interactive’
#> batchtools cluster functions template: <NA>
#> Cache directory: ‘/tmp/hb/RtmpudlmVL/future.batchtools/docs/reference/.future/20260407_085537-Y3LsQk’ (0 folders)
#> batchtools resources:
#>  list()

message("Main process ID: ", Sys.getpid())
#> Main process ID: 285143

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
#>     hostname    os cores    pid
#> 1 hb-x1-2023 Linux     8 285301
#>                                                                                                                                                                                                        modules
#> 1 CBI:cmake/4.3.1:r/4.5.3:pandoc/3.9:quarto/1.9.36:imagemagick/7.1.2-18:bat/0.26.1:fzf/0.70.0:glow/2.1.1:github-cli/2.89.0:git-flow/1.12.3:git-extras/7.4.0:shellcheck/0.11.0:bash-startup/0.5.0:restic/0.18.1
```
