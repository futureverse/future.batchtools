# Version (development version)

## Significant Changes

 * **future.batchtools** now implements the FutureBackend API
   introduced in **future** 1.40.0 (2025-04-10).

## New Features

 * Batchtools future backends gained argument `delete` for controlling
   if and when batchtools futures should be deleted from the file
   system. Supported values are `"on-success"`, `"never"` and
   `"always"`. The default value can be set via R option
   `future.batchtools.delete`.

 * Most types of batchtools future can now be interrupted, including
   `batchtools_multicore` and all job-scheduler backends, e.g.
   `batchtools_sge` and `batchtools_slurm`.

 * Canceling batchtools futures will now interrupt them by default,
   if the backend supports it.

 * Calling `plan()` on `batchtools_sge` and `batchtools_slurm` reports
   on the version of the scheduler.
   
 * Printing a batchtools future will show logged output.

 * Built-in job-script templates:
 
   - Tidy up built-in template job scripts for Bash, SGE, Slurm and
     Torque/PBS.
  
   - Add built-in template job scripts for LSF and OpenLava.
  
   - All built-in template job scripts support "as-is" resource
     specifications via character vector `resources[["asis"]]`.
  
   - All built-in template job scripts support "environment module"
     resource specifications via character vector
     `resources[["modules"]]`. When specified, corresponding `module
     load <name>` entries are injected to the generated job script.
     
   - All built-in template job scripts support "startup" and
     "shutdown" code resource specifications via character vectors
     `resources[["startup"]]` and `resources[["shutdown"]]`. When
     specified, corresponding lines are injected in the generated job
     script and the beginning and end, respectively.

   - All built-in template job scripts support a "details" resource
     specification via logical scalar `resources[["details"]]`. If
     TRUE, scheduler job details are outputted to the log files at the
     beginning and job summaries at the end.

   - All built-in template job scripts, which are written in Bash,
     error and exit early, by setting more strict Bash options. This
     should help with any troubleshooting, especially when getting
     started.

## Documentation

 * Add `timeout <runtime> ...` to the default `batchtools_bash`
   template script to illustrate how to control this runtime via the
   backend `resources` argument.
   
 * Add explicit **batchtools** arguments `scheduler.latency` and
   `fs.latency` to batchtools backends, where applicable.

## Bug Fixes

 * Calling `value()` on a terminated batchtools job could take a very
   long time before it returned with an error. This was because it
   read logged output to be part of the error message. When there was
   no output file, **batchtools** would wait for the file up to
   `fs.latency` seconds (default 65 seconds) to give job schedulers
   and any global file system time to write output to file. Now
   **future.batchtools** will no longer wait for such files and only
   read their content if they exists when checked.

## Deprecated and Defunct

 * R option `future.delete` is deprecated. Please use new R option
   `future.batchtools.delete` instead. For backward compatible
   reasons, if R option `future.delete` sets option
   `future.batchtools.delete` when the packages is loaded and the
   latter is not already set. If `future.delete` is FALSE, then
   `future.batchtools.delete` is set to `"never"`.  If `future.delete`
   is TRUE, then `future.batchtools.delete` is set to `"on-success"`.
   

# Version 0.12.2 [2025-06-06]

## Bug Fixes

 * Attempts to cancel batchtools futures via `cancel()` would result
   in "Interruption of futures require a backend implementing the
   FutureBackend API". Until this package implements the new
   FutureBackend API of future (>= 1.40.0), any calls to `cancel()`
   will be silently ignored.
  

# Version 0.12.1 [2023-12-19]

## Bug Fixes

 * `plan(future.batchtools::batchtools_multicore)` did not support
   `workers` argument being a function.


# Version 0.12.0 [2023-02-24]

## New Features

 * Improved performance of batchtools futures by avoiding re-checking
   the **batchtools** status if the **batchtools** job has already
   been observed to be resolved.  Checking the **batchtools** status
   is fairly expense, especially since each status check queries a set
   of files on the file system.

 * Improved performance of batchtools futures by making the removal of
   the **batchtools** registry about 10-15 times faster.

## Bug Fixes

 * `run()` for `BatchtoolsFuture` would update the RNG state, if the
   future would attach packages.


# Version 0.11.0 [2022-12-13]

## Significant Changes

 * `R_FUTURE_BATCHTOOLS_*` environment variables are now only read
   when the **future.batchtools** package is loaded, where they set
   the corresponding `future.batchtools*` option.  This is in line
   with how all packages in the Futureverse works.
   
 * Add `nbrOfFreeWorkers()` for batchtools futures.
   
## New Features

 * It is now possible to tweak arguments used by an underlying
   `batchtools::makeClusterFunctionsNnn()` function for some of the
   `batchtools_nnn` backends, e.g.
   `plan(future.batchtools::batchtools_slurm, scheduler.latency = 60)`.
 
 * `plan(future.batchtools::batchtools_multicore, workers = I(1))`
   overrides the fallback to `batchtools_local` and forces a single
   `batchtools_multicore` worker.
 
 * `print()` for BatchtoolsFuture now reports on the batchtools
   configuration file (an R script) and the the batchtools job template
   file (a shell script) with info on location, file size, and number
   of lines, if they exist.

 * `run()` for BatchtoolsFuture now produce an informative
   BatchtoolsFutureError in case `batchtools::submitJobs()` fails, for
   instance, due to invalid job-scheduler resource specifications.
 
 * Add BatchtoolsFuture subclasses; abstract
   BatchtoolsUniprocessFuture, abstract BatchtoolsMultiprocessFuture,
   BatchtoolsInteractiveFuture, BatchtoolsLocalFuture,
   BatchtoolsMulticoreFuture, BatchtoolsLsfFuture,
   BatchtoolsOpenLavaFuture, BatchtoolsSGEFuture,
   BatchtoolsSlurmFuture, BatchtoolsTorqueFuture, and
   BatchtoolsCustomFuture.

 * Add `batchtools_bash` and BatchtoolsBashFuture to illustrate how to
   create a basic `batchtools_custom` future based on a **batchtools**
   template file `bash.tmpl` part of the package.

 * Add `batchtools_ssh` and BatchtoolsSSHFuture for creating
   batchtools futures based on **batchtools** SSH workers created by
   `batchtools::makeClusterFunctionsSSH()`.
   
 * Add example template files for SGE and Slurm.

## Miscellaneous

 * `result()` for HPC batchtools backends would use a timeout of
   `fs.latency` seconds (as set for the cluster functions) when trying
   to collect the logged output.  However, since it has already
   collected the results, the log file should be available already and
   there would be no need to have to wait for the log file to appear.
   Because of this, we temporarily set `fs.latency = 1.0` (second)
   timeout for trying to find the log file.  This makes a big
   difference in case the template used a `--output=<path>` location
   other than `--output=<%= log.file %>`. In such cases, the log file
   would not be found, requiring a full timeout to complete.

## Bug Fixes

 * Using `plan(future.batchtools::batchtools_nnn, finalize = FALSE)`
   would give a warning on `Detected 1 unknown future arguments:
   'finalize'`.

 * Template files in `system.file(package = "future.batchtools",
   "templates")` were not found.

 * `run()`, `resolved()`, and `result()` for `BatchtoolsFuture` would
   update the RNG state.


# Version 0.10.0 [2021-01-02]

## Significant Changes

 * Lazy batchtools futures only creates the internal **batchtools**
   registry when the future is launched.

 * Removed S3 generic functions `await()`, `finished()`, and
   `status()`, which were functions that were used for internal
   purposes.

## Documentation

 * Document option `future.delete` and clarify option
   `future.cache.path` in `help("future.batchtools.options")`.

## Bug Fixes

 * If `run()` was called twice for a BatchtoolsFuture, it would not
   produce a FutureError but only a regular non-classed error.

## Deprecated and Defunct

 * Removed S3 generic functions `await()`, `finished()`, and
   `status()`, which were functions that were used for internal
   purposes.


# Version 0.9.0 [2020-04-14]

## Significant Changes

 * The default number of workers on HPC environments is now 100. To
   revert to the previous default of +Inf, see below news entry.


## New Features

 * It is now possible to configure the default number of workers on
   the job queue of an HPC scheduler via either R option
   `future.batchtools.workers` or environment variable
   `R_FUTURE_BATCHTOOLS_WORKERS`.

 * It is now possible to configure the **batchtools** registries that
   are used by batchtools futures via new argument `registry` to
   `plan()`.  This argument should be a named list of parameters
   recognized by the **batchtools** package,
   e.g. `plan(future.batchtools::batchtools_sge, registry =
   list(...))`.  For notable example, see below news entries.

 * The default working directory for batchtools futures is the current
   working directory of R _when_ the batchtools future is created.
   This corresponds to specifying
   `plan(future.batchtools::batchtools_nnn, registry = list(work.dir =
   NULL)`.  Sometimes it is useful to use a explicit working directory
   that is guaranteed to be available on all workers on a shared file
   system, e.g. `plan(future.batchtools::batchtools_nnn, registry =
   list(work.dir = "~"))`.

 * It is possible to control if and how **batchtools** should use file
   compression for exported globals and results by specifying
   **batchtools** registry parameter `compress`.  For example, to turn
   off file compression, use `plan(future.batchtools::batchtools_nnn,
   registry = list(compress = FALSE))`.

 * The default location of the `.future` folder can be controlled by R
   option `future.cache.path` or environment variable
   `R_FUTURE_CACHE_PATH`.

 * `batchtools_custom()` and BatchtoolsFuture gained argument
   `conf.file`. Using `plan(future.batchtools::batchtools_custom)`
   will now use any **batchtools** configuration file (an R script)
   found on the `batchtools::findConfFile()` search path.


## Documentation

 * Add `help("future.batchtools.options")` which descriptions R
   options and environment variables used specifically by the
   **future.batchtools** package.


# Version 0.8.1 [2019-09-30]

## Bug Fixes

 * `print()` for BatchtoolsFuture would produce an error if the
   underlying **batchtools** Registry was incomplete.


# Version 0.8.0 [2019-05-04]

## New Features

 * Setting option `future.delete` to FALSE will now prevent removal of
   the **batchtools** registry folders.

 * When a **batchtools** job expires, for instance when the scheduler
   terminates it because the job was running out of its allocated
   resources, then a BatchtoolsFutureError is produced which by
   default outputs the tail of the output logged by **batchtools**.  The
   default number of lines displayed from the end is now increased
   from six to 48 - a number which now can be set via option
   `future.batchtools.expiration.tail`.

 * Now a more informative error message is produced if a **batchtools**
   `*.tmpl` template file was not found.

 * Debug messages are now prepended with a timestamp.


## Bug Fixes

 * Argument `workers` could not be a function.

 * Argument `workers` of type character was silently accepted and
   effectively interpreted as `workers = length(workers)`.


# Version 0.7.2 [2018-12-03]

## Documentation

 * Add a simple `example(future_custom)`.


## Fixes

 * Made internal code agile to upcoming changes in the **future** package
   on how a captured error is represented.


## Software Quality

 * FYI: Every release is tested against one Torque/PBS and one SGE
   scheduler.


## Bug Fixes

 * `resolve()` on a lazy batchtools future would stall and never
   return.


# Version 0.7.1 [2018-07-18]

## New Features

 * The `batchtools_*` backends support the handling of the standard
   output as implemented in **future** (>= 1.9.0).


## Bug Fixes

 * A bug was introduced in **future.batchtools** 0.7.0 that could result
   in `Error in readLog(id, reg = reg) : Log file for job with id 1
   not available"` when using one of the batchtools backends.  It
   occurred when the value was queried.  It was observed using
   `batchtools_torque` but not when using `batchtools_local`.  This
   bug was missed because the 0.7.0 release was not tested on an
   TORQUE/PBS HPC scheduler as it should have.


# Version 0.7.0 [2018-05-03]

## New Features

 * Argument `workers` of future strategies may now also be a function,
   which is called without argument when the future strategy is set up
   and used as-is.  For instance, `plan(future.callr::callr, workers =
   halfCores)` where `halfCores <- function() { max(1,
   round(availableCores() / 2)) }` will use half of the number of
   available cores.  This is useful when using nested future
   strategies with remote machines.


## Code Refactoring

 * Preparing for futures to gather a richer set of results from
   batchtools backends.


# Version 0.6.0 [2017-09-10]

## New Features

 * If the built-in attempts of **batchtools** for finding a default
   template file fails, then `system("templates", package =
   "future.batchtools")` is searched for template files as well.
   Currently, there exists a `torque.tmpl` file.

 * A job's name in the scheduler is now set as the future's label
   (requires **batchtools** 0.9.4 or newer).  If no label is specified,
   the default job name is controlled by **batchtools**.

 * The period between each poll of the scheduler to check whether a
   future (job) is finished or not now increases geometrically as a
   function of number of polls.  This lowers the load on the scheduler
   for long running jobs.

 * The error message for expired batchtools futures now include the
   last few lines of the logged output, which sometimes includes clues
   on why the future expired.  For instance, if a TORQUE/PBS job use
   more than the allocated amount of memory it might be terminated by
   the scheduler leaving the message `PBS: job killed: vmem 1234000
   exceeded limit 1048576` in the output.

 * `print()` for BatchtoolsFuture returns the object invisibly.

## Bug Fixes

 * Calling `future_lapply()` with functions containing globals part of
   non-default packages would when using batchtools futures give an
   error complaining that the global is missing. This was due to
   updates in **future** (>= 1.4.0) that broke this package.

 * `loggedOutput()` for BatchtoolsFuture would always return NULL
   unless an error had occurred.


# Version 0.5.0 [2017-06-02]


# Version 0.4.0 [2017-05-16]

## New Features

 * Added `batchtools_custom()` for specifying batchtools futures using
   any type of batchtools cluster functions.

 * `batchtools_template(pathname = NULL, type = <type>)` now relies on
   the **batchtools** package for locating the `<type>` template file.

 * `nbrOfWorkers()` for batchtools futures now defaults to +Inf unless
   the evaluator's `workers` or `cluster.functions` specify something
   else.

 * Renamed argument `pathname` to `template` for `batchtools_<tmpl>()`
   functions.


## Bug Fixes

 * Under `plan(future.batchtools::batchtools_nnn)`, when being created
   futures would produce an error on `all(is.finite(workers)) is not
   TRUE` due to an outdated sanity check.


## Software Quality

 * TESTS: Added test of `future_lapply()` for batchtools backends.

 * TESTS: Added optional tests for `batchtools_*` HPC schedulers
   listed in environment variable `R_FUTURE_TESTS_STRATEGIES`.


## Code Refactoring

 * CLEANUP: Package no longer depends on **R.utils**.


# Version 0.3.0 [2017-03-19]

## New Features

 * The number of jobs one can add to the queues of HPC schedulers is
   in principle unlimited, which is why the number of available
   workers for such `batchtools_*` backends is reported as +Inf.
   However, as the number of workers is used by `future_lapply()` to
   decide how many futures should be used to best partition the
   elements, this means that `future_lapply()` will always use one
   future per element.  Because of this, it is now possible to specify
   `plan(future.batchtools::batchtools_nnn, workers = n)` where `n` is
   the target number of workers.


# Version 0.2.0 [2017-02-23]

## Globals

 * **batchtools** (>= 0.9.2) now supports exporting objects with any type
   of names (previously only possible if they mapped to strictly valid
   filenames). This allowed me to avoid lots of internal workaround
   code encoding and decoding globals.


# Version 0.1.0 [2017-02-11]
