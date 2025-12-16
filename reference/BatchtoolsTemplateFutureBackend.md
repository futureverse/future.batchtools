# Batchtools futures for LSF, OpenLava, SGE, Slurm, TORQUE etc.

Batchtools futures for LSF, OpenLava, SGE, Slurm, TORQUE etc. are
asynchronous multiprocess futures that will be evaluated on a compute
cluster via a job scheduler.

## Usage

``` r
BatchtoolsTemplateFutureBackend(
  type,
  scheduler.latency = 1,
  fs.latency = 65,
  resources = list(),
  delete = getOption("future.batchtools.delete", "on-success"),
  template = NULL,
  makeClusterFunctions = NULL,
  workers = getOption("future.batchtools.workers", default = 100L),
  ...
)
```

## Arguments

- type:

  (character string) Type of job scheduler.

- scheduler.latency:

  \[`numeric(1)`\]  
  Time to sleep after important interactions with the scheduler to
  ensure a sane state. Currently only triggered after calling
  [`submitJobs`](https://batchtools.mlr-org.com/reference/submitJobs.html).

- fs.latency:

  \[`numeric(1)`\]  
  Expected maximum latency of the file system, in seconds. Set to a
  positive number for network file systems like NFS which enables more
  robust (but also more expensive) mechanisms to access files and
  directories. Usually safe to set to `0` to disable the heuristic, e.g.
  if you are working on a local file system.

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

- delete:

  Controls if and when the batchtools job registry folder is deleted. If
  `"on-success"` (default), it is deleted if the future was resolved
  successfully *and* the expression did not produce an error. If
  `"never"`, then it is never deleted. If `"always"`, then it is always
  deleted.

- template:

  (optional) A batchtools template file or a template string (in brew
  format). If not specified, it is left to the batchtools package to
  locate such file using its search rules.

- workers:

  The maximum number of workers the batchtools backend may use at any
  time, which for HPC schedulers corresponds to the maximum number of
  queued jobs. The default is
  `getOption("`[`future.batchtools.workers`](https://future.batchtools.futureverse.org/reference/zzz-future.batchtools.options.md)`", 100)`.

- ...:

  Additional arguments passed to
  [`BatchtoolsFutureBackend()`](https://future.batchtools.futureverse.org/reference/BatchtoolsFutureBackend.md).

## Value

An object of class `BatchtoolsFutureBackend`.

## Details

These type of batchtools futures rely on batchtools backends set up
using the following batchtools functions:

- [`batchtools::makeClusterFunctionsLSF()`](https://batchtools.mlr-org.com/reference/makeClusterFunctionsLSF.html)
  for [Load Sharing Facility
  (LSF)](https://en.wikipedia.org/wiki/Platform_LSF)

- [`batchtools::makeClusterFunctionsOpenLava()`](https://batchtools.mlr-org.com/reference/makeClusterFunctionsOpenLava.html)
  for [OpenLava](https://en.wikipedia.org/wiki/OpenLava)

- [`batchtools::makeClusterFunctionsSGE()`](https://batchtools.mlr-org.com/reference/makeClusterFunctionsSGE.html)
  for [Sun/Oracle Grid Engine
  (SGE)](https://en.wikipedia.org/wiki/Oracle_Grid_Engine)

- [`batchtools::makeClusterFunctionsSlurm()`](https://batchtools.mlr-org.com/reference/makeClusterFunctionsSlurm.html)
  for [Slurm](https://en.wikipedia.org/wiki/Slurm_Workload_Manager)

- [`batchtools::makeClusterFunctionsTORQUE()`](https://batchtools.mlr-org.com/reference/makeClusterFunctionsTORQUE.html)
  for [TORQUE](https://en.wikipedia.org/wiki/TORQUE) / PBS
