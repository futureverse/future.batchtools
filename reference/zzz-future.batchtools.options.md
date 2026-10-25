# Options used for batchtools futures

Below are the R options and environment variables that are used by the
future.batchtools package. See
[future::future.options](https://future.futureverse.org/reference/zzz-future.options.html)
for additional ones that apply to futures in general.\
\
*WARNING: Note that the names and the default values of these options
may change in future versions of the package. Please use with care until
further notice.*

## Settings for batchtools futures

- future.batchtools.workers::

  (a positive numeric or `+Inf`) The default number of workers available
  on HPC schedulers with job queues. (Default: `100`)

- future.batchtools.output::

  (logical) If TRUE, batchtools will produce extra output. If FALSE,
  such output will be disabled by setting batchtools options
  batchtools.verbose and batchtools.progress to FALSE. (Default:
  `getOption("future.debug", FALSE)`)

- future.batchtools.expiration.tail::

  (a positive numeric) When a batchtools job expires, the last few lines
  will be relayed by batchtools futures to help troubleshooting. This
  option controls how many lines are displayed. (Default: `48L`)

- future.cache.path::

  (character string) An absolute or relative path specifying the root
  folder in which batchtools registry folders are stored. This folder
  needs to be accessible from all hosts ("workers"). Specifically, it
  must *not* be a folder that is only local to the machine such as
  `file.path(tempdir(), ".future"` if a job scheduler on an HPC
  environment is used. (Default: `.future` in the current working
  directory)

- future.batchtools.relay.immediate::

  (logical) If TRUE, conditions of class `immediateCondition` are
  relayed as soon as possible. To do this, the worker saves each of them
  as a small file in the batchtools registry folder of the future, and
  the main R process reads these files when it polls the future, e.g.
  using
  [`future::resolved()`](https://future.futureverse.org/reference/resolved.html).
  If FALSE, no such files are written, and immediate conditions are
  treated as any other condition, that is, they are relayed at the end,
  when the value of the future is collected. Disabling this can be
  useful if a future signals so many immediate conditions that the file
  system gets overloaded. (Default: `TRUE`)

- future.batchtools.script.validate::

  (logical) If TRUE, each generated job script that has a Bash or
  POSIX-shell shebang is checked for syntax errors using `bash -n`
  before it is submitted, which gives an immediate error instead of a
  failed job on the compute node. Set to FALSE to disable, e.g. if a
  false positive is reported. (Default: `TRUE`)

- future.batchtools.delete::

  (character string) Controls whether or not the future's batchtools
  registry folder is deleted after the future result has been collected.
  If `"always"`, it is always deleted. If `"never"`, it is never
  deleted. If `"on-success"`, it is deleted if the future resolved
  successfully, whereas if it failed, it is left as-is to help with
  troubleshooting. (Default: `"on-success"`)

## Environment variables that set R options

All of the above R future.batchtools.\* options can be set by
corresponding environment variable `R_FUTURE_BATCHTOOLS_*` *when the
future.batchtools package is loaded*. This means that those environment
variables must be set before the future.batchtools package is loaded in
order to have an effect. For example, if
`R_FUTURE_BATCHTOOLS_WORKERS="200"` is set, then option
future.batchtools.workers is set to `200` (numeric).

## Examples

``` r
# Set an R option:
options(future.cache.path = "/cluster-wide/folder/.future")
```
