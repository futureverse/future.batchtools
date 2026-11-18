# future.batchtools: Parallel Future Backend for HPC Schedulers using ‘batchtools’

## Introduction

The **[future.batchtools](https://future.batchtools.futureverse.org)**
package is a backend for the
**[future](https://future.futureverse.org)** framework, also known as
the [Futureverse](https://www.futureverse.org). The Futureverse makes it
easy to parallelize existing R code - often with only a minor change of
code. Most users interact with it via higher-level packages such as
**[futurize](https://futurize.futureverse.org)**,
**[future.apply](https://future.apply.futureverse.org)**,
**[furrr](https://furrr.futureverse.org)**, and
**[doFuture](https://doFuture.futureverse.org)**, and you choose *where*
and *how* the code runs by setting a future backend with
[`plan()`](https://future.futureverse.org/reference/plan.html). This
package adds backends that run futures as jobs on a high-performance
compute (HPC) cluster, via job schedulers such as Slurm, SGE, LSF, and
TORQUE/PBS, using the
**[batchtools](https://cran.r-project.org/package=batchtools)** package.

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`futurize`](https://futurize.futureverse.org)`)`\
[`plan`](https://future.futureverse.org/reference/plan.html)`(``future.batchtools``::`[`batchtools_slurm`](https://future.batchtools.futureverse.org/reference/batchtools_slurm.md)`)``  ``# parallelize via Slurm jobs`\
\
`# A slow function`\
`slow_fcn`` ``<-`` ``function``(``x``)`` ``{`\
`  `[`Sys.sleep`](https://rdrr.io/r/base/Sys.sleep.html)`(``1.0``)`\
`  ``x``^``2`\
`}`\
\
`# Example data`\
`xs`` ``<-`` ``1``:``10`\
\
`# Sequential and parallel base R apply`\
`ys`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``xs``, ``slow_fcn``)`\
`ys`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``xs``, ``slow_fcn``)`` ``|>`` `[`futurize`](https://futurize.futureverse.org/reference/futurize.html)`(``)`\
\
`# Sequential and parallel purrr map`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`purrr`](https://purrr.tidyverse.org/)`)`\
`ys`` ``<-`` ``xs`` ``|>``  `[`map`](https://purrr.tidyverse.org/reference/map.html)`(``slow_fcn``)`\
`ys`` ``<-`` ``xs`` ``|>``  `[`map`](https://purrr.tidyverse.org/reference/map.html)`(``slow_fcn``)`` ``|>`` `[`futurize`](https://futurize.futureverse.org/reference/futurize.html)`(``)`\
\
`# Sequential and parallel foreach calls`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`foreach`](https://github.com/RevolutionAnalytics/foreach)`)`\
`ys`` ``<-`` `[`foreach`](https://rdrr.io/pkg/foreach/man/foreach.html)`(``x ``=`` ``xs``)`` `[`%do%`](https://rdrr.io/pkg/foreach/man/foreach.html)` ``slow_fcn``(``x``)`\
`ys`` ``<-`` `[`foreach`](https://rdrr.io/pkg/foreach/man/foreach.html)`(``x ``=`` ``xs``)`` `[`%do%`](https://rdrr.io/pkg/foreach/man/foreach.html)` ``slow_fcn``(``x``)`` ``|>`` `[`futurize`](https://futurize.futureverse.org/reference/futurize.html)`(``)`

The only thing that differs from using [any other future
backend](https://www.futureverse.org/backends.html) is the
[`plan()`](https://future.futureverse.org/reference/plan.html) call. The
code does not have to change, it gives identical results, and it behaves
exactly the same.

## The batchtools backends

| Backend | Description | Alternative in future package |
|:---|:---|:---|
| `batchtools_lsf` | Futures are evaluated via a [Load Sharing Facility (LSF)](https://en.wikipedia.org/wiki/Platform_LSF) job scheduler | N/A |
| `batchtools_openlava` | Futures are evaluated via an [OpenLava](https://en.wikipedia.org/wiki/OpenLava) job scheduler | N/A |
| `batchtools_sge` | Futures are evaluated via a [Sun/Son of/Oracle/Univa/Altair Grid Engine (SGE)](https://en.wikipedia.org/wiki/Oracle_Grid_Engine) job scheduler | N/A |
| `batchtools_slurm` | Futures are evaluated via a [Slurm](https://en.wikipedia.org/wiki/Slurm_Workload_Manager) job scheduler | N/A |
| `batchtools_torque` | Futures are evaluated via a [TORQUE](https://en.wikipedia.org/wiki/TORQUE) / PBS job scheduler | N/A |
| `batchtools_custom` | Futures are evaluated via a custom batchtools configuration R script or via a set of cluster functions | N/A |
| `batchtools_multicore` | parallel evaluation by forking the current R process | `plan(multicore)` |
| `batchtools_local` | sequential evaluation in a separate R process (on current machine) | `plan(cluster, workers = I(1))` |

Each future is submitted as a job to the job scheduler. You can request
the resources each job needs via argument `resources`, e.g.

\
`# Limit runtime to 10 minutes and memory to 400 MiB per future, and`\
`# load environment module 'r/4.5' in the job script`\
[`plan`](https://future.futureverse.org/reference/plan.html)`(``future.batchtools``::`[`batchtools_slurm`](https://future.batchtools.futureverse.org/reference/batchtools_slurm.md)`, resources ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`  time ``=`` ``"00:10:00"``, mem ``=`` ``"400M"``, modules ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"r/4.5"``)`\
`)``)`

### Advantages

- You can harness the compute power of an HPC cluster from within R,
  without having to write job scripts or change any code.

- Each future can request its own resources, e.g. runtime, memory, CPU
  cores, and environment modules.

- All of the common job schedulers are supported, and others can be
  supported via a custom **batchtools** configuration.

### Limitations

- Each future is a separate job, which means it has to wait in the job
  queue before it starts. This makes the batchtools backends best suited
  for futures that take minutes or longer to run, rather than many small
  futures.

- The batchtools registry folder, which is `.future/` in the current
  working directory by default, must be on a file system that is shared
  between the machine submitting the jobs and the compute nodes.

## Installation

R package future.batchtools is available on
[CRAN](https://cran.r-project.org/package=future.batchtools) and can be
installed in R as:

\
[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(``"future.batchtools"``)`

### Pre-release version

To install the pre-release version, use:

\
[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(``"future.batchtools"``, repos ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"https://futureverse-devel.r-universe.dev"``, ``"https://cloud.r-project.org"``)``)`
