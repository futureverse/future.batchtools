

<div id="badges"><!-- pkgdown markup -->
<a href="https://CRAN.R-project.org/web/checks/check_results_future.batchtools.html"><img border="0" src="https://www.r-pkg.org/badges/version/future.batchtools" alt="CRAN check status"/></a> <a href="https://github.com/futureverse/future.batchtools/actions?query=workflow%3AR-CMD-check"><img border="0" src="https://github.com/futureverse/future.batchtools/actions/workflows/R-CMD-check.yaml/badge.svg?branch=develop" alt="R CMD check status"/></a>  <a href="https://github.com/futureverse/future.batchtools/actions?query=workflow%3Afuture_tests"><img border="0" src="https://github.com/futureverse/future.batchtools/actions/workflows/future_tests.yaml/badge.svg?branch=develop" alt="future.tests checks status"/></a>   <a href="https://app.codecov.io/gh/futureverse/future.batchtools"><img border="0" src="https://codecov.io/gh/futureverse/future.batchtools/branch/develop/graph/badge.svg" alt="Coverage Status"/></a> 
</div>

# future.batchtools: Parallel Future Backend for HPC Schedulers using 'batchtools' 

## Introduction

The **[future.batchtools]** package is a backend for the **[future]**
framework, also known as the [Futureverse]. The Futureverse makes it
easy to parallelize existing R code - often with only a minor change
of code. Most users interact with it via higher-level packages such
as **[futurize]**, **[future.apply]**, **[furrr]**, and
**[doFuture]**, and you choose _where_ and _how_ the code runs by
setting a future backend with `plan()`. This package adds backends
that run futures as jobs on a high-performance compute (HPC) cluster,
via job schedulers such as Slurm, SGE, LSF, and TORQUE/PBS, using the
**[batchtools]** package.

```r
library(futurize)
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
ys <- xs |>  map(slow_fcn)
ys <- xs |>  map(slow_fcn) |> futurize()

# Sequential and parallel foreach calls
library(foreach)
ys <- foreach(x = xs) %do% slow_fcn(x)
ys <- foreach(x = xs) %do% slow_fcn(x) |> futurize()
```

The only thing that differs from using [any other future backend](https://www.futureverse.org/backends.html) is
the `plan()` call. The code does not have to change, it gives
identical results, and it behaves exactly the same.


## The batchtools backends

| Backend                  | Description                                                              | Alternative in future package
|:-------------------------|:-------------------------------------------------------------------------|:------------------------------------
| `batchtools_lsf`         | Futures are evaluated via a [Load Sharing Facility (LSF)] job scheduler  | N/A
| `batchtools_openlava`    | Futures are evaluated via an [OpenLava] job scheduler                    | N/A
| `batchtools_sge`         | Futures are evaluated via a [Sun/Son of/Oracle/Univa/Altair Grid Engine (SGE)] job scheduler | N/A
| `batchtools_slurm`       | Futures are evaluated via a [Slurm] job scheduler                        | N/A
| `batchtools_torque`      | Futures are evaluated via a [TORQUE] / PBS job scheduler                 | N/A
| `batchtools_custom`      | Futures are evaluated via a custom batchtools configuration R script or via a set of cluster functions  | N/A
| `batchtools_multicore`   | parallel evaluation by forking the current R process                     | `plan(multicore)`
| `batchtools_local`       | sequential evaluation in a separate R process (on current machine)       | `plan(cluster, workers = I(1))`

Each future is submitted as a job to the job scheduler. You can
request the resources each job needs via argument `resources`, e.g.

```r
# Limit runtime to 10 minutes and memory to 400 MiB per future, and
# load environment module 'r/4.5' in the job script
plan(future.batchtools::batchtools_slurm, resources = list(
  time = "00:10:00", mem = "400M", modules = c("r/4.5")
))
```


### Advantages

* You can harness the compute power of an HPC cluster from within R,
  without having to write job scripts or change any code.

* You can specify the resources that the jobs need, e.g. runtime,
  memory, CPU cores, and environment modules, via `plan()`.

* All of the common job schedulers are supported, and others can be
  supported via a custom **batchtools** configuration.


### Limitations

* Each future is a separate job, which means it has to wait in the
  job queue before it starts. This makes the batchtools backends best
  suited for futures that take minutes or longer to run, rather than
  many small futures.

* The batchtools registry folder, which is `.future/` in the current
  working directory by default, must be on a file system that is
  shared between the machine submitting the jobs and the compute
  nodes.


[Futureverse]: https://www.futureverse.org
[future]: https://future.futureverse.org
[future.batchtools]: https://future.batchtools.futureverse.org
[future.apply]: https://future.apply.futureverse.org
[furrr]: https://furrr.futureverse.org
[doFuture]: https://doFuture.futureverse.org
[futurize]: https://futurize.futureverse.org
[batchtools]: https://cran.r-project.org/package=batchtools
[TORQUE]: https://en.wikipedia.org/wiki/TORQUE
[Slurm]: https://en.wikipedia.org/wiki/Slurm_Workload_Manager
[Sun/Son of/Oracle/Univa/Altair Grid Engine (SGE)]: https://en.wikipedia.org/wiki/Oracle_Grid_Engine
[Load Sharing Facility (LSF)]: https://en.wikipedia.org/wiki/Platform_LSF
[OpenLava]: https://en.wikipedia.org/wiki/OpenLava

## Installation

R package future.batchtools is available on [CRAN](https://cran.r-project.org/package=future.batchtools) and can be installed in R as:

```r
install.packages("future.batchtools")
```


### Pre-release version

To install the pre-release version, use:

```r
install.packages("future.batchtools", repos = c("https://futureverse-devel.r-universe.dev", "https://cloud.r-project.org"))
```


<!-- pkgdown-drop-below -->

