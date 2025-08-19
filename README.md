

<div id="badges"><!-- pkgdown markup -->
<a href="https://CRAN.R-project.org/web/checks/check_results_future.batchtools.html"><img border="0" src="https://www.r-pkg.org/badges/version/future.batchtools" alt="CRAN check status"/></a> <a href="https://github.com/futureverse/future.batchtools/actions?query=workflow%3AR-CMD-check"><img border="0" src="https://github.com/futureverse/future.batchtools/actions/workflows/R-CMD-check.yaml/badge.svg?branch=develop" alt="R CMD check status"/></a>  <a href="https://github.com/futureverse/future.batchtools/actions?query=workflow%3Afuture_tests"><img border="0" src="https://github.com/futureverse/future.batchtools/actions/workflows/future_tests.yaml/badge.svg?branch=develop" alt="future.tests checks status"/></a>   <a href="https://app.codecov.io/gh/futureverse/future.batchtools"><img border="0" src="https://codecov.io/gh/futureverse/future.batchtools/branch/develop/graph/badge.svg" alt="Coverage Status"/></a> 
</div>

# future.batchtools: A Future API for Parallel and Distributed Processing using 'batchtools' 

## TL;DR

Here is an example on how evaluate R expression on a Slurm
high-performance compute (HPC) cluster.

```r
library(future)

# Limit runtime to 10 minutes and memory to 400 MiB per future,
# request a parallel environment with four slots on a single host.
# On this system, R is available via environment module 'r'. By
# specifying 'r/4.5.1', 'module load r/4.5.1' will be added to
# the submitted job script.
plan(future.batchtools::batchtools_slurm, resources = list(
  time = "00:10:00", mem = "400M", nodes=1, ntasks=4,
  modules = c("r/4.5.1")
))

# Give it a spin
f <- future({
  data.frame(
    hostname = Sys.info()[["nodename"]],
          os = Sys.info()[["sysname"]],
       cores = unname(parallelly::availableCores()),
     modules = Sys.getenv("LOADEDMODULES")
  )
})
info <- value(f)
print(info)
#>   hostname    os cores  modules
#> 1      n12 Linux     4  r/4.5.1
```

## Introduction

The **[future]** package provides a generic API for using futures in
R.  A future is a simple yet powerful mechanism to evaluate an R
expression and retrieve its value at some point in time.  Futures can
be resolved in many different ways depending on which strategy is
used.  There are various types of synchronous and asynchronous futures
to choose from in the **[future]** package.

This package, **[future.batchtools]**, provides a type of futures that
utilizes the **[batchtools]** package.  This means that _any_ type of
backend that the **batchtools** package supports can be used as a
future.  More specifically, **future.batchtools** will allow you or
users of your package to leverage the compute power of
high-performance computing (HPC) clusters via a simple switch in
settings - without having to change any code at all.

For instance, if **batchtools** is properly configured, the below two
expressions for two futures will be processed on two different compute
nodes:

```r
library(future)
plan(future.batchtools::batchtools_slurm)

f_x <- future({ Sys.sleep(5); 3.14 })
f_y <- future({ Sys.sleep(5); 2.71 })
x <- value(f_x)
y <- value(f_y)
x + y
#> [1] 5.85
```

This is just a toy example to illustrate what futures look like and
how to work with them.

For an introduction as well as full details on how to use futures,
please see <https://www.futureverse.org> or consult the package
vignettes of the **[future]** package.


## Demos

The **[future]** package provides a demo using futures for calculating
a set of Mandelbrot planes.  The demo does not assume anything about
what type of futures are used.  _The user has full control of how
futures are evaluated_.  For instance, to use local batchtools
futures, run the demo as:

```r
library(future)
plan(future.batchtools::batchtools_local)
demo("mandelbrot", package = "future", ask = FALSE)
```


## Available batchtools backend

The **future.batchtools** package implements a generic future wrapper
for all batchtools backends.  Below are the most common types of
batchtools backends. For other types of parallel and distributed
backends, please see <https://www.futureverse.org/backends.html>.


| Backend                  | Description                                                              | Alternative in future package
|:-------------------------|:-------------------------------------------------------------------------|:------------------------------------
| `batchtools_lsf`         | Futures are evaluated via a [Load Sharing Facility (LSF)] job scheduler  | N/A
| `batchtools_openlava`    | Futures are evaluated via an [OpenLava] job scheduler                    | N/A
| `batchtools_sge`         | Futures are evaluated via a [Sun/Oracle Grid Engine (SGE)] job scheduler | N/A
| `batchtools_slurm`       | Futures are evaluated via a [Slurm] job scheduler                        | N/A
| `batchtools_torque`      | Futures are evaluated via a [TORQUE] / PBS job scheduler                 | N/A
| `batchtools_custom`      | Futures are evaluated via a custom batchtools configuration R script or via a set of cluster functions  | N/A
| `batchtools_multicore`   | parallel evaluation by forking the current R process                     | `plan(multicore)`
| `batchtools_local`       | sequential evaluation in a separate R process (on current machine)       | `plan(cluster, workers = I(1))`




[batchtools]: https://cran.r-project.org/package=batchtools
[future]: https://cran.r-project.org/package=future
[future.batchtools]: https://cran.r-project.org/package=future.batchtools
[TORQUE]: https://en.wikipedia.org/wiki/TORQUE
[Slurm]: https://en.wikipedia.org/wiki/Slurm_Workload_Manager
[Sun/Oracle Grid Engine (SGE)]: https://en.wikipedia.org/wiki/Oracle_Grid_Engine
[Load Sharing Facility (LSF)]: https://en.wikipedia.org/wiki/Platform_LSF
[OpenLava]: https://en.wikipedia.org/wiki/OpenLava

## Installation
R package future.batchtools is available on [CRAN](https://cran.r-project.org/package=future.batchtools) and can be installed in R as:
```r
install.packages("future.batchtools")
```


### Pre-release version

To install the pre-release version that is available in Git branch `develop` on GitHub, use:
```r
remotes::install_github("futureverse/future.batchtools", ref="develop")
```
This will install the package from source.  

<!-- pkgdown-drop-below -->


## Contributing

To contribute to this package, please see [CONTRIBUTING.md](CONTRIBUTING.md).

