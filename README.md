

<div id="badges"><!-- pkgdown markup -->
<a href="https://CRAN.R-project.org/web/checks/check_results_future.batchtools.html"><img border="0" src="https://www.r-pkg.org/badges/version/future.batchtools" alt="CRAN check status"/></a> <a href="https://github.com/futureverse/future.batchtools/actions?query=workflow%3AR-CMD-check"><img border="0" src="https://github.com/futureverse/future.batchtools/actions/workflows/R-CMD-check.yaml/badge.svg?branch=develop" alt="R CMD check status"/></a>  <a href="https://github.com/futureverse/future.batchtools/actions?query=workflow%3Afuture_tests"><img border="0" src="https://github.com/futureverse/future.batchtools/actions/workflows/future_tests.yaml/badge.svg?branch=develop" alt="future.tests checks status"/></a>   <a href="https://app.codecov.io/gh/futureverse/future.batchtools"><img border="0" src="https://codecov.io/gh/futureverse/future.batchtools/branch/develop/graph/badge.svg" alt="Coverage Status"/></a> 
</div>

# future.batchtools: A Future API for Parallel and Distributed Processing using 'batchtools' 

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

For instance, if **batchtools** is properly configures, the below two
expressions for futures `x` and `y` will be processed on two different
compute nodes:

```r
library(future)
plan(future.batchtools::batchtools_slurm)

x %<-% { Sys.sleep(5); 3.14 }
y %<-% { Sys.sleep(5); 2.71 }
x + y
#> [1] 5.85
```

This is just a toy example to illustrate what futures look like and
how to work with them.

A more realistic example comes from the field of cancer research
where very large data FASTQ files, which hold a large number of short
DNA sequence reads, are produced.  The first step toward a biological
interpretation of these data is to align the reads in each sample
(one FASTQ file) toward the human genome.  In order to speed this up,
we can have each file be processed by a separate compute node and each
node we can use 24 parallel processes such that each process aligns a
separate chromosome.  Here is an outline of how this nested parallelism
could be implemented using futures.

```r
library(future)
library(listenv)

## The first level of futures should be submitted to the
## cluster using batchtools.  The second level of futures
## should be using multisession, where the number of
## parallel processes is automatically decided based on
## what the cluster grants to each compute node.
plan(list(future.batchtools::batchtools_slurm, multisession))

## Find all samples (one FASTQ file per sample)
fqs <- dir(pattern = "[.]fastq$")

## The aligned results are stored in BAM files
bams <- listenv()

## For all samples (FASTQ files) ...
for (ss in seq_along(fqs)) {
  fq <- fqs[ss]

  ## ... use futures to align them ...
  bams[[ss]] %<-% {
    bams_ss <- listenv()
	## ... and for each FASTQ file use a second layer
	## of futures to align the individual chromosomes
    for (cc in 1:24) {
      bams_ss[[cc]] %<-% htseq::align(fq, chr = cc)
    }
	## Resolve the "chromosome" futures and return as a list
    as.list(bams_ss)
  }
}
## Resolve the "sample" futures and return as a list
bams <- as.list(bams)
```

Note that a user who do not have access to a cluster could use the
same script processing samples sequentially and chromosomes in
parallel on a single machine using:

```r
plan(list(sequential, multisession))
```

or samples in parallel and chromosomes sequentially using:

```r
plan(list(multisession, sequential))
```

For an introduction as well as full details on how to use futures,
please consult the package vignettes of the **[future]** package.



## Choosing batchtools backend

The **future.batchtools** package implements a generic future wrapper
for all batchtools backends.  Below are the most common types of
batchtools backends.


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


### Examples

Below is an examples illustrating how to use `batchtools_slurm` to
configure the batchtools backend.  For further details and examples on
how to configure batchtools, see the [batchtools configuration] wiki
page.

To configure **batchtools** for job schedulers we need to setup a
`*.tmpl` template file that is used to generate the script used by the
scheduler.  This is what a template file for Slurm may look like:

```sh
#!/bin/bash

<%
defaults <- list(
  nodes = 1,         # single-host processing
  time = "00:05:00", # 5-min runtime
  mem  = "100M"      # 100 MiB memory
)
resources <- c(resources, defaults[setdiff(names(defaults), names(resources))])
opts <- unlist(resources, use.names = TRUE)
opts <- sprintf("--%s=%s", names(opts), opts)
opts <- paste(opts, collapse = " ") %>
%>

#SBATCH --job-name=<%= job.name %>
#SBATCH --output=<%= log.file %>
#SBATCH <%= opts %>

Rscript -e 'batchtools::doJobCollection("<%= uri %>")'
```

If this template is saved to file `batchtools.slurm.tmpl` (without
period) in the working directory or as `.batchtools.slurm.tmpl` (with
period) the user's home directory, then it will be automatically
located by the **batchtools** framework and loaded when doing:

```r
plan(future.batchtools::batchtools_slurm)
```

It is also possible to specify the template file explicitly, e.g.

```r
plan(future.batchtools::batchtools_slurm, template = "/path/to/batchtools.slurm.tmpl")
```


Resource parameters can be specified via argument `resources` which
should be a named list and is passed as is to the template file.  For
example, to request that each job would get alloted 12 cores (one a
single machine) and up to 5 GiB of memory, use:

```r
plan(future.batchtools::batchtools_slurm, resources = list(ntasks = 12, mem = "5G"))
```

To specify the `resources` argument at the same time as using nested
future strategies, one can use `tweak()` to tweak the default
arguments.  For instance,

```r
plan(list(
  tweak(future.batchtools::batchtools_slurm, resources = list(ntasks = 12, mem = "5G")),
  multisession
))
```

causes the first level of futures to be submitted via the Slurm job
scheduler requesting 12 cores and 5 GiB of memory per job.  The second
level of futures will be evaluated using multisession using the 12
cores given to each job by the scheduler.

For further details and examples on how to configure **batchtools**
per se, see the [batchtools configuration] wiki page and the help
pages for `batchtools_slurm`, `batchtools_sge`, etc.


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


[batchtools]: https://cran.r-project.org/package=batchtools
[brew]: https://cran.r-project.org/package=brew
[future]: https://cran.r-project.org/package=future
[future.batchtools]: https://cran.r-project.org/package=future.batchtools
[batchtools configuration]: https://batchtools.mlr-org.com/articles/batchtools.html
[TORQUE]: https://en.wikipedia.org/wiki/TORQUE
[Slurm]: https://en.wikipedia.org/wiki/Slurm_Workload_Manager
[Sun/Oracle Grid Engine (SGE)]: https://en.wikipedia.org/wiki/Oracle_Grid_Engine
[Load Sharing Facility (LSF)]: https://en.wikipedia.org/wiki/Platform_LSF
[OpenLava]: https://en.wikipedia.org/wiki/OpenLava
[Docker Swarm]: https://docs.docker.com/swarm/

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

