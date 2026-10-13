# A Future for batchtools

## TL;DR

Here is an example of how to evaluate R expressions on a Slurm
high-performance computing (HPC) cluster from within R.

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`future`](https://future.futureverse.org)`)`\
\
`# Limit runtime to 10 minutes and memory to 400 MiB per future,`\
`# request a parallel environment with four slots on a single host.`\
`# On this system, R is available via environment module 'r'. By`\
`# specifying 'r/4.5', 'module load r/4.5' will be added to`\
`# the submitted job script.`\
[`plan`](https://future.futureverse.org/reference/plan.html)`(``future.batchtools``::`[`batchtools_slurm`](https://future.batchtools.futureverse.org/reference/batchtools_slurm.md)`, resources ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`  time ``=`` ``"00:10:00"``, mem ``=`` ``"400M"``, `\
`  nodes ``=`` ``1``, ntasks ``=`` ``4``, ``"cpus-per-task"`` ``=`` ``1``,`\
`  modules ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"r/4.5"``)`\
`)``)`\
\
`# Give it a spin`\
`f`` ``<-`` `[`future`](https://future.futureverse.org/reference/future.html)`(``{`\
`  `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`    hostname ``=`` `[`Sys.info`](https://rdrr.io/r/base/Sys.info.html)`(``)``[[``"nodename"``]``]``,`\
`          os ``=`` `[`Sys.info`](https://rdrr.io/r/base/Sys.info.html)`(``)``[[``"sysname"``]``]``,`\
`       cores ``=`` `[`unname`](https://rdrr.io/r/base/unname.html)`(``parallelly``::`[`availableCores`](https://parallelly.futureverse.org/reference/availableCores.html)`(``)``)``,`\
`     modules ``=`` `[`Sys.getenv`](https://rdrr.io/r/base/Sys.getenv.html)`(``"LOADEDMODULES"``)`\
`  ``)`\
`}``)`\
`info`` ``<-`` `[`value`](https://future.futureverse.org/reference/value.html)`(``f``)`\
[`print`](https://rdrr.io/r/base/print.html)`(``info``)`\
`#>   hostname    os cores  modules`\
`#> 1      n12 Linux     4  r/4.5.2`

## Introduction

The **[future](https://cran.r-project.org/package=future)** package
provides a generic API for using futures in R. A future is a simple yet
powerful mechanism to evaluate an R expression and retrieve its value at
some point in time. Futures can be resolved in many different ways
depending on which strategy is used. There are various types of
synchronous and asynchronous futures to choose from in the
**[future](https://cran.r-project.org/package=future)** package.

This package,
**[future.batchtools](https://cran.r-project.org/package=future.batchtools)**,
provides a type of futures that utilizes the
**[batchtools](https://cran.r-project.org/package=batchtools)** package.
This means that *any* type of backend that the **batchtools** package
supports can be used as a future. More specifically,
**future.batchtools** will allow you or users of your package to
leverage the compute power of high-performance computing (HPC) clusters
via a simple switch in settings - without having to change any code at
all.

For instance, the following two future R expressions will be processed
by parallel R workers launched on different compute nodes by the
specified job scheduler:

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`future`](https://future.futureverse.org)`)`\
[`plan`](https://future.futureverse.org/reference/plan.html)`(``future.batchtools``::`[`batchtools_slurm`](https://future.batchtools.futureverse.org/reference/batchtools_slurm.md)`)`\
\
`f_x`` ``<-`` `[`future`](https://future.futureverse.org/reference/future.html)`(``{`` `[`Sys.sleep`](https://rdrr.io/r/base/Sys.sleep.html)`(``5``)``; ``3.14`` ``}``)`\
`f_y`` ``<-`` `[`future`](https://future.futureverse.org/reference/future.html)`(``{`` `[`Sys.sleep`](https://rdrr.io/r/base/Sys.sleep.html)`(``5``)``; ``2.71`` ``}``)`\
`x`` ``<-`` `[`value`](https://future.futureverse.org/reference/value.html)`(``f_x``)`\
`y`` ``<-`` `[`value`](https://future.futureverse.org/reference/value.html)`(``f_y``)`\
`x`` ``+`` ``y`\
`#> [1] 5.85`

This is just a toy example to illustrate what futures look like and how
to work with them.

For an introduction as well as full details on how to use futures,
please see <https://www.futureverse.org> or consult the package
vignettes of the **[future](https://cran.r-project.org/package=future)**
package.

## Demos

The **[future](https://cran.r-project.org/package=future)** package
provides a demo using futures for calculating a set of Mandelbrot
planes. The demo does not assume anything about what type of futures are
used. *The user has full control of how futures are evaluated*. For
instance, to use local batchtools futures, run the demo as:

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`future`](https://future.futureverse.org)`)`\
[`plan`](https://future.futureverse.org/reference/plan.html)`(``future.batchtools``::`[`batchtools_local`](https://future.batchtools.futureverse.org/reference/batchtools_local.md)`)`\
[`demo`](https://rdrr.io/r/utils/demo.html)`(``"mandelbrot"``, package ``=`` ``"future"``, ask ``=`` ``FALSE``)`

## Available batchtools backends

The **future.batchtools** package implements a generic future wrapper
for all batchtools backends. Below are the most common types of
batchtools backends. For other types of parallel and distributed
backends, please see <https://www.futureverse.org/backends.html>.

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
