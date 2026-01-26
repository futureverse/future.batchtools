# A batchtools backend that resolves futures sequentially in the current R session

The batchtools interactive backend is useful for verifying parts of your
batchtools setup locally, while still being able to do interactive
debugging.

## Usage

``` r
batchtools_interactive(
  ...,
  fs.latency = 0,
  delete = getOption("future.batchtools.delete", "on-success")
)
```

## Arguments

- fs.latency:

  \[`numeric(1)`\]  
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

- ...:

  Not used.

## Details

Batchtools interactive futures use batchtools cluster functions created
by
[`batchtools::makeClusterFunctionsInteractive()`](https://batchtools.mlr-org.com/reference/makeClusterFunctionsInteractive.html)
with `external = TRUE`.

An alternative to the batchtools interactive backend is to use
`plan(future::sequential)`, which is a faster way process futures
sequentially and that also can be debugged interactively.

## Examples

``` r
library(future)
plan(future.batchtools::batchtools_interactive)

message("Main process ID: ", Sys.getpid())
#> Main process ID: 276558

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
#> 1 hb-x1-2023 Linux     8 276558
#>                                                                                                   modules
#> 1 CBI:bash-startup/0.5.0:restic/0.18.1:r/4.5.2:quarto/1.8.26:bat/0.26.1:git-flow/1.12.3:shellcheck/0.11.0
```
