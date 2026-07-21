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

  \[`numeric(1)`\]\
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

## Value

Nothing.

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
#> Main process ID: 2603437

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
#>     hostname    os cores     pid
#> 1 hb-x1-2023 Linux     8 2603437
#>                                                                                                                                                                                                                                                                                                                  modules
#> 1 CBI:cmake/4.3.3:r/4.6.1:pandoc/3.9:quarto/1.9.38:port4me/0.7.1:imagemagick/7.1.2-24:bat/0.26.1:fzf/0.74.0:glow/2.1.2:github-cli/2.96.0:git-flow/1.12.3:git-extras/7.5.0:shellcheck/0.11.0:node/26.0.0:markdownlint-cli/0.49.1:rclone/1.74.4:osgrep/0.5.16:opencode/1.15.12:bash-startup/0.5.0:restic/0.19.1:deno/2.9.3
```
