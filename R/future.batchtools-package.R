#' future.batchtools: A Future for batchtools
#'
#' The \pkg{future.batchtools} package implements the Future API
#' on top of \pkg{batchtools} such that futures can be resolved
#' on for instance high-performance compute (HPC) clusters via
#' job schedulers.
#' The Future API is defined by the \pkg{future} package.
#'
#' To use batchtools futures, attach \pkg{future}, and
#' select the type of future you wish to use via
#' [future::plan()].
#'
#' @section Example:
#' ```r
#' library(futurize)                          # also attaches 'future'
#' plan(future.batchtools::batchtools_slurm)  # parallelize via Slurm jobs
#'
#' # A slow function
#' slow_fcn <- function(x) {
#'   Sys.sleep(1.0)
#'   x^2
#' }
#'
#' # Example data
#' xs <- 1:10
#'
#' # Sequential and parallel base R apply
#' ys <- lapply(xs, slow_fcn)
#' ys <- lapply(xs, slow_fcn) |> futurize()
#'
#' # Sequential and parallel purrr map
#' library(purrr)
#' ys <- xs |> map(slow_fcn)
#' ys <- xs |> map(slow_fcn) |> futurize()
#'
#' # Sequential and parallel foreach calls
#' library(foreach)
#' ys <- foreach(x = xs) %do% slow_fcn(x)
#' ys <- foreach(x = xs) %do% slow_fcn(x) |> futurize()
#' ```
#'
#' @name future.batchtools
#' @aliases future.batchtools-package
"_PACKAGE"
