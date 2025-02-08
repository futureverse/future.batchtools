#' future.batchtools: A Future for batchtools
#'
#' The \pkg{future.batchtools} package implements the Future API
#' on top of \pkg{batchtools} such that futures can be resolved
#' on for instance high-performance compute (HPC) clusters via
#' job schedulers.
#' The Future API is defined by the \pkg{future} package.
#'
#' To use batchtools futures, load \pkg{future.batchtools}, and
#' select the type of future you wish to use via
#' [future::plan()].
#'
#' @example incl/future.batchtools.R
#'
#' @examples
#' \donttest{
#' plan(batchtools_local)
#' demo("mandelbrot", package = "future", ask = FALSE)
#' }
#'
#' @name future.batchtools
#' @aliases future.batchtools-package
"_PACKAGE"
