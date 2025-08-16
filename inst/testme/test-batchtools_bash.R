#' @tags skip_on_cran
#' @tags batchtools_bash

library(future)

message("*** batchtools_bash() ...")

bin <- Sys.which("bash")
if (utils::file_test("-f", bin)) {
  plan(future.batchtools::batchtools_bash)
  print(plan())
  
  message("*** Launch future")
  
  f <- future({
    42L
  })
  stopifnot(inherits(f, "BatchtoolsFuture"))
  
  y <- value(f)
  print(y)
  stopifnot(y == 42L)
  
  
  message("*** Launch future with run-time error")
  f <- future({
    stop("Whoops!")
    1
  })
  v <- value(f, signal = FALSE)
  print(v)
  stopifnot(inherits(v, "error"))
} else {
  message("Skipping: 'bash' is not available")
}

message("*** batchtools_bash() ... DONE")

