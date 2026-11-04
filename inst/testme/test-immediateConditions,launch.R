#' @tags conditions immediateCondition
#' @tags batchtools_multicore

library(future)

message("*** Immediate conditions are relayed while launching other futures ...")

cond <- structure(
  class = c("myImmediateCondition", "immediateCondition", "condition"),
  list(message = "hello", call = NULL)
)

plan(future.batchtools::batchtools_multicore, workers = 2L)

relayed <- 0L
withCallingHandlers({
  f1 <- future({
    signalCondition(cond)
    Sys.sleep(2)
    1L
  }, globals = list(cond = cond))

  ## Give the job time to signal the condition
  t_end <- Sys.time() + 5
  while (Sys.time() < t_end) {
    Sys.sleep(0.2)
    if (relayed > 0L) break
    ## Launching another future should relay the condition of 'f1'
    f2 <- future(2L)
    value(f2)
  }
  stopifnot(relayed == 1L)
}, myImmediateCondition = function(c) relayed <<- relayed + 1L)

stopifnot(identical(value(f1), 1L))
plan(sequential)

message("*** Immediate conditions are relayed while launching other futures ... done")
