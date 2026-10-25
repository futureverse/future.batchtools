#' @tags conditions immediateCondition
#' @tags batchtools_interactive

library(future)

message("*** Immediate conditions are relayed live, if the job is evaluated in the current R session ...")

## A batchtools interactive future is evaluated in the current R session,
## so an immediate condition can be relayed as soon as it is signaled,
## just like for sequential futures.  This is done by the job creating a
## file after having signaled the condition, and the handler checking that
## the file does not exist when the condition is relayed.

cond <- structure(
  class = c("myImmediateCondition", "immediateCondition", "condition"),
  list(message = "hello", call = NULL)
)

strategies <- list(
  batchtools_interactive = future.batchtools::batchtools_interactive
)

for (name in names(strategies)) {
  message(sprintf("- plan(%s) ...", name))
  plan(strategies[[name]])
  
  marker <- tempfile(pattern = "done-")
  live <- logical(0L)
  
  v <- withCallingHandlers({
    value(future({
      signalCondition(cond)
      Sys.sleep(0.5)
      file.create(marker)
      42L
    }, globals = list(cond = cond, marker = marker)))
  }, myImmediateCondition = function(c) live <<- c(live, !file.exists(marker)))
  stopifnot(identical(v, 42L))

  ## The condition is relayed once, and while the job is still running,
  ## that is, it is not relayed again when the job has finished
  stopifnot(identical(live, TRUE))

  message(sprintf("- plan(%s) ... done", name))
}

message("*** Immediate conditions are relayed live, if the job is evaluated in the current R session ... done")
