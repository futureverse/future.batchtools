#' @tags conditions immediateCondition
#' @tags batchtools_local batchtools_multicore

library(future)

message("*** Option future.batchtools.relay.immediate ...")

strategies <- list(
  batchtools_local = future.batchtools::batchtools_local
)
if (.Platform[["OS.type"]] != "windows" && parallelly::supportsMulticore()) {
  strategies[["batchtools_multicore"]] <- future.batchtools::batchtools_multicore
}

cond <- structure(
  class = c("myImmediateCondition", "immediateCondition", "condition"),
  list(message = "hello", call = NULL)
)

count <- 0L
handler <- function(c) count <<- count + 1L

## Folder where the worker saves immediate conditions, if enabled
immediate_folder <- function(f) {
  file.path(f[["config"]][["reg"]][["file.dir"]], ".future", "immediateConditions")
}

for (name in names(strategies)) {
  message(sprintf("- plan(%s) ...", name))
  plan(strategies[[name]])

  for (relay in c(TRUE, FALSE)) {
    message(sprintf("  - future.batchtools.relay.immediate = %s", relay))
    oopts <- options(future.batchtools.relay.immediate = relay)

    count <- 0L
    f <- withCallingHandlers({
      future({
        for (kk in 1:5) signalCondition(cond)
        Sys.sleep(0.5)
        42L
      })
    }, myImmediateCondition = handler)

    ## Immediate conditions are saved to file, unless disabled
    stopifnot(identical(dir.exists(immediate_folder(f)), relay))

    ## Changing the option after the future was launched has no effect on it
    options(future.batchtools.relay.immediate = !relay)

    withCallingHandlers({
      while (!resolved(f)) Sys.sleep(0.1)
      v <- value(f)
    }, myImmediateCondition = handler)
    stopifnot(identical(v, 42L))

    ## All conditions are relayed, once, whether they are relayed as soon
    ## as possible, or at the end, like any other condition
    stopifnot(count == 5L)

    options(oopts)
  }

  message(sprintf("- plan(%s) ... done", name))
}

message("*** Option future.batchtools.relay.immediate ... done")
