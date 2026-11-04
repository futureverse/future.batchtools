#' @tags conditions immediateCondition
#' @tags batchtools_local batchtools_multicore

library(future)

message("*** Immediate conditions ...")

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

for (name in names(strategies)) {
  message(sprintf("- plan(%s) ...", name))
  plan(strategies[[name]])

  ## Some batchtools backends, e.g. 'batchtools_local', evaluate the future
  ## when it is created, so the condition is relayed then

  message("  - Poll with resolved() and then call value()")
  count <- 0L
  f <- withCallingHandlers({
    future({
      signalCondition(cond)
      Sys.sleep(0.5)
      42L
    })
  }, myImmediateCondition = handler)
  withCallingHandlers({
    while (!resolved(f)) Sys.sleep(0.1)
  }, myImmediateCondition = handler)

  ## The immediate condition is relayed before value() is called
  stopifnot(count == 1L)

  withCallingHandlers({
    v <- value(f)
  }, myImmediateCondition = handler)
  stopifnot(identical(v, 42L))

  ## ... and only once
  stopifnot(count == 1L)

  message("  - Querying again must not relay the condition again")
  withCallingHandlers({
    r <- resolved(f)
    v <- value(f)
  }, myImmediateCondition = handler)
  stopifnot(r, identical(v, 42L), count == 1L)

  message("  - Call value() without polling")
  count <- 0L
  withCallingHandlers({
    f <- future({
      signalCondition(cond)
      42L
    })
    v <- value(f)
  }, myImmediateCondition = handler)
  stopifnot(identical(v, 42L))
  stopifnot(count == 1L)

  message("  - Immediate conditions of other futures are not relayed")
  f1 <- future({ Sys.sleep(0.5); 1L })
  f2 <- future({ signalCondition(cond); 2L })
  count <- 0L
  withCallingHandlers({
    while (!resolved(f1)) Sys.sleep(0.1)
    v1 <- value(f1)
  }, myImmediateCondition = handler)
  stopifnot(identical(v1, 1L))
  stopifnot(count == 0L)
  v2 <- value(f2)
  stopifnot(identical(v2, 2L))

  message(sprintf("- plan(%s) ... done", name))
}

message("*** Immediate conditions ... done")
