registerFuture <- function(future, ...) {
  UseMethod("registerFuture")
}

#' @export
registerFuture.default <- function(future, ...) NULL

#' @export
registerFuture.BatchtoolsUniprocessFuture <- function(future, ...) NULL



unregisterFuture <- function(future, ...) {
  UseMethod("unregisterFuture")
}

#' @export
unregisterFuture.default <- function(future, ...) NULL

#' @export
unregisterFuture.BatchtoolsUniprocessFuture <- function(future, ...) NULL


#' @export
registerFuture.BatchtoolsFuture <- function(future, ...) {
  backend <- future[["backend"]]
  FutureRegistry(backend[["reg"]], action = "add", future = future, earlySignal = FALSE, ...)
}


#' @export
unregisterFuture.BatchtoolsFuture <- function(future, ...) {
  backend <- future[["backend"]]
  try(FutureRegistry(backend[["reg"]], action = "remove", future = future, ...), silent = TRUE)
}


## Local copy of future:::waitForWorker(), for future (< 1.82.0-9016).
## It is replaced by the one in 'future', if available, when this
## package is loaded; see import_future_functions()
#' @importFrom future FutureError nbrOfWorkers
waitForWorker <- function(backend, workers = nbrOfWorkers(backend), timeout = backend[["timeout"]], delta = backend[["delta"]], alpha = backend[["alpha"]], registry = FutureRegistry, debug = FALSE) {
  stop_if_not(
    length(workers) == 1L, is.numeric(workers), !is.na(workers), workers >= 1,
    length(timeout) == 1L, is.numeric(timeout), !is.na(timeout), timeout >= 0,
    length(delta) == 1L, is.numeric(delta), is.finite(delta), delta > 0,
    length(alpha) == 1L, is.numeric(alpha), is.finite(alpha), alpha > 0,
    is.function(registry)
  )

  ## Name of the registry to poll
  reg <- backend[["reg"]]

  t0 <- Sys.time()
  dt <- 0
  iter <- 1L
  interval <- delta
  finished <- FALSE
  while (dt <= timeout) {
    ## Check for available workers
    used <- length(registry(reg, action = "list", earlySignal = FALSE))
    finished <- (used < workers)
    if (finished) break

    if (debug) mdebugf("Poll #%d (%s): usedWorkers() = %d, workers = %d", iter, format(round(dt, digits = 2L)), used, workers)

    ## Wait
    Sys.sleep(interval)
    interval <- alpha * interval

    ## Finish/close workers, iff possible
    registry(reg, action = "collect-first")

    iter <- iter + 1L
    dt <- difftime(Sys.time(), t0, units = "secs")
  }

  if (!finished) {
    msg <- sprintf("TIMEOUT: All %d workers are still occupied after %s (polled %d times)", workers, format(round(dt, digits = 2L)), iter)
    if (debug) mdebug(msg)
    stop(FutureError(msg))
  }

  invisible(NULL)
}
