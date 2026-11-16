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


#' @importFrom future FutureError nbrOfWorkers
waitForWorker <- function(backend, workers = nbrOfWorkers(backend), debug = FALSE) {
  ## FutureRegistry to use
  reg <- backend[["reg"]]

  timeout <- backend[["future.wait.timeout"]]
  delta <- backend[["future.wait.interval"]]
  alpha <- backend[["future.wait.alpha"]]

  t0 <- Sys.time()
  dt <- 0
  iter <- 1L
  interval <- delta
  finished <- FALSE
  while (dt <= timeout) {
    ## Check for available workers
    used <- length(FutureRegistry(reg, action = "list", earlySignal = FALSE))
    finished <- (used < workers)
    if (finished) break

    if (debug) mdebugf("Poll #%d (%s): usedWorkers() = %d, workers = %d", iter, format(round(dt, digits = 2L)), used, workers)

    ## Wait
    Sys.sleep(interval)
    interval <- alpha * interval

    ## Finish/close workers, iff possible
    FutureRegistry(reg, action = "collect-first")

    iter <- iter + 1L
    dt <- difftime(Sys.time(), t0, units = "secs")
  }

  if (!finished) {
    msg <- sprintf("TIMEOUT: All %d workers are still occupied after %s (polled %d times)", workers, format(round(dt, digits = 2L)), iter)
    if (debug) mdebug(msg)
    stop(FutureError(msg))
  }
}
