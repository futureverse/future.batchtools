waitForWorker <- function(future, ...) {
  UseMethod("waitForWorker")
}

#' @export
waitForWorker.default <- function(future, ...) NULL

#' @export
waitForWorker.BatchtoolsUniprocessFuture <- function(future, ...) NULL


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
  freg <- sprintf("workers-%s", class(future)[1])
  FutureRegistry(freg, action = "add", future = future, earlySignal = FALSE, ...)
}


#' @export
unregisterFuture.BatchtoolsFuture <- function(future, ...) {
  freg <- sprintf("workers-%s", class(future)[1])
  FutureRegistry(freg, action = "remove", future = future, ...)
}


#' @importFrom future FutureError
#' @export
waitForWorker.BatchtoolsFuture <- function(future,
         workers,
         await = NULL,
         timeout = getOption("future.wait.timeout", 30 * 24 * 60 * 60),
         delta = getOption("future.wait.interval", 0.2),
         alpha = getOption("future.wait.alpha", 1.01),
         ...) {
  debug <- isTRUE(getOption("future.debug"))
  if (debug) {
    mdebugf_push("waitForWorker() for %s ...", class(future)[1])
    on.exit(mdebug_pop())
  }

  stop_if_not(is.null(await) || is.function(await))
  workers <- as.integer(workers)
  stop_if_not(length(workers) == 1, is.finite(workers), workers >= 1L)
  stop_if_not(length(timeout) == 1, is.finite(timeout), timeout >= 0)
  stop_if_not(length(alpha) == 1, is.finite(alpha), alpha > 0)

  freg <- sprintf("workers-%s", class(future)[1])

  ## Use a default await() function?
  if (is.null(await)) {
    await <- function() FutureRegistry(freg, action = "collect-first")
  }  
 
  ## Number of occupied workers
  usedWorkers <- function() {
    length(FutureRegistry(freg, action = "list", earlySignal = FALSE))
  }

  t0 <- Sys.time()
  dt <- 0
  iter <- 1L
  interval <- delta
  finished <- FALSE
  while (dt <= timeout) {
    ## Check for available workers
    used <- usedWorkers()
    finished <- (used < workers)
    if (finished) break

    if (debug) mdebugf("Poll #%d (%s): usedWorkers() = %d, workers = %d", iter, format(round(dt, digits = 2L)), used, workers)

    ## Wait
    Sys.sleep(interval)
    interval <- alpha * interval
    
    ## Finish/close workers, iff possible
    await()

    iter <- iter + 1L
    dt <- difftime(Sys.time(), t0)
  }

  if (!finished) {
    msg <- sprintf("TIMEOUT: All %d workers are still occupied after %s (polled %d times)", workers, format(round(dt, digits = 2L)), iter)
    if (debug) mdebug(msg)
    stop(FutureError(msg))
  }
}
