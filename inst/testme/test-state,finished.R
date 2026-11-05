library(future)

message("*** Future state 'finished' implies a collected result ...")

plan(future.batchtools::batchtools_local)

exprs <- list(
  success = quote(42L),
  error   = quote(stop("boom"))
)

for (name in names(exprs)) {
  message(sprintf("- %s", name))
  f <- eval(bquote(future(.(exprs[[name]]), seed = NULL)))

  t0 <- Sys.time()
  while (!resolved(f)) {
    stopifnot(difftime(Sys.time(), t0, units = "secs") < 60)
    Sys.sleep(0.1)
  }

  ## A future must not report state 'finished' before its result has
  ## been collected, because print() and other methods rely on it
  message("State after resolved(): ", f[["state"]])
  stopifnot(!identical(f[["state"]], "finished") || inherits(f[["result"]], "FutureResult"))

  ## Without a collected result, print() must not claim that the
  ## future was resolved successfully
  if (identical(name, "error") && !inherits(f[["result"]], "FutureResult")) {
    out <- capture.output(print(f))
    stopifnot(!any(grepl("resolved successfully", out)))
  }

  ## Collecting the result moves it to 'finished'
  res <- result(f)
  stopifnot(inherits(res, "FutureResult"))
  stopifnot(identical(f[["state"]], "finished"))
  stopifnot(inherits(f[["result"]], "FutureResult"))

  out <- capture.output(print(f))
  if (identical(name, "error")) {
    stopifnot(any(grepl("run-time", out)))
  } else {
    stopifnot(any(grepl("resolved successfully", out)))
  }
}

message("*** Future state 'finished' implies a collected result ... DONE")
