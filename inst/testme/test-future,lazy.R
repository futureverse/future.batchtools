library(future.batchtools)

message("*** Futures - lazy ...")

strategies <- c("batchtools_local")

for (strategy in strategies) {
  mprintf("- plan('%s') ...\n", strategy)
  plan(strategy)

  a <- 42
  f <- future(2 * a, lazy = TRUE)
  a <- 21
  ## In future (> 1.14.0), resolved() will launch lazy future,
  ## which means for some backends (e.g. sequential) this means
  ## that resolved() might end up returning TRUE.
  f <- resolve(f)
  stopifnot(resolved(f))
  v <- value(f)
  stopifnot(v == 84)

  a <- 42
  v %<-% { 2 * a } %lazy% TRUE
  a <- 21
  f <- futureOf(v)  
  ## In future (> 1.14.0), resolved() will launch lazy future,
  ## which means for some backends (e.g. sequential) this means
  ## that resolved() might end up returning TRUE.
  f <- resolve(f)
  stopifnot(resolved(f))
  stopifnot(v == 84)

  mprintf("- plan('%s') ... DONE\n", strategy)
} ## for (strategy ...)

message("*** Futures - lazy ... DONE")

