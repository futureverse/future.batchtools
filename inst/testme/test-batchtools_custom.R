library(future.batchtools)
library(batchtools)
library(listenv)

message("*** batchtools_custom() ...")

message("*** batchtools_custom() w/ 'conf.file' on R_BATCHTOOLS_SEARCH_PATH")

f <- batchtools_custom({
  42L
})
print(f)
stopifnot(inherits(f, "BatchtoolsFuture"))
v <- value(f)
print(v)
stopifnot(v == 42L)


message("*** batchtools_custom() w/ 'cluster.functions' without globals")

cf <- makeClusterFunctionsInteractive(external = TRUE)
str(cf)

f <- batchtools_custom({
  42L
}, cluster.functions = cf)
stopifnot(inherits(f, "BatchtoolsFuture"))

## Check whether a batchtools_custom future is resolved
## or not will force evaluation
print(is_resolved <- resolved(f))
stopifnot(is_resolved)

y <- value(f)
print(y)
stopifnot(y == 42L)


message("*** batchtools_custom()  w/ 'cluster.functions' with globals")
## A global variable
a <- 0
f <- batchtools_custom({
  b <- 3
  c <- 2
  a * b * c
}, cluster.functions = cf)
print(f)

## Although 'f' is a batchtools_custom future and therefore
## resolved/evaluates the future expression only
## when the value is requested, any global variables
## identified in the expression (here 'a') are
## "frozen" at the time point when the future is
## created.  Because of this, 'a' preserved the
## zero value although we reassign it below
a <- 7  ## Make sure globals are frozen
v <- value(f)
print(v)
stopifnot(v == 0)


message("*** batchtools_custom()  w/ 'cluster.functions' with globals (tricky)")
x <- listenv()
for (ii in 1:2) {
  x[[ii]] <- batchtools_custom({ ii }, globals = TRUE, cluster.functions = cf)
}
v <- unlist(value(x))
stopifnot(all(v == 1:2))  ## Make sure globals are frozen


message("*** batchtools_custom()  w/ 'cluster.functions' and errors")
f <- batchtools_custom({
  stop("Whoops!")
  1
}, cluster.functions = cf)
v <- value(f, signal = FALSE)
print(v)
stopifnot(inherits(v, "simpleError"))

res <- try({ v <- value(f) }, silent = TRUE)
print(res)
stopifnot(inherits(res, "try-error"))

## Error is repeated
res <- try(value(f), silent = TRUE)
print(res)
stopifnot(inherits(res, "try-error"))

message("*** batchtools_custom() ... DONE")

