library(future)
library(listenv)

message("*** batchtools_interactive() ...")

message("*** batchtools_interactive() without globals")

plan(future.batchtools::batchtools_interactive)

f <- future({
  42L
})
stopifnot(inherits(f, "BatchtoolsFuture"))

## Check whether a batchtools_interactive future is resolved
## or not will force evaluation
print(is_resolved <- resolved(f))
stopifnot(is_resolved)

y <- value(f)
print(y)
stopifnot(y == 42L)


message("*** batchtools_interactive() with globals")
## A global variable
a <- 0
f <- future({
  b <- 3
  c <- 2
  a * b * c
})

## Although 'f' is a batchtools_interactive future and therefore
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


message("*** batchtools_interactive() with globals (tricky)")
x <- listenv()
for (ii in 1:2) x[[ii]] <- future({ ii }, globals = TRUE)
v <- unlist(value(x))
stopifnot(all(v == 1:2))  ## Make sure globals are frozen


message("*** batchtools_interactive() and errors")
f <- future({
  stop("Whoops!")
  1
})
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

message("*** batchtools_interactive() ... DONE")

