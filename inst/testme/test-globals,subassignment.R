library(future)

plan(future.batchtools::batchtools_local)

oopts <- c(oopts, options(
  future.globals.resolve = TRUE,
  future.globals.onMissing = "error"
))


message("*** Globals - subassignments ...")

message("*** Globals - subassignments w/ x$a <- value ...")

## Truth:
x <- x0 <- list()
y0 <- list(a = 1)
str(list(x = x, y0 = y0))

y <- local({
  x$a <- 1
  x
})
stopifnot(identical(y, y0))

y <- local({
  x[["a"]] <- 1
  x
})
stopifnot(identical(y, y0))

y <- local({
  x["a"] <- list(1)
  x
})
stopifnot(identical(y, y0))

stopifnot(identical(x, list()))

## Explicit future
x <- list()
f <- future({
  x$a <- 1
  x
})
rm(list = "x")
y <- value(f)
print(y)
stopifnot(identical(y, y0))

## Future assignment
x <- list()
y %<-% {
  x$a <- 1
  x
}
rm(list = "x")
print(y)
stopifnot(identical(y, y0))

## 'x' is _not_ a global variable here
x <- list()
y %<-% {
  x <- list(b = 2)
  x$a <- 1
  x
}
rm(list = "x")
print(y)
stopifnot(identical(y, list(b = 2, a = 1)))

## Explicit future
x <- list()
f <- future({
  x[["a"]] <- 1
  x
})
rm(list = "x")
y <- value(f)
print(y)
stopifnot(identical(y, y0))

## Future assignment
x <- list()
y %<-% {
  x[["a"]] <- 1
  x
}
rm(list = "x")
print(y)
stopifnot(identical(y, y0))

## Explicit future
x <- list()
f <- future({
  x["a"] <- list(1)
  x
})
rm(list = "x")
y <- value(f)
print(y)
stopifnot(identical(y, y0))

## Future assignment
x <- list()
y %<-% {
  x["a"] <- list(1)
  x
}
rm(list = "x")
print(y)
stopifnot(identical(y, y0))

## Future assignment
x <- list()
name <- "a"
y %<-% {
  x[name] <- list(1)
  x
}
rm(list = c("x", "name"))
print(y)
stopifnot(identical(y, y0))

message("*** Globals - subassignments w/ x$a <- value ... DONE")

message("*** Globals - subassignments ... DONE")

