library(future)

options(future.debug = FALSE)

message("*** RNG ...")

plan(future.batchtools::batchtools_local)

message("- run() does not update RNG state")

f1 <- future(1, lazy = TRUE)
f2 <- future(2, lazy = TRUE)

rng0 <- globalenv()$.Random.seed

f1 <- run(f1)
stopifnot(identical(globalenv()$.Random.seed, rng0))  ## RNG changed?

f2 <- run(f2)
stopifnot(identical(globalenv()$.Random.seed, rng0))  ## RNG changed?

v1 <- value(f1)
stopifnot(identical(v1, 1))

v2 <- value(f2)
stopifnot(identical(v2, 2))


message("- future() does not update RNG state")

rng0 <- globalenv()$.Random.seed

f1 <- future(1)
stopifnot(identical(globalenv()$.Random.seed, rng0))  ## RNG changed?

f2 <- future(2)
stopifnot(identical(globalenv()$.Random.seed, rng0))  ## RNG changed?

v1 <- value(f1)
stopifnot(identical(v1, 1))

v2 <- value(f2)
stopifnot(identical(v2, 2))


message("- resolved() does not update RNG state")

f1 <- future(1)
f2 <- future(2)

rng0 <- globalenv()$.Random.seed

d1 <- resolved(f1)
stopifnot(identical(globalenv()$.Random.seed, rng0))  ## RNG changed?

d2 <- resolved(f2)
stopifnot(identical(globalenv()$.Random.seed, rng0))  ## RNG changed?

v1 <- value(f1)
stopifnot(identical(v1, 1))

v2 <- value(f2)
stopifnot(identical(v2, 2))


message("- result() does not update RNG state")

f1 <- future(1)
f2 <- future(2)

rng0 <- globalenv()$.Random.seed

r1 <- result(f1)
stopifnot(identical(r1$value, 1))
stopifnot(identical(globalenv()$.Random.seed, rng0))  ## RNG changed?

r2 <- result(f2)
stopifnot(identical(r2$value, 2))
stopifnot(identical(globalenv()$.Random.seed, rng0))  ## RNG changed?

v1 <- value(f1)
stopifnot(identical(v1, 1))

v2 <- value(f2)
stopifnot(identical(v2, 2))


message("- value() does not update RNG state")

f1 <- future(1)
f2 <- future(2)

rng0 <- globalenv()$.Random.seed

v1 <- value(f1)
stopifnot(identical(v1, 1))
stopifnot(identical(globalenv()$.Random.seed, rng0))  ## RNG changed?

v2 <- value(f2)
stopifnot(identical(v2, 2))
stopifnot(identical(globalenv()$.Random.seed, rng0))  ## RNG changed?

message("*** RNG ... DONE")

