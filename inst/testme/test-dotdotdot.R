library(future.batchtools)
library(listenv)

strategies <- c("batchtools_interactive", "batchtools_local")

message("*** Global argument '...' in futures ...")

sum_fcns <- list()

sum_fcns$A <- function(x, ...) {
  message("Arguments '...' exists: ", exists("...", inherits = TRUE))
  y %<-% { sum(x, ...) }
  y
}


sum_fcns$B <- function(x, ...) {
  sumt <- function(x) {
    message("Arguments '...' exists: ", exists("...", inherits = TRUE))
    y %<-% { sum(x, ...) }
    y
  }
  sumt(x)
}

sum_fcns$C <- function(x, y) {
  message("Arguments '...' exists: ", exists("...", inherits = TRUE))
  y %<-% { sum(x, y) }
  y
}

sum_fcns$D <- function(x, y) {
  message("Arguments '...' exists: ", exists("...", inherits = TRUE))
  y %<-% { sum(x, y, ...) }
  y
}


for (strategy in strategies) {
  plan(strategy, substitute = FALSE)

  for (name in names(sum_fcns)) {
    mprintf("** Sum function '%s' with plan('%s') ...\n", name, strategy)
    sum_fcn <- sum_fcns[[name]]
    print(sum_fcn)
    y <- try(sum_fcn(1:2, 3))
    print(y)
    if (name %in% c("D")) {
      stopifnot(inherits(y, "try-error"))
    } else {
      stopifnot(y == 6)
    }
  }
}

message("*** Global argument '...' in futures ... DONE")

