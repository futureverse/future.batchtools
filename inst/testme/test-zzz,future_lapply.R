library(future)
library(future.batchtools)

if (requireNamespace("future.apply", quietly = TRUE)) {
  future_lapply <- future.apply::future_lapply
  library(listenv)

  ## Setup all strategies including custom once for testing on HPC environments
  print(all_strategies())

  message("All HPC strategies:")
  strategies <- c("batchtools_lsf", "batchtools_openlava", "batchtools_sge",
                  "batchtools_slurm", "batchtools_torque")
  mprint(strategies, debug = TRUE)
  
  message("Supported HPC strategies:")
  strategies <- strategies[sapply(strategies, FUN = test_strategy)]
  mprint(strategies, debug = TRUE)

  strategies <- c("batchtools_local", strategies)
  
  if (fullTest) {
    strategies <- c("batchtools_interactive", strategies)
    
    batchtools_custom_local <- function(expr, substitute = TRUE,
        cluster.functions = batchtools::makeClusterFunctionsInteractive(external = TRUE), ...) {
      if (substitute) expr <- substitute(expr)
      batchtools_custom(expr, substitute = FALSE, ...,
                        cluster.functions = cluster.functions)
    }
    class(batchtools_custom_local) <- c("batchtools_custom_local",
                                        class(batchtools_custom))
    strategies <- c("batchtools_custom_local", strategies)
  }

  message("Strategies to test with:")
  mprint(strategies, debug = TRUE)
  
  message("*** future_lapply() ...")
  
  message("- future_lapply(x, FUN = vector, ...) ...")
  
  x <- list(a = "integer", c = "character", c = "list")
  str(list(x = x))

  y0 <- lapply(x, FUN = vector, length = 2L)
  str(list(y0 = y0))

  for (strategy in strategies) {
    mprintf("- plan('%s') ...\n", strategy)
    plan(strategy)
    mprint(plan, debug = TRUE)
    if (nbrOfWorkers() > 2) plan(strategy, workers = 2L)
    stopifnot(nbrOfWorkers() < Inf)
  
    for (scheduling in list(FALSE, TRUE)) {
      y <- future_lapply(x, FUN = vector, length = 2L,
                         future.scheduling = scheduling)
      str(list(y = y))
      stopifnot(identical(y, y0))
    }
  }
  
  
  message("- future_lapply(x, FUN = base::vector, ...) ...")
  
  x <- list(a = "integer", c = "character", c = "list")
  str(list(x = x))
  
  y0 <- lapply(x, FUN = base::vector, length = 2L)
  str(list(y0 = y0))
  
  for (strategy in strategies) {
    mprintf("- plan('%s') ...\n", strategy)
    plan(strategy)
    mprint(plan, debug = TRUE)
    if (nbrOfWorkers() > 2) plan(strategy, workers = 2L)
    stopifnot(nbrOfWorkers() < Inf)
  
    for (scheduling in list(FALSE, TRUE)) {
      y <- future_lapply(x, FUN = base::vector, length = 2L,
                         future.scheduling = scheduling)
      str(list(y = y))
      stopifnot(identical(y, y0))
    }
  }
  
  message("- future_lapply(x, FUN = future:::hpaste, ...) ...")
  
  x <- list(a = c("hello", b = 1:100))
  str(list(x = x))
  
  y0 <- lapply(x, FUN = future:::hpaste, collapse = "; ", maxHead = 3L)
  str(list(y0 = y0))
  
  for (strategy in strategies) {
    mprintf("- plan('%s') ...\n", strategy)
    plan(strategy)
    mprint(plan, debug = TRUE)
    if (nbrOfWorkers() > 2) plan(strategy, workers = 2L)
    stopifnot(nbrOfWorkers() < Inf)
  
    for (scheduling in list(FALSE, TRUE)) {
      y <- future_lapply(x, FUN = future:::hpaste, collapse = "; ",
                         maxHead = 3L, future.scheduling = scheduling)
      str(list(y = y))
      stopifnot(identical(y, y0))
    }
  }
  
  
  message("- future_lapply(x, FUN = listenv::listenv, ...) ...")
  
  x <- list()
  
  y <- listenv()
  y$A <- 3L
  x$a <- y
  
  y <- listenv()
  y$A <- 3L
  y$B <- c("hello", b = 1:100)
  x$b <- y
  
  print(x)
  
  y0 <- lapply(x, FUN = listenv::mapping)
  str(list(y0 = y0))
  
  for (strategy in strategies) {
    mprintf("- plan('%s') ...\n", strategy)
    plan(strategy)
    if (nbrOfWorkers() > 2) plan(strategy, workers = 2L)
    stopifnot(nbrOfWorkers() < Inf)
    
    for (scheduling in list(FALSE, TRUE)) {
      y <- future_lapply(x, FUN = listenv::mapping, future.scheduling = scheduling)
      str(list(y = y))
      stopifnot(identical(y, y0))
    }
  }
  
  
  message("- future_lapply(x, FUN, ...) for large length(x) ...")
  a <- 3.14
  x <- 1:1e5
  
  y <- future_lapply(x, FUN = function(z) sqrt(z + a))
  y <- unlist(y, use.names = FALSE)
  
  stopifnot(all.equal(y, sqrt(x + a)))
  
  
  message("- future_lapply() with global in non-attached package ...")
  library("tools")
  my_ext <- function(x) file_ext(x)
  y_truth <- lapply("abc.txt", FUN = my_ext)
  
  for (strategy in strategies) {
    plan(strategy)
    if (nbrOfWorkers() > 2) plan(strategy, workers = 2L)
    stopifnot(nbrOfWorkers() < Inf)
    y <- future_lapply("abc.txt", FUN = my_ext)
    stopifnot(identical(y, y_truth))
  }
  
  message("*** future_lapply() ... DONE")
}

