if (.Platform[["OS.type"]] != "windows") {
  library(future.batchtools)
  library(listenv)
  
  message("*** batchtools_ssh() ...")
  
  plan(batchtools_ssh, workers = 2L)
  supports_ssh <- tryCatch({
    f <- future(42L)
    v <- value(f)
    identical(v, 42L)
  }, error = function(e) FALSE)
  message("Supports batchtools_ssh: ", supports_ssh)
  
  if (supports_ssh) {
    message("future(a) ...")
    a0 <- a <- 42
    f <- future(a)
    stopifnot(identical(f$globals$a, a0))
    v <- value(f)
    stopifnot(identical(v, a0))
  
    message("future(a, lazy = TRUE) ...")
    a0 <- a <- 42
    f <- future(a, lazy = TRUE)
    rm(list = "a")
    stopifnot(identical(f$globals$a, a0))
    v <- value(f)
    stopifnot(identical(v, a0))
  } ## if (supports_ssh)
  
  message("*** batchtools_ssh() ... DONE")
}


