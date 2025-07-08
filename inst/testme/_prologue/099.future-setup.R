## Use local callr futures by default
oplan <- local({
  oopts <- options(future.debug = FALSE)
  on.exit(options(oopts))
  future::plan(future::sequential)
})

all_strategies <- local({
  .cache <- NULL
  function(envir = parent.frame()) {
    if (!is.null(.cache)) return(.cache)

    strategies <- Sys.getenv("R_FUTURE_TESTS_STRATEGIES")
    strategies <- unlist(strsplit(strategies, split = ","))
    strategies <- gsub(" ", "", strategies)
    strategies <- strategies[nzchar(strategies)]
    
    ## When testing for instance 'batchtools_sge', look for a customize
    ## template file, e.g. R_BATCHTOOLS_SEARCH_PATH/batchtools.sge.tmpl
    if (length(strategies) > 0L) {
      ## If there is a custom R_BATCHTOOLS_SEARCH_PATH/setup.R' file, run it
      pathname <- file.path(path, "setup.R")
      if (file_test("-f", pathname)) source(pathname, local = envir)
    }
    
    strategies <- c(future:::supportedStrategies(), strategies)
    strategies <- unique(strategies)
    .cache <<- strategies
    
    strategies
  }
})

test_strategy <- function(strategy) {
  strategy %in% all_strategies()
}

## In case it set outside, reset:
options(future.batchtools.workers = NULL)
Sys.unsetenv("R_FUTURE_BATCHTOOLS_WORKERS")

path <- Sys.getenv("R_BATCHTOOLS_SEARCH_PATH")
if (!nzchar(path)) {
  path <- system.file(package = "future.batchtools",
                      "templates-for-R_CMD_check", mustWork = TRUE)
  Sys.setenv(R_BATCHTOOLS_SEARCH_PATH = path)
} else {
  warning("Using a non-standard R_BATCHTOOLS_SEARCH_PATH while testing: ",
          sQuote(path))
  if (!file_test("-d", path)) {
    stop("R_BATCHTOOLS_SEARCH_PATH specifies a non-existing folder: ",
         sQuote(path))
  }
}
