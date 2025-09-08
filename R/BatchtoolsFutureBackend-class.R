#' A batchtools future is a future whose value will be resolved via batchtools
#'
#' @inheritParams future::FutureBackend
#'
#' @param workers (optional) The maximum number of workers the batchtools
#' backend may use at any time.   Interactive and "local" backends can only
#' process one future at the time (`workers = 1`), whereas HPC backends,
#' where futures are resolved via separate jobs on a scheduler, can have
#' multiple workers. In the latter, the default is `workers = NULL`, which
#' will resolve to \code{getOption("\link{future.batchtools.workers}", 100)}.
#'
#' @param finalize If TRUE, a future's \pkg{batchtools}
#' \link[batchtools:makeRegistry]{Registry} is automatically deleted when
#' the future is garbage collected, otherwise not.
#'
#' @param cluster.functions (optional) Assigned as-is to the each future's
#' \pkg{batchtools} \link[batchtools:makeRegistry]{Registry}.
#'
#' @param registry (optional) A named list of settings applied to each
#' future's \pkg{batchtools} \link[batchtools:makeRegistry]{Registry}.
#' This is a more convenient alternative to using argument `conf.file`.
#'
#' @param conf.file (optional) A "batchtools-configuration" R script, which
#' is sourced when each future's \pkg{batchtools}
#' \link[batchtools:makeRegistry]{Registry} is created. Any variables
#' created by this script is assigned to the registry.
#' The default file is the one found by [batchtools::findConfFile()], if any.
#'
#' @param resources (optional) A named list passed to the \pkg{batchtools}
#' job-script template as variable `resources`. This is based on how
#' [batchtools::submitJobs()] works, with the exception for specially
#' reserved names defined by the \pkg{future.batchtools} package;
#' * `resources[["asis"]]` is a character vector that are passed as-is to
#'   the job script and are injected as job resource declarations.
#' * `resources[["modules"]]` is character vector of Linux environment
#'   modules to be loaded.
#' * `resources[["startup"]]` and `resources[["shutdown"]]` are character
#'   vectors of shell code to be injected to the job script as-is.
#' * `resources[["details"]]`, if TRUE, results in the job script outputting
#'   job details and job summaries at the beginning and at the end.
#' * All remaining `resources` named elements are injected as named resource
#'   specification for the scheduler.
#'
#'
#' @param delete Controls if and when the batchtools job registry folder is
#' deleted.
#' If `"on-success"` (default), it is deleted if the future was resolved
#' successfully _and_ the expression did not produce an error.
#' If `"never"`, then it is never deleted.
#' If `"always"`, then it is always deleted.
#'
#' @param \ldots Not used.
#'
#' @return A [future::FutureBackend] object of class BatchtoolsFutureBackend 
#'
#' @aliases BatchtoolsUniprocessFutureBackend BatchtoolsMultiprocessFutureBackend
#' @importFrom utils file_test
#' @importFrom future FutureBackend
#' @keywords internal
#' @export
BatchtoolsFutureBackend <- function(
                             workers = 1L, resources = list(),
                             finalize = getOption("future.finalize", TRUE),
                             cluster.functions = NULL,
                             registry = list(),
                             conf.file = findConfFile(),
                             interrupts = TRUE,
                             delete = getOption("future.batchtools.debug", "on-success"),
                             ...) {
  assert_no_positional_args_but_first()

  if (is.function(workers)) workers <- workers()
  if (is.numeric(workers)) {
    stop_if_not(length(workers) == 1L, !is.na(workers), workers >= 1)
  } else if (is.character(workers)) {
    stop_if_not(length(workers) >= 1L, !anyNA(workers), all(nzchar(workers)))
  } else {
    stop("Argument 'workers' should be either a numeric, a character vector, or a function: ", mode(workers))
  }

  if (!is.null(cluster.functions)) {
    stop_if_not(is.list(cluster.functions))
    stop_if_not(inherits(cluster.functions, "ClusterFunctions"))
  } else {
    ## If 'cluster.functions' is not specified, then 'conf.file' must
    ## exist
    if (!file_test("-f", conf.file)) {
      stop("No such batchtools configuration file: ", sQuote(conf.file))
    }
  }
  
  stop_if_not(is.list(registry))
  if (length(registry) > 0L) {
    stopifnot(!is.null(names(registry)), all(nzchar(names(registry))))
  }
  
  stop_if_not(is.list(resources))
  stop_if_not(is.character(delete))
  delete <- match.arg(delete, choices = c("on-success", "never", "always"))

  core <- FutureBackend(
    reg = "workers-batchtools",
    workers = workers,
    resources = resources,
    conf.file = conf.file,
    cluster.functions = cluster.functions,
    registry = registry,
    finalize = finalize,
    interrupts = interrupts,
    delete = delete,
    future.wait.timeout = getOption("future.wait.timeout", 30 * 24 * 60 * 60),
    future.wait.interval = getOption("future.wait.interval", 0.01),
    future.wait.alpha = getOption("future.wait.alpha", 1.01),
    ...
  )
  core[["futureClasses"]] <- c("BatchtoolsFuture", core[["futureClasses"]])
  core <- structure(core, class = c("BatchtoolsFutureBackend", "FutureBackend", class(core)))
  core
}


#' @inheritParams BatchtoolsFutureBackend
#'
#' @export
BatchtoolsUniprocessFutureBackend <- function(workers = 1L, ...) {
  assert_no_positional_args_but_first()
  core <- BatchtoolsFutureBackend(workers = 1L, ...)
  class(core) <- c("BatchtoolsUniprocessFutureBackend", "BatchtoolsFutureBackend", "SequentialFutureBackend", "FutureBackend")
  core
}

#' @inheritParams BatchtoolsFutureBackend
#'
#' @export
BatchtoolsMultiprocessFutureBackend <- function(...) {
  assert_no_positional_args_but_first()
  core <- BatchtoolsFutureBackend(...)
  class(core) <- c("BatchtoolsMultiprocessFutureBackend", "BatchtoolsFutureBackend", "MultiprocessFutureBackend", "FutureBackend")
  core
}


#' @importFrom utils str
#' @export
#' @keywords internal
print.BatchtoolsFutureBackend <- function(x, ...) {  
  NextMethod()

  backend <- x
  conf.file <- backend[["conf.file"]]
  printf("batchtools configuration file: %s\n", file_info(conf.file))
  
  cluster.functions <- backend[["cluster.functions"]]
  printf("batchtools cluster functions: %s\n",
         sQuote(cluster.functions$name))
  template <- attr(cluster.functions, "template")
  printf("batchtools cluster functions template: %s\n", file_info(template))

  path <- future_cache_path()
  printf("Cache directory: %s\n", dir_info(path))

  resources <- backend[["resources"]]
  printf("batchtools resources:\n")
  str(resources)

  invisible(x)
}


#' @importFrom batchtools saveRegistry batchExport batchMap setJobNames submitJobs
#' @importFrom future launchFuture
#' @export
launchFuture.BatchtoolsFutureBackend <- local({
  function(backend, future, ...) {
    debug <- isTRUE(getOption("future.debug"))
    if (debug) {
      mdebugf_push("launchFuture() for %s ...", class(backend)[1])
      on.exit(mdebugf_pop())
    }

    if (future[["state"]] != "created") {
      label <- sQuoteLabel(future)
      msg <- sprintf("A future ('%s') can only be launched once", label)
      stop(FutureError(msg, future = future))
    }

    ## Assert that the process that created the future is
    ## also the one that evaluates/resolves/queries it.
    assertOwner(future)

    ## Temporarily disable batchtools output?
    ## (i.e. messages and progress bars)
    batchtools_output <- getOption("future.batchtools.output", debug)
    if (!batchtools_output) {
      oopts <- options(batchtools.verbose = FALSE, batchtools.progress = FALSE)
    } else {
      oopts <- list()
    }
    on.exit(options(oopts), add = TRUE)
  
    ## (i) Create batchtools registry
    reg <- local({
      reg <- NULL
      if (debug) {
        mdebug_push("Creating batchtools registry ...")
        on.exit({
          mprint(reg)
          mdebug_pop()
        })
      }
      reg <- temp_registry(
        label             = future[["label"]],
        conf.file         = backend[["conf.file"]],
        cluster.functions = backend[["cluster.functions"]],
        config            = backend[["registry"]]
      )
      stop_if_not(inherits(reg, "Registry"))
      reg
    })

    config <- list(reg = reg)
    if (debug) {
      mprint(list(config = config))
    }
    future[["config"]] <- config
    future[["delete"]] <- backend[["delete"]]

    ## Register finalizer?
    if (backend[["finalize"]]) future <- add_finalizer(future)

    ## (ii) Attach packages that needs to be attached
    packages <- future$packages
    if (length(packages) > 0) local({
      if (debug) {
        mdebugf_push("Attaching %d packages (%s) ...",
                     length(packages), hpaste(sQuote(packages)))
        on.exit(mdebug_pop())
      }
  
      ## Record which packages in 'pkgs' that are loaded and
      ## which of them are attached (at this point in time).
      is_loaded <- is.element(packages, loadedNamespaces())
      is_attached <- is.element(packages, attached_packages())
  
      ## FIXME: Update the expression such that the new session
      ## will have the same state of (loaded, attached) packages.
  
      reg$packages <- packages
      with_stealth_rng({
        saveRegistry(reg = reg)
      })
    })
    ## Not needed anymore
    packages <- NULL
  
    ## (iii) Export globals?
    if (length(future$globals) > 0) {
      batchExport(export = future$globals, reg = reg)
    }

    ## 1. Add to batchtools for evaluation
    jobid <- local({
      if (debug) {
        mdebug_push("batchtools::batchMap() ...")
        on.exit(mdebug_pop())
      }

      data <- getFutureData(future)
    
      ## WORKAROUND: batchtools::batchMap() updates the RNG state,
      ## which we must make sure to undo.
      with_stealth_rng({
        jobid <- batchMap(fun = future:::evalFuture, list(data), reg = reg)
      })
      jobid
    })

    config[["jobid"]] <- jobid
    future[["config"]] <- config

    ## 2. Set job name, if specified
    label <- future$label
    if (!is.null(label)) local({
      if (debug) {
        mdebug_push("batchtools::setJobNames() ...")
        on.exit(mdebug_pop())
      }
      setJobNames(ids = jobid, names = label, reg = reg)
    })
    
    if (debug) mdebugf("Created %s future #%d", class(future)[1], jobid$job.id)


    ## 3. WORKAROUND: (For multicore and macOS only)
    if (reg$cluster.functions$name == "Multicore") local({
      if (debug) {
        mdebug_push("Multicore 'ps' workaround ...")
        on.exit(mdebug_pop())
      }
    
      ## On some macOS systems, a system call to 'ps' may output an error message
      ## "dyld: DYLD_ environment variables being ignored because main executable
      ##  (/bin/ps) is setuid or setgid" to standard error that is picked up by
      ## batchtools which incorrectly tries to parse it.  By unsetting all DYLD_*
      ## environment variables, we avoid this message.  For more info, see:
      ## * https://github.com/tudo-r/BatchJobs/issues/117
      ## * https://github.com/futureverse/future.BatchJobs/issues/59
      ## /HB 2016-05-07
      dyld_envs <- tryCatch({
        envs <- list()
        res <- system2("ps", stdout = TRUE, stderr = TRUE)
        if (any(grepl("DYLD_", res))) {
          envs <- Sys.getenv()
          envs <- envs[grepl("^DYLD_", names(envs))]
          if (length(envs) > 0L) lapply(names(envs), FUN = Sys.unsetenv)
        }
        envs
      }, error = function(ex) list())
    })

    ## 4. Wait for an available worker
    waitForWorker(future, workers = backend[["workers"]])

    ## 5. Submit
    resources <- backend[["resources"]]
    config[["resources"]] <- resources
    future[["config"]] <- config

    ## WORKAROUND: batchtools::submitJobs() updates the RNG state,
    ## which we must make sure to undo.
    tryCatch({
      with_stealth_rng({
        submitJobs(reg = reg, ids = jobid, resources = resources)
      })
    }, error = function(ex) {
      path <- reg$file.dir
      msg <- conditionMessage(ex)
      label <- sQuoteLabel(future)
      msg <- sprintf("Failed to submit %s (%s). The reason was: %s", class(future)[1], label, msg)
      info <- capture.output(str(resources))
      info <- paste(info, collapse = "\n")
      msg <- sprintf("%s\nTROUBLESHOOTING INFORMATION:\nbatchtools::submitJobs() was called with the following 'resources' argument:\n%s", msg, info)
      msg <- sprintf("%s\nDETAILS:\nThe batchtools registry path: %s", msg, sQuote(path))
      stop(FutureLaunchError(msg, future = future))
    })
    
    if (debug) mdebugf("Launched future #%d", jobid$job.id)

    future[["state"]] <- "running"
  
    ## 6. Reserve worker for future
    registerFuture(future)

    ## 7. Trigger early signalling
    if (inherits(future, "BatchtoolsUniprocessFuture")) {
      resolved(future)
    }

    invisible(future)
  }
})


#' @importFrom batchtools killJobs
#' @importFrom future interruptFuture
#' @export
interruptFuture.BatchtoolsFutureBackend <- function(backend, future, ...) {
  job.id <- NULL ## To please R CMD check
  
  debug <- isTRUE(getOption("future.debug"))
  if (debug) {
    mdebugf_push("interruptFuture() for %s ...", class(backend)[1])
    mdebugf("Future state before: %s", sQuote(future[["state"]]))
    on.exit({
      mdebugf("Future state after: %s", sQuote(future[["state"]]))
      mdebug_pop()
    })
  }

  if (!backend[["interrupts"]]) {
    if (debug) mdebug("Interrupts are disabled for the current backend")
    return(future)
  }
  
  config <- future[["config"]]
  reg <- config[["reg"]]

  ## Does the backend support terminating jobs?
  cluster.functions <- reg[["cluster.functions"]]
  if (is.null(cluster.functions$killJob)) {
    if (debug) mdebug("Cannot interrupt, because the registered cluster functions does not define a killJob() function")
    return(future)
  }

  jobid <- config[["jobid"]]$job.id
  if (debug) mdebugf("Job ID: %s", jobid)

  res <- local({
    ## Temporarily disable batchtools output?
    ## (i.e. messages and progress bars)
    batchtools_output <- getOption("future.batchtools.output", debug)
    if (!batchtools_output) {
      oopts <- options(batchtools.verbose = FALSE, batchtools.progress = FALSE)
    } else {
      oopts <- list()
    }
    on.exit(options(oopts), add = TRUE)
    killJobs(ids = jobid, reg = reg)
  })

  if (debug) {
    mdebug("killJobs() result:")
    mprint(res)
  }

  if (nrow(res) == 0L) {
    future[["state"]] <- "interrupted"
  } else {
    res <- subset(res, job.id == jobid)
    if (nrow(res) == 1L && isTRUE(res$killed)) {
      future[["state"]] <- "interrupted"
    }
  }
  
  invisible(future)
}



#' Prints a batchtools future
#'
#' @param x An BatchtoolsFuture object
#' @param \ldots Not used.
#'
#' @export
#' @keywords internal
print.BatchtoolsFuture <- function(x, ...) {  
  NextMethod()

  backend <- x[["backend"]]
  ## batchtools specific
  config <- x$config

  conf.file <- config$conf.file
  printf("batchtools configuration file: %s\n", file_info(conf.file))
  
  reg <- config$reg
  if (inherits(reg, "Registry")) {
    cluster.functions <- reg$cluster.functions
    printf("batchtools cluster functions: %s\n",
           sQuote(cluster.functions$name))
    template <- attr(cluster.functions, "template")
    printf("batchtools cluster functions template: %s\n", file_info(template))
  } else {
    printf("batchtools cluster functions: <none>\n")
  }

  ## Ask for status once
  status <- status(x)
  printf("batchtools status: %s\n", paste(sQuote(status), collapse = ", "))
  if (any(c("finished", "error", "expired") %in% status)) {
    if ("error" %in% status) {
      lines <- loggedError(x)
      lines <- sprintf("[error] %s", lines)
      lines <- paste(lines, collapse = "\n")
      printf("Error captured by batchtools:\n%s\n", lines)
    }
    lines <- loggedOutput(x)
    lines <- sprintf("[output] %s", lines)
    lines <- paste(lines, collapse = "\n")
    printf("Output captured by batchtools:\n%s\n", lines)
  }

  if (is_na(status)) {
    printf("batchtools %s: Not found (happens when finished and deleted)\n",
           class(reg))
  } else {
    if (inherits(reg, "Registry")) {
      printf("batchtools Registry:\n")
      printf("  File dir exists: %s\n", file_test("-d", reg$file.dir))
      printf("  Work dir exists: %s\n", file_test("-d", reg$work.dir))
      try(print(reg))
    } else {
      printf("batchtools Registry: <NA>\n")
    }
  }

  invisible(x)
}


#' @importFrom batchtools getStatus
status <- function(future, ...) {
  debug <- isTRUE(getOption("future.debug"))
  if (debug) {
    mdebugf_push("status() for %s ...", class(future)[1])
    on.exit({
      mdebugf("Status: %s", paste(sQuote(status), collapse = ", "))
      mdebug_pop()
    })
  }
  
  ## WORKAROUND: Avoid warnings on partially matched arguments
  get_status <- function(...) {
    ## Temporarily disable batchtools output?
    ## (i.e. messages and progress bars)
    debug <- isTRUE(getOption("future.debug"))
    batchtools_output <- getOption("future.batchtools.output", debug)
    if (!batchtools_output) {
      oopts <- options(batchtools.verbose = FALSE, batchtools.progress = FALSE)
    } else {
      oopts <- list()
    }
    on.exit(options(oopts))
    ## WORKAROUND: batchtools::getStatus() updates the RNG state,
    ## which we must make sure to undo.
    with_stealth_rng({
      batchtools::getStatus(...)
    })
  } ## get_status()

  ## Known to be in its final state?
  if (getOption("future.batchtools.status.cache", TRUE)) {
    status <- future$.status
    if (identical(status, c("defined", "finished", "started", "submitted"))) {
      return(status)
    }
  }

  config <- future$config
  reg <- config$reg
  if (!inherits(reg, "Registry")) return(NA_character_)
  ## Closed and deleted?
  if (!file_test("-d", reg$file.dir)) return(NA_character_)

  jobid <- config$jobid
  if (is.na(jobid)) return("not submitted")
  status <- get_status(reg = reg, ids = jobid)
  status <- (unlist(status) == 1L)
  status <- status[status]
  status <- sort(names(status))
  status <- setdiff(status, c("n"))

  status[status == "done"] <- "finished"
  
  result <- future$result
  if (inherits(result, "FutureResult")) {
    if (result_has_errors(result)) status <- unique(c("error", status))
  } else if (inherits(result, "FutureError")) {
    status <- unique(c("error", status))
  }

  ## Cache result
  future$.status <- status
  
  status
}


finished <- function(future, ...) {
  status <- status(future)
  if (is_na(status)) return(NA)
  any(c("finished", "error", "expired") %in% status)
}



#' Logged output of batchtools future
#'
#' @param future The future.
#' @param \ldots Not used.
#'
#' @return A character vector or a logical scalar.
#'
#' @aliases loggedOutput loggedError
#'
#' @export loggedError
#' @export loggedOutput
#' @keywords internal
loggedOutput <- function(...) UseMethod("loggedOutput")
loggedError <- function(...) UseMethod("loggedError")


#' @importFrom batchtools getErrorMessages
#' @rdname loggedOutput
#' @export
loggedError.BatchtoolsFuture <- function(future, ...) {
  stat <- status(future)
  if (is_na(stat)) return(NULL)

  if (!finished(future)) {
    label <- sQuoteLabel(future)
    msg <- sprintf("%s ('%s') has not finished yet", class(future)[1L], label)
    stop(BatchtoolsFutureError(msg, future = future))
  }

  if (!"error" %in% stat) return(NULL)

  config <- future$config
  reg <- config$reg
  if (!inherits(reg, "Registry")) return(NULL)
  jobid <- config$jobid
  res <- getErrorMessages(reg = reg, ids = jobid)  ### CHECKED
  msg <- res$message
  msg <- paste(sQuote(msg), collapse = ", ")
  msg
} # loggedError()


batchtools_getLog <- function(id, reg, timeout = NULL) {
  debug <- isTRUE(getOption("future.debug"))
  if (debug) {
    mdebug_push("batchtools::getLog() ...")
    on.exit(mdebug_pop())
  }
  
  if (!is.null(timeout)) {
    stopifnot(length(timeout) == 1, is.numeric(timeout), !is.na(timeout), timeout >= 0.0)
    oldValue <- reg$cluster.functions$fs.latency
    on.exit({
      reg$cluster.functions$fs.latency <- oldValue
    }, add = TRUE)
    reg$cluster.functions$fs.latency <- timeout
  }
  
  tryCatch(suppressWarnings({
    getLog(id = id, reg = reg)
  }), error = function(e) NULL)
} ## batchtools_getLog()


#' @importFrom batchtools getLog
#' @export
loggedOutput.BatchtoolsFuture <- function(future, timeout = NULL, ...) {
  stat <- status(future)
  if (is_na(stat)) return(NULL)

  if (!finished(future)) {
    label <- sQuoteLabel(future)
    msg <- sprintf("%s ('%s') has not finished yet", class(future)[1L], label)
    stop(BatchtoolsFutureError(msg, future = future))
  }

  config <- future$config
  reg <- config$reg
  if (!inherits(reg, "Registry")) return(NULL)
  jobid <- config$jobid

  batchtools_getLog(id = jobid, reg = reg, timeout = timeout)
} # loggedOutput()


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Future API
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @importFrom future resolved
#' @export
#' @keywords internal
resolved.BatchtoolsFuture <- function(x, ...) {
  ## Is value already collected?
  if (!is.null(x$result)) {
    ## Signal conditions early?
    signalEarly(x, ...)
    return(TRUE)
  }

  ## Assert that the process that created the future is
  ## also the one that evaluates/resolves/queries it.
  assertOwner(x)
  
  ## If not, checks the batchtools registry status
  resolved <- finished(x)
  if (is.na(resolved)) return(FALSE)

  if (!resolved && x[["state"]] == "canceled") {
    return(TRUE)
  }

  ## Signal conditions early? (happens only iff requested)
  if (resolved) signalEarly(x, ...)

  resolved
}

#' @importFrom future result run
#' @export
#' @keywords internal
result.BatchtoolsFuture <- function(future, cleanup = TRUE, ...) {
  debug <- isTRUE(getOption("future.debug"))
  if (debug) {
    mdebugf_push("result() for %s ...", class(future)[1])
    on.exit(mdebug_pop())
  }

  ## Has the value already been collected?
  result <- future$result
  if (!is.null(result)) {
    if (debug) mdebug("FutureResult already collected")
    if (inherits(result, "FutureError")) {
      stop(result)
    }
    return(result)
  }

  if (future$state == "created") {
    future <- local({
      if (debug) {
        mdebug_push("Starting future ...")
        on.exit(mdebug_pop())
      }
      run(future)
    })
  }

  if (debug) mdebug("Getting batchtools status")
  stat <- status(future)
  if (is_na(stat)) {
    label <- sQuoteLabel(future)
    stopf("The result no longer exists (or never existed) for Future ('%s') of class %s", label, paste(sQuote(class(future)), collapse = ", ")) #nolint
  }

  result <- local({
    res <- NULL
    if (debug) {
      mdebug_push("Waiting for batchtools job to finish ...")
      on.exit({
        mdebugf("Result: <%s>", class(res)[1])
        mdebug_pop()
      })
    }
    res <- await(future, cleanup = FALSE)
    res
  })
  stop_if_not(inherits(result, c("FutureResult", "FutureError")))
  future[["result"]] <- result
  if (inherits(result, "FutureInterruptError")) {
    future[["state"]] <- "interrupted"
  } else {
    future[["state"]] <- "finished"
  }

  if (cleanup) {
    local({
      if (debug) {
        mdebugf_push("Delete %s ...", class(future)[1])
        on.exit(mdebug_pop())
      }
      delete(future)
    })
  }

  if (inherits(result, "FutureError")) {
    stop(result)
  }

  result
} ## result()


#' @importFrom future FutureInterruptError FutureLaunchError FutureWarning
#' @importFrom batchtools loadResult waitForJobs
#' @importFrom utils tail
await <- function(future, cleanup = TRUE, ...) {
  backend <- future[["backend"]]
  timeout <- backend[["future.wait.timeout"]]
  delta <- backend[["future.wait.interval"]]
  alpha <- backend[["future.wait.alpha"]]
  
  debug <- isTRUE(getOption("future.debug"))
  if (debug) {
    mdebug_push("future.batchtools:::await() ...")
    on.exit(mdebug_pop())
  }

  expr <- future$expr
  config <- future$config
  reg <- config$reg
  stop_if_not(inherits(reg, "Registry"))
  jobid <- config$jobid

  if (debug) mdebug_push("batchtools::waitForJobs() ...")

  ## Control batchtools info output
  oopts <- options(batchtools.verbose = debug)
  on.exit(options(oopts), add = TRUE)

  ## Sleep function - increases geometrically as a function of iterations
  sleep_fcn <- function(i) delta * alpha ^ (i - 1)
 
  if (debug) mdebugf_push("batchtools::waitForJobs(..., timeout = %s) ...", timeout)
  res <- waitForJobs(ids = jobid, timeout = timeout, sleep = sleep_fcn,
                     stop.on.error = FALSE, reg = reg)
  if (!isTRUE(res)) {
    warning(FutureWarning(sprintf("batchtools::waitForJobs(..., timeout = %s) returned FALSE", timeout), future = future))
  }
  if (debug) {
    mdebugf("Result: %s", res)
    mdebugf_pop()
  }
  
  stat <- status(future)
  if (debug) {
    mdebugf("status(): %s", paste(sQuote(stat), collapse = ", "))
    mdebug_pop()
  }

  finished <- is_na(stat) || any(c("finished", "error", "expired") %in% stat)

  ## PROTOTYPE RESULTS BELOW:
  prototype_fields <- NULL
  
  result <- NULL
  if (finished) {
    if (debug) mdebug("Results:")
    label <- sQuoteLabel(future)
    if ("finished" %in% stat) {
      result <- local({
        if (debug) {
          mdebug_push("batchtools::loadResult() ...")
          on.exit(mdebug_pop())
        }
        loadResult(reg = reg, id = jobid)
      })
      
      if (inherits(result, "FutureResult")) {
        prototype_fields <- c(prototype_fields, "batchtools_log")
        ## Since we're already collected the results, the log file
        ## should already exist, if it exists.  Because of this,
        ## only poll for the log file for a second before giving up.
        result[["batchtools_log"]] <- batchtools_getLog(id = jobid, reg = reg, timeout = 0.0)
        if (result_has_errors(result)) cleanup <- FALSE
      }
    } else if ("error" %in% stat) {
      cleanup <- FALSE
      msg <- sprintf(
              "BatchtoolsFutureError for %s ('%s') captured by batchtools: %s",
              class(future)[1], label, loggedError(future))
      stop(BatchtoolsFutureError(msg, future = future))
    } else if ("expired" %in% stat) {
      ## NOTE: If a batchtools job crashes or is killed, then it gets status
      ## 'expired'. In such cases, we should throw a FutureInterruptError.
      ##
      ## I think we might also see 'expired' for jobs that fail to launch,
      ## which in case we should throw a FutureLaunchError. I'm not sure
      ## how we can distinguish the two right now, but I'll assume that
      ## started jobs have a 'submitted' or 'started' status flag too,
      ## whereas jobs that failed to launch won't. /HB 2025-07-15
      hints <- NULL
      
      state <- future[["state"]]
      info <- sprintf("Future state: %s", sQuote(state))
      hints <- c(hints, info)
      info <- sprintf("Batchtools status: %s", commaq(stat))
      hints <- c(hints, info)

      ## SPECIAL CASE: Some Slurm users report on 'expired' jobs, although they never started.
      ## Output more breadcrumbs to be able to narrow in on what causes this. /HB 2025-09-07
      if (inherits(future, "BatchtoolsSlurmFuture")) {
        ## Get _all_ jobs of the users, including those not submitted via future.batchtools
        slurm_job_ids <- unique(c(
          reg$cluster.functions$listJobsQueued(reg),
          reg$cluster.functions$listJobsRunning(reg)
        ))
        if (length(slurm_job_ids) > 0) {
          info <- sprintf("Slurm job ID: [n=%d] %s", length(slurm_job_ids), commaq(slurm_job_ids))
          args <- c("--noheader", "--format='job_id=%i,state=%T,submitted_on=%V,time_used=%M'", "-j", paste(slurm_job_ids, collapse = ","))
          res <- system2("squeue", args = args, stdout = TRUE, stderr = TRUE)
          res <- paste(res, collapse = "; ") ## should only be a single line, but ...
          info <- c(info, sprintf("Slurm job status: %s", res))
	} else {
          info <- "Slurm job ID: <not found>"
          info <- c(info, sprintf("Slurm job status: <unknown>"))
        }
	hints <- c(hints, info)
      }

      ## TROUBLESHOOTING: Logged output
      info <- tryCatch({
        output <- loggedOutput(future, timeout = 0.0)
        info <- unlist(strsplit(output, split = "\n", fixed = TRUE))
        info <- info[nzchar(info)]
        info <- tail(info, n = getOption("future.batchtools.expiration.tail", 48L))
      }, error = function(e) NULL)

      if (length(info) > 0) {
        info <- c("The last few lines of the logged output:", info)
      } else {
        info <- "No logged output file exist (at the moment)"
      }
      hints <- c(hints, info)

      if (length(hints) > 0) {
        hints <- c("\nPost-mortem details:", hints)
        hints <- paste(hints, collapse = "\n")
      }	
      if (any(c("submitted", "started") %in% stat)) {
        msg <- sprintf("Future (%s) of class %s expired, which indicates that it crashed or was killed.%s", label, class(future)[1], hints)
        result <- FutureInterruptError(msg, future = future)
      } else {
        msg <- sprintf("Future (%s) of class %s failed to launch.%s", label, class(future)[1], hints)
        result <- FutureLaunchError(msg, future = future)
      }
    } else if (future[["state"]] %in% c("canceled", "interrupted")) {
      label <- sQuoteLabel(future)
      msg <- sprintf("Future (%s) of class %s was %s", label, class(future)[1], future[["state"]])
      result <- FutureInterruptError(msg, future = future)
    } else if (is_na(stat)) {
      msg <- sprintf("BatchtoolsDeleted: Cannot retrieve value. Future ('%s') deleted: %s", label, reg$file.dir) #nolint
      stop(BatchtoolsFutureError(msg, future = future))
    }
    if (debug) { mstr(result) }
  } else if (future[["state"]] %in% c("canceled", "interrupted")) {
    label <- sQuoteLabel(future)
    msg <- sprintf("Future (%s) of class %s was %s", label, class(future)[1], future[["state"]])
    result <- FutureInterruptError(msg, future = future)
  } else {
    label <- sQuoteLabel(future)
    cleanup <- FALSE
    msg <- sprintf("AsyncNotReadyError: Polled for results for %s seconds every %g seconds, but asynchronous evaluation for future ('%s') is still running: %s", timeout, delta, label, reg$file.dir) #nolint
    stop(BatchtoolsFutureError(msg, future = future))
  }

  if (length(prototype_fields) > 0) {
    result$PROTOTYPE_WARNING <- sprintf("WARNING: The fields %s should be considered internal and experimental for now, that is, until the Future API for these additional features has been settled. For more information, please see https://github.com/futureverse/future/issues/172", hpaste(sQuote(prototype_fields), max_head = Inf, collapse = ", ", last_collapse  = " and "))
  }
  
  ## Cleanup?
  if (cleanup) {
    delete(future, delta = 0.5 * delta, ...)
  }

  result
} # await()


delete <- function(...) UseMethod("delete")

#' Removes a batchtools future
#'
#' @param future The future.
#' @param onRunning Action if future is running or appears to run.
#' @param onFailure Action if failing to delete future.
#' @param onMissing Action if future does not exist.
#' @param times The number of tries before giving up.
#' @param \ldots Not used.
#'
#' @return (invisibly) TRUE if deleted and FALSE otherwise.
#'
#' @export
#' @importFrom batchtools clearRegistry removeRegistry
#' @importFrom utils file_test
#' @keywords internal
delete.BatchtoolsFuture <- function(future,
                                onRunning = c("warning", "error", "skip"),
                                onFailure = c("error", "warning", "ignore"),
                                onMissing = c("ignore", "warning", "error"),
                                times = 10L,
                                ...) {
  onRunning <- match.arg(onRunning)
  onMissing <- match.arg(onMissing)
  onFailure <- match.arg(onFailure)

  debug <- isTRUE(getOption("future.debug"))
  if (debug) {
    mdebugf_push("delete() for %s ...", class(future)[1])
    on.exit(mdebugf_pop())
  }

  backend <- future[["backend"]]
  delta <- backend[["future.wait.interval"]]
  alpha <- backend[["future.wait.alpha"]]

  ## Identify registry
  config <- future$config
  reg <- config$reg
  
  ## Trying to delete a non-launched batchtools future?
  if (!inherits(reg, "Registry")) return(invisible(TRUE))
  
  path <- reg$file.dir

  ## Already deleted?
  if (is.null(path) || !file_test("-d", path)) {
    if (onMissing %in% c("warning", "error")) {
      msg <- sprintf("Cannot remove batchtools registry, because directory does not exist: %s", sQuote(path)) #nolint
      if (debug) mdebugf("delete(): %s", msg)
      if (onMissing == "warning") {
        warning(msg)
      } else if (onMissing == "error") {
        stop(BatchtoolsFutureError(msg, future = future))
      }
    }
    return(invisible(TRUE))
  }


  ## Is the future still not resolved? If so, then...
  if (!resolved(future)) {
    if (onRunning == "skip") return(invisible(TRUE))
    status <- status(future)
    label <- sQuoteLabel(future)
    msg <- sprintf("Will not remove batchtools registry, because is appears to hold a non-resolved future (%s; state = %s; batchtools status = %s): %s", label, sQuote(future$state), paste(sQuote(status), collapse = ", "), sQuote(path)) #nolint
    if (debug) mdebugf("delete(): %s", msg)
    if (onRunning == "warning") {
      warning(msg)
      return(invisible(TRUE))
    } else if (onRunning == "error") {
      stop(BatchtoolsFutureError(msg, future = future))
    }
  }

  ## Make sure to collect the results before deleting
  ## the internal batchtools registry
  result <- future[["result"]]
  if (is.null(result)) {
    result <- result(future, cleanup = FALSE)
  }

  ## Free up worker
  unregisterFuture(future)

  ## To simplify post mortem troubleshooting in non-interactive sessions,
  ## should the batchtools registry files be removed or not?
  delete <- future[["delete"]]
  if (debug) {
    mdebugf("delete(): Future backend argument 'delete' is %s", sQuote(delete))
  }
  
  if (delete != "never") {
    status <- status(future)
    res <- future$result
    if (inherits(res, "FutureResult")) {
      if (result_has_errors(res)) status <- unique(c("error", status))
    }
    if (debug) {
      mdebugf("delete(): status(<future>) = %s",
              paste(sQuote(status), collapse = ", "))
    }
    if (any(c("error", "expired") %in% status)) {
      if (delete == "on-success") {
        msg <- sprintf("Will not remove batchtools registry, because the status of the batchtools was %s and future backend argument 'delete' is %s: %s", paste(sQuote(status), collapse = ", "), sQuote(delete), sQuote(path)) #nolint
        if (debug) mdebugf("delete(): %s", msg)
        warning(msg)
        return(invisible(FALSE))
      }
    }
  }

  ## Have user disabled deletions?
  if (delete == "never") {
    msg <- sprintf("Future backend argument 'delete' is %s - will not delete batchtools registry: %s", sQuote(delete), sQuote(path))
    if (debug) mdebugf("delete(): %s", msg)
    return(invisible(FALSE))
  }

  ## Control batchtools info output
  oopts <- options(batchtools.verbose = debug)
  on.exit(options(oopts))

  ## Try to delete registry
  ## WORKAROUND: batchtools::clearRegistry() and
  ## batchtools::removeRegistry() update the RNG state,
  ## which we must make sure to undo.
  with_stealth_rng({
    interval <- delta
    for (kk in seq_len(times)) {
      try(unlink(path, recursive = TRUE), silent = FALSE)
      if (!file_test("-d", path)) break
      try(removeRegistry(wait = 0.0, reg = reg), silent = FALSE)
      if (!file_test("-d", path)) break
      try(clearRegistry(reg = reg), silent = TRUE)
      try(removeRegistry(wait = 0.0, reg = reg), silent = FALSE)
      if (!file_test("-d", path)) break
      Sys.sleep(interval)
      interval <- alpha * interval
    }
  })

  ## Success?
  if (file_test("-d", path)) {
    if (onFailure %in% c("warning", "error")) {
      msg <- sprintf("Failed to remove batchtools registry: %s", sQuote(path))
      if (debug) mdebugf("delete(): %s", msg)
      if (onMissing == "warning") {
        warning(msg)
      } else if (onMissing == "error") {
        stop(BatchtoolsFutureError(msg, future = future))
      }
    }
    return(invisible(FALSE))
  }

  if (debug) mdebugf("delete(): batchtools registry deleted: %s", sQuote(path))

  invisible(TRUE)
} # delete()


add_finalizer <- function(...) UseMethod("add_finalizer")

#' @export
add_finalizer.BatchtoolsFuture <- function(future, debug = FALSE, ...) {
  ## Register finalizer (will clean up registries etc.)

  if (debug) {
    mdebugf_push("add_finalizer() for %s ...", sQuote(class(future)[1]))
    on.exit(mdebug_pop())
  }

  reg.finalizer(future, f = function(f) {
    if (debug) {
      if (!exists("mdebug", mode = "function")) mdebug <- message
      mdebug("Finalize ", sQuote(class(f)[1]), " ...")
      on.exit(mdebug("Finalize ", sQuote(class(f)[1]), " ... done"), add = TRUE)
    }
    if (inherits(f, "BatchtoolsFuture") && "future.batchtools" %in% loadedNamespaces()) {
      if (debug) {
        mdebug("- attempting to delete future")
        if (requireNamespace("utils", quietly = TRUE)) {
          mdebug(utils::capture.output(utils::str(as.list(f))))
        }
      }
      res <- try({
        delete(f, onRunning = "skip", onMissing = "ignore", onFailure = "warning")
      })
      if (debug) {
        if (inherits(res, "try-error")) {
          mdebug("- Failed to delete: ", sQuote(res))
        } else {
          mdebug("- deleted: ", res)
        }
      }
    }
  }, onexit = TRUE)

  invisible(future)
}


#' @importFrom future cancel stopWorkers
#' @export
stopWorkers.BatchtoolsFutureBackend <- function(backend, ...) {
  debug <- isTRUE(getOption("future.debug"))
  if (debug) {
    mdebugf_push("stopWorkers() for %s ...", class(backend)[1])
    on.exit(mdebugf_pop())
  }
  
  futures <- FutureRegistry(backend[["reg"]], action = "list", earlySignal = FALSE)
  
  ## Nothing to do?
  if (length(futures) == 0L) return(backend)

  ## Enable interrupts temporarily, if disabled
  if (!isTRUE(backend[["interrupts"]])) {
    backend[["interrupts"]] <- TRUE
    on.exit({ backend[["interrupts"]] <- FALSE }, add = TRUE)
  }

  ## Cancel and interrupt all futures, which terminates the workers
  futures <- lapply(futures, FUN = cancel, interrupt = TRUE)

  ## Erase registry
  futures <- FutureRegistry(backend[["reg"]], action = "reset")

  backend
}
