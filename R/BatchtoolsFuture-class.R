#' A batchtools future is a future whose value will be resolved via batchtools
#'
#' @param expr The R expression to be evaluated
#'
#' @param envir The environment in which global environment
#' should be located.
#'
#' @param substitute Controls whether `expr` should be
#' `substitute()`:d or not.
#'
#' @param globals (optional) a logical, a character vector, a named list, or a
#' [Globals][globals::Globals] object.  If TRUE, globals are identified by code
#' inspection based on `expr` and `tweak` searching from environment
#' `envir`.  If FALSE, no globals are used.  If a character vector, then
#' globals are identified by lookup based their names `globals` searching
#' from environment `envir`.  If a named list or a Globals object, the
#' globals are used as is.
#'
#' @param label (optional) Label of the future (where applicable, becomes the
#' job name for most job schedulers).
#'
#' @param resources (optional) A named list passed to the \pkg{batchtools}
#' template (available as variable `resources`).  See Section 'Resources'
#' in [batchtools::submitJobs()] more details.
#'
#' @param workers (optional) The maximum number of workers the batchtools
#' backend may use at any time.   Interactive and "local" backends can only
#' process one future at the time (`workers = 1L`), whereas HPC backends,
#' where futures are resolved via separate jobs on a scheduler, can have
#' multiple workers.  In the latter, the default is `workers = NULL`, which
#' will resolve to
#' \code{getOption("\link{future.batchtools.workers}")}.
#' If neither are specified, then the default is `100`.
#'
#' @param finalize If TRUE, any underlying registries are
#' deleted when this object is garbage collected, otherwise not.
#'
#' @param conf.file (optional) A batchtools configuration file.
#'
#' @param cluster.functions (optional) A batchtools
#' [ClusterFunctions][batchtools::ClusterFunctions] object.
#'
#' @param registry (optional) A named list of settings to control the setup
#' of the batchtools registry.
#'
#' @param \ldots Additional arguments passed to [future::Future()].
#'
#' @return A BatchtoolsFuture object
#'
#' @export
#' @importFrom future Future getGlobalsAndPackages
#' @keywords internal
BatchtoolsFuture <- function(expr = NULL, envir = parent.frame(),
                             substitute = TRUE,
                             globals = TRUE, packages = NULL,
                             label = NULL,
                             resources = list(),
                             workers = NULL,
                             finalize = getOption("future.finalize", TRUE),
                             conf.file = findConfFile(),
                             cluster.functions = NULL,
                             registry = list(),
                             ...) {
  if (substitute) expr <- substitute(expr)
  assert_no_positional_args_but_first()

  ## Record globals
  gp <- getGlobalsAndPackages(expr, envir = envir, globals = globals)

  future <- Future(expr = gp$expr, envir = envir, substitute = FALSE,
                   globals = gp$globals,
                   packages = unique(c(packages, gp$packages)),
                   label = label,
                   ...)

  future <- as_BatchtoolsFuture(future,
                                resources = resources,
                                workers = workers,
                                finalize = finalize,
                                conf.file = conf.file,
                                cluster.functions = cluster.functions,
                                registry = registry)

  future
}


## Helper function to create a BatchtoolsFuture from a vanilla Future
#' @importFrom utils file_test
as_BatchtoolsFuture <- function(future,
                                resources = list(),
                                workers = NULL,
                                finalize = getOption("future.finalize", TRUE),
                                conf.file = findConfFile(),
                                cluster.functions = NULL,
                                registry = list(),
                                ...) {
  if (is.function(workers)) workers <- workers()
  if (is.null(workers)) {
    workers <- getOption("future.batchtools.workers", default = 100)
    stop_if_not(
      is.numeric(workers),
      length(workers) == 1,
      !is.na(workers), workers >= 1
    )
  } else {
    stop_if_not(length(workers) >= 1)
    if (is.numeric(workers)) {
      stop_if_not(length(workers) == 1, !is.na(workers), workers >= 1)
    } else if (is.character(workers)) {
      stop_if_not(length(workers) >= 0, !anyNA(workers))
    } else {
      stop("Argument 'workers' should be either a numeric or a function: ",
           mode(workers))
    }
  }
  future$workers <- workers

  if (!is.null(cluster.functions)) {
    stop_if_not(is.list(cluster.functions))
    stop_if_not(inherits(cluster.functions, "ClusterFunctions"))
  } else if (missing(conf.file)) {
    ## BACKWARD COMPATILITY: Only when calling BatchtoolsFuture() directly
    cluster.functions <- makeClusterFunctionsInteractive(external = FALSE)
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

  ## batchtools configuration
  future$config <- list(
    reg = NULL,
    jobid = NA_integer_,
    resources = resources,
    conf.file = conf.file,
    cluster.functions = cluster.functions,
    registry = registry,
    finalize = finalize
  )

  structure(future, class = c("BatchtoolsFuture", class(future)))
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
  if ("error" %in% status) {
    printf("Error captured by batchtools: %s\n", loggedError(x))
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


#' @importFrom future cancel
#' @export
cancel.BatchtoolsFuture <- function(x, interrupt = FALSE, ...) {
  ## FIXME
  NextMethod(interrupt = interrupt)
}


#' @importFrom batchtools getStatus
status <- function(future, ...) {
  debug <- isTRUE(getOption("future.debug"))
  if (debug) {
    mdebug("status() for ", class(future)[1], " ...")
    on.exit(mdebug("status() for ", class(future)[1], " ... done"), add = TRUE)
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
  }

  ## Cache result
  future$.status <- status
  
  if (debug) mdebug("Status: ", paste(sQuote(status), collapse = ", "))

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


#' @importFrom batchtools getLog
#' @export
loggedOutput.BatchtoolsFuture <- function(future, ...) {
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
  getLog(id = jobid, reg = reg)
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
  assertOwner <- import_future("assertOwner")
  assertOwner(x)

  ## If not, checks the batchtools registry status
  resolved <- finished(x)
  if (is.na(resolved)) return(FALSE)

  ## Signal conditions early? (happens only iff requested)
  if (resolved) signalEarly(x, ...)

  resolved
}

#' @importFrom future result
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
  if (inherits(result, "FutureResult")) {
    if (debug) mdebug("FutureResult already collected")
    return(result)
  }

  ## Has the value already been collected? - take two
  if (future$state %in% c("finished", "failed", "interrupted")) {
    if (debug) mdebug("FutureResult already collected - take 2")
    return(NextMethod())
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
    if (debug) {
      mdebug_push("Waiting for batchtools job to finish ...")
      on.exit(mdebug_pop())
    }
    await(future, cleanup = FALSE)
  })
  stop_if_not(inherits(result, "FutureResult"))
  future$result <- result
  future$state <- "finished"

  if (cleanup) {
    local({
      if (debug) {
        mdebugf_push("Delete %s ...", class(future)[1])
        on.exit(mdebug_pop())
      }
      delete(future)
    })
  }

  if (debug) mdebug("NextMethod()")
  NextMethod()
}


#' @importFrom future run getExpression
#' @importFrom batchtools batchExport batchMap saveRegistry setJobNames submitJobs
#' @importFrom utils capture.output str
#' @export
run.BatchtoolsFuture <- function(future, ...) {
  debug <- isTRUE(getOption("future.debug"))
  if (debug) {
    mdebugf_push("run() for %s ...", class(future)[1])
    on.exit(mdebug_pop())
  }
  
  if (future$state != "created") {
    label <- sQuoteLabel(future)
    msg <- sprintf("A future ('%s') can only be launched once.", label)
    stop(FutureError(msg, future = future))
  }

  ## Assert that the process that created the future is
  ## also the one that evaluates/resolves/queries it.
  assertOwner <- import_future("assertOwner")
  assertOwner(future)

  ## Temporarily disable batchtools output?
  ## (i.e. messages and progress bars)
  batchtools_output <- getOption("future.batchtools.output", debug)
  if (!batchtools_output) {
    oopts <- options(batchtools.verbose = FALSE, batchtools.progress = FALSE)
  } else {
    oopts <- list()
  }
  on.exit(options(oopts))

  expr <- getExpression(future)

  ## Always evaluate in local environment
  expr <- substitute(local(expr), list(expr = expr))

  ## (i) Create batchtools registry
  reg <- future$config$reg
  stop_if_not(is.null(reg) || inherits(reg, "Registry"))
  if (is.null(reg)) {
    if (debug) mdebug("Creating batchtools registry")
    config <- future$config
    stop_if_not(is.list(config))
    
    ## Create batchtools registry
    reg <- temp_registry(
      label             = future$label,
      conf.file         = config$conf.file,
      cluster.functions = config$cluster.functions,
      config            = config$registry
    )
    if (debug) mprint(reg)
    future$config$reg <- reg

    ## Register finalizer?
    if (config$finalize) future <- add_finalizer(future)
    
    config <- NULL
  }
  stop_if_not(inherits(reg, "Registry"))

  ## (ii) Attach packages that needs to be attached
  packages <- future$packages
  if (length(packages) > 0) {
    mdebugf_push("Attaching %d packages (%s) ...",
                    length(packages), hpaste(sQuote(packages)))

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

    mdebug_pop()
  }
  ## Not needed anymore
  packages <- NULL

  ## (iii) Export globals?
  if (length(future$globals) > 0) {
    batchExport(export = future$globals, reg = reg)
  }

  ## 1. Add to batchtools for evaluation
  if (debug) mdebug("batchtools::batchMap()")
  ## WORKAROUND: batchtools::batchMap() updates the RNG state,
  ## which we must make sure to undo.
  with_stealth_rng({
    jobid <- batchMap(fun = geval, list(expr),
                      more.args = list(substitute = TRUE), reg = reg)
  })

  ## 2. Set job name, if specified
  label <- future$label
  if (!is.null(label)) {
    setJobNames(ids = jobid, names = label, reg = reg)
  }
  
  ## 3. Update
  future$config$jobid <- jobid
  if (debug) mdebugf("Created %s future #%d", class(future)[1], jobid$job.id)

  ## WORKAROUND: (For multicore and macOS only)
  if (reg$cluster.functions$name == "Multicore") {
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
  }

  ## 4. Wait for an available worker
  waitForWorker(future, workers = future$workers)

  ## 5. Submit
  future$state <- "running"
  resources <- future$config$resources
  if (is.null(resources)) resources <- list()

  ## WORKAROUND: batchtools::submitJobs() updates the RNG state,
  ## which we must make sure to undo.
  tryCatch({
    with_stealth_rng({
      submitJobs(reg = reg, ids = jobid, resources = resources)
    })
  }, error = function(ex) {
    msg <- conditionMessage(ex)
    label <- sQuoteLabel(future)
    msg <- sprintf("Failed to submit %s (%s). The reason was: %s", class(future)[1], label, msg)
    info <- capture.output(str(resources))
    info <- paste(info, collapse = "\n")
    msg <- sprintf("%s\nTROUBLESHOOTING INFORMATION:\nbatchtools::submitJobs() was called with the following 'resources' argument:\n%s\n", msg, info)
    stop(BatchtoolsFutureError(msg, future = future))
  })

  mdebugf("Launched future #%d", jobid$job.id)

  ## 6. Rerserve worker for future
  registerFuture(future)

  ## 7. Trigger early signalling
  if (inherits(future, "BatchtoolsUniprocessFuture")) {
    resolved(future)
  }
  
  invisible(future)
} ## run()


#' @importFrom batchtools loadResult waitForJobs
#' @importFrom utils tail
await <- function(future, cleanup = TRUE,
                  timeout = getOption("future.wait.timeout", 30 * 24 * 60 * 60),
                  delta = getOption("future.wait.interval", 1.0),
                  alpha = getOption("future.wait.alpha", 1.01),
                  ...) {
  stop_if_not(is.finite(timeout), timeout >= 0)
  stop_if_not(is.finite(alpha), alpha > 0)
  
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
 
  res <- waitForJobs(ids = jobid, timeout = timeout, sleep = sleep_fcn,
                     stop.on.error = FALSE, reg = reg)
  if (debug) mdebugf("batchtools::waitForJobs(): %s", res)
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
        result[["batchtools_log"]] <- try(local({
          if (debug) {
            mdebug_push("batchtools::getLog() ...")
            on.exit(mdebug_pop())
          }
	  ## Since we're already collected the results, the log file
	  ## should already exist, if it exists.  Because of this,
	  ## only poll for the log file for a second before giving up.
	  reg$cluster.functions$fs.latency <- 1.0
          getLog(id = jobid, reg = reg)
        }), silent = TRUE)
        if (result_has_errors(result)) cleanup <- FALSE
      }
    } else if ("error" %in% stat) {
      cleanup <- FALSE
      msg <- sprintf(
              "BatchtoolsFutureError for %s ('%s') captured by batchtools: %s",
              class(future)[1], label, loggedError(future))
      stop(BatchtoolsFutureError(msg, future = future))
    } else if ("expired" %in% stat) {
      cleanup <- FALSE
      msg <- sprintf("BatchtoolsExpiration: Future ('%s') expired (registry path %s).", label, reg$file.dir)
      output <- loggedOutput(future)
      hint <- unlist(strsplit(output, split = "\n", fixed = TRUE))
      hint <- hint[nzchar(hint)]
      hint <- tail(hint, n = getOption("future.batchtools.expiration.tail", 48L))
      if (length(hint) > 0) {
        hint <- paste(hint, collapse = "\n")
        msg <- paste(msg, ". The last few lines of the logged output:\n",
	             hint, sep="")
      } else {
        msg <- sprintf("%s. No logged output exist.", msg)
      }
      stop(BatchtoolsFutureError(msg, future = future))
    } else if (is_na(stat)) {
      msg <- sprintf("BatchtoolsDeleted: Cannot retrieve value. Future ('%s') deleted: %s", label, reg$file.dir) #nolint
      stop(BatchtoolsFutureError(msg, future = future))
    }
    if (debug) { mstr(result) }    
  } else {
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
#' @param delta The delay interval (in seconds) between retries.
#' @param alpha A multiplicative penalty increasing the delay
#' for each failed try.
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
                                delta = getOption("future.wait.interval", 1.0),
                                alpha = getOption("future.wait.alpha", 1.01),
                                ...) {
  onRunning <- match.arg(onRunning)
  onMissing <- match.arg(onMissing)
  onFailure <- match.arg(onFailure)

  debug <- isTRUE(getOption("future.debug"))

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


  ## Is the future still not resolved?  If so, then...
  if (!resolved(future)) {
    if (onRunning == "skip") return(invisible(TRUE))
    status <- status(future)
    label <- sQuoteLabel(future)
    msg <- sprintf("Will not remove batchtools registry, because is appears to hold a non-resolved future (%s; state = %s; batchtools status = %s): %s", sQuote(label), sQuote(future$state), paste(sQuote(status), collapse = ", "), sQuote(path)) #nolint
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
  result <- result(future, cleanup = FALSE)
  stop_if_not(inherits(result, "FutureResult"))

  ## Free up worker
  unregisterFuture(future)

  ## To simplify post mortem troubleshooting in non-interactive sessions,
  ## should the batchtools registry files be removed or not?
  if (debug) {
    mdebugf("delete(): Option 'future.delete = %s",
            sQuote(getOption("future.delete", "<NULL>")))
  }
  if (!getOption("future.delete", interactive())) {
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
      msg <- sprintf("Will not remove batchtools registry, because the status of the batchtools was %s and option 'future.delete' is FALSE or running in an interactive session: %s", paste(sQuote(status), collapse = ", "), sQuote(path)) #nolint
      if (debug) mdebugf("delete(): %s", msg)
      warning(msg)
      return(invisible(FALSE))
    }
  }

  ## Have user disabled deletions?
  if (!getOption("future.delete", TRUE)) {
    msg <- sprintf("Option 'future.delete' is FALSE - will not delete batchtools registry: %s", sQuote(path))
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
