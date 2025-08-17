#' @inheritParams BatchtoolsTemplateFutureBackend
#' @inheritParams batchtools::makeClusterFunctions
#'
#' @rdname BatchtoolsFutureBackend
#' @keywords internal
#'
#' @export
BatchtoolsBashFutureBackend <- function(...,
    cluster.functions = makeClusterFunctionsBash(template = template, fs.latency = fs.latency),
    fs.latency = 0.0,
    template = "bash") {
  assert_no_positional_args_but_first()

  args <- list(...)
  if ("workers" %in% names(args)) {
    stop("Unknown argument 'workers'")
  }
  
  core <- BatchtoolsUniprocessFutureBackend(
    ...,
    cluster.functions = cluster.functions,
    template = template
  )

  core[["futureClasses"]] <- c("BatchtoolsBashFuture", core[["futureClasses"]])
  core <- structure(core, class = c("BatchtoolsBashFutureBackend", class(core)))
  core
}


#' A batchtools bash backend that resolves futures sequentially via a Bash template script
#'
#' The `batchtools_bash` backend was added to illustrate how to write a
#' custom \pkg{future.batchtools} backend that uses a templated job script.
#' Please see the source code, for details.
#'
#' @inheritParams BatchtoolsFutureBackend
#' @inheritParams BatchtoolsTemplateFutureBackend
#' @inheritParams BatchtoolsBashFutureBackend
#'
#' @param template (optional) Name of job-script template to be searched
#' for by [batchtools::findTemplateFile()]. If not found, it defaults to
#' the `templates/bash.tmpl` part of this package (see below).
#'
#' @param \ldots Not used.
#'
#' @details
#' Batchtools bash futures use \pkg{batchtools} cluster functions
#' created by [makeClusterFunctionsBash()] and requires that `bash` is
#' installed on the current machine and the `timeout` command is available.
#'
#' The default template script `templates/bash.tmpl` can be found in:
#'
#' ```r
#' system.file("templates", "bash.tmpl", package = "future.batchtools")
#' ```
#'
#' and comprise:
#'
#' `r paste(c("\x60\x60\x60bash", readLines("inst/templates/bash.tmpl"), "\x60\x60\x60"), collapse = "\n")`
#'
#' @examplesIf interactive()
#' library(future)
#'
#' # Limit runtime to 30 seconds per future
#' plan(future.batchtools::batchtools_bash, resources = list(runtime = 30))
#'
#' message("Main process ID: ", Sys.getpid())
#'
#' f <- future(Sys.getpid())
#' pid <- value(f)
#' message("Worker process ID: ", pid)
#' 
#' @export
batchtools_bash <- function(
  ...,
  cluster.functions = makeClusterFunctionsBash(template = "bash", fs.latency = 0.0),
  fs.latency = 0.0,
  template = "bash",
  registry = list(),
  conf.file = findConfFile(),
  resources = list(),
  finalize = getOption("future.finalize", TRUE)) {
 stop("INTERNAL ERROR: The future.batchtools::batchtools_bash() must never be called directly")
}
class(batchtools_bash) <- c(
  "batchtools_bash", "batchtools_custom",
  "batchtools_uniprocess", "batchtools",
  "uniprocess", "future", "function"
)
attr(batchtools_bash, "tweakable") <- c("finalize")
attr(batchtools_bash, "untweakable") <- c("workers")
attr(batchtools_bash, "init") <- TRUE
attr(batchtools_bash, "factory") <- BatchtoolsBashFutureBackend


#' @inheritParams batchtools::makeClusterFunctions
#'
#' @return
#' `makeClusterFunctionsBash()` returns a
#' \link[batchtools:makeClusterFunctions]{ClusterFunctions} object.
#'
#' @rdname batchtools_bash
#'
#' @importFrom batchtools cfReadBrewTemplate cfBrewTemplate makeClusterFunctions makeSubmitJobResult
#' @importFrom utils file_test
#' @export
makeClusterFunctionsBash <- function(template = "bash", fs.latency = 0.0) {
  bin <- Sys.which("bash")
  stop_if_not(file_test("-f", bin), file_test("-x", bin))
  
  template <- find_template_file(template)
  template_text <- cfReadBrewTemplate(template)

  submitJob <- function(reg, jc) {
    stop_if_not(inherits(reg, "Registry"))
    stop_if_not(inherits(jc, "JobCollection"))

    script <- cfBrewTemplate(reg, text = template_text, jc = jc)
    output <- system2(bin, args = c(script), stdout = TRUE, stderr = TRUE, wait = TRUE)
    debug <- isTRUE(getOption("future.debug"))
    if (debug) {
      mdebug_push("makeClusterFunctionsBash() ...")
      mdebug(paste(c(output, ""), collapse = "\n"))
      on.exit(mdebug_pop())
    }
    
    status <- attr(output, "status")
    if (is.null(status)) {
      status <- 0L
      batch.id <- sprintf("bash#%d", Sys.getpid())
    } else {
      batch.id <- NA_character_
    }

    makeSubmitJobResult(status = status, batch.id = batch.id)
  }

  cf <- makeClusterFunctions(
    name = "Bash",
    submitJob = submitJob,
    store.job.collection = TRUE,
    fs.latency = fs.latency
  )
  attr(cf, "template") <- template
  attr(cf, "template_text") <- template_text
  cf
}
