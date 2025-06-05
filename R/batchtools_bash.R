#' @inheritParams batchtools_custom
#' @inheritParams batchtools_template
#'
#' @export
batchtools_bash <- function(..., envir = parent.frame(), template = "bash") {
  cf <- makeClusterFunctionsBash(template = template)
  future <- BatchtoolsBashFuture(..., envir = envir, cluster.functions = cf)
  if (!future$lazy) future <- run(future)
  invisible(future)
}
class(batchtools_bash) <- c(
  "batchtools_bash", "batchtools_custom",
  "batchtools_uniprocess", "batchtools",
  "uniprocess", "future", "function"
)
attr(batchtools_bash, "tweakable") <- c("finalize")
attr(batchtools_bash, "untweakable") <- c("workers")


#' @importFrom batchtools cfReadBrewTemplate cfBrewTemplate makeClusterFunctions makeSubmitJobResult
#' @importFrom utils file_test
makeClusterFunctionsBash <- function(template = "bash") {
  bin <- Sys.which("bash")
  stop_if_not(file_test("-f", bin), file_test("-x", bin))
  
  template <- find_template_file(template)
  template_text <- cfReadBrewTemplate(template)

  submitJob <- function(reg, jc) {
    stop_if_not(inherits(reg, "Registry"))
    stop_if_not(inherits(jc, "JobCollection"))

    script <- cfBrewTemplate(reg, text = template_text, jc = jc)
    output <- system2(bin, args = c(script), stdout = TRUE, stderr = TRUE)
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
    store.job.collection = TRUE
  )
  attr(cf, "template") <- template
  attr(cf, "template_text") <- template_text
  cf
}
