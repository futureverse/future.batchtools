## Validates the syntax of a rendered job script using 'bash -n' (or 'sh -n'),
## which parses the script without evaluating it. Only scripts with a
## Bash/POSIX-shell shebang are validated; all other scripts are skipped.
## Returns the pathname invisibly, or throws an error if a syntax error
## is detected.
validate_job_script <- function(pathname) {
  if (!isTRUE(getOption("future.batchtools.script.validate", TRUE))) {
    return(invisible(pathname))
  }

  line <- readLines(pathname, n = 1L, warn = FALSE)
  if (length(line) == 0L || !grepl("^#!", line)) return(invisible(pathname))

  ## Parse shebang, e.g. '#!/bin/bash', '#!/usr/bin/env bash -l'
  parts <- strsplit(trimws(sub("^#!", "", line)), split = "[[:space:]]+")[[1]]
  if (length(parts) == 0L) return(invisible(pathname))
  shell <- basename(parts[1])
  if (shell == "env" && length(parts) >= 2L) shell <- basename(parts[2])
  if (!is.element(shell, c("bash", "sh"))) return(invisible(pathname))

  bin <- Sys.which(shell)
  if (!nzchar(bin)) return(invisible(pathname))

  output <- suppressWarnings(
    system2(bin, args = c("-n", shQuote(pathname)), stdout = TRUE, stderr = TRUE)
  )
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) {
    stopf("The generated job script has a syntax error according to '%s -n' (exit code %d): %s\n%s\nScript: %s\nIf this is a false positive, disable validation by setting option 'future.batchtools.script.validate' to FALSE.",
          shell, status, pathname, paste(output, collapse = "\n"), pathname)
  }

  invisible(pathname)
}


## Patches the submitJob() function of a batchtools ClusterFunctions object
## such that each job script that it renders via cfBrewTemplate() is
## validated before it is submitted. Patching is idempotent.
#' @importFrom batchtools cfBrewTemplate
patch_cluster_functions_validate <- function(cf) {
  submitJob <- cf[["submitJob"]]
  if (!is.function(submitJob)) return(cf)

  env <- environment(submitJob)
  if (isTRUE(env[["..future.batchtools.validated.."]])) return(cf)

  env_new <- new.env(parent = env)
  env_new[["..future.batchtools.validated.."]] <- TRUE
  env_new[["cfBrewTemplate"]] <- function(...) {
    pathname <- cfBrewTemplate(...)
    validate_job_script(pathname)
    pathname
  }
  environment(submitJob) <- env_new
  cf[["submitJob"]] <- submitJob
  cf
}
