#' @tags skip_on_cran

library(future)

message("*** batchtools_bash() - failing job script ...")

bin <- Sys.which("bash")
if (utils::file_test("-f", bin) && .Platform[["OS.type"]] != "windows") {
  ## A failing job script must give a launch error, and not be retried
  ## by batchtools::submitJobs(), which treats submit statuses 1-99 as
  ## temporary errors, and loops on statuses 100 and > 200
  launch <- function() {
    setTimeLimit(elapsed = 60, transient = TRUE)
    on.exit(setTimeLimit(elapsed = Inf, transient = FALSE))
    tryCatch(future(42L), error = identity)
  }

  message("- Built-in template with invalid 'rscript' resource (exit 1)")
  plan(future.batchtools::batchtools_bash, resources = list(rscript = "no-such-Rscript"))
  res <- launch()
  print(res)
  stopifnot(
    inherits(res, "FutureLaunchError"),
    grepl("Fatal error occurred: 101", conditionMessage(res)),
    grepl("exited with status 1", conditionMessage(res)),
    grepl("non-existing 'Rscript'", conditionMessage(res))
  )

  for (code in c(100L, 101L, 255L)) {
    message(sprintf("- Custom template exiting with status %d", code))
    ## Use a unique template file, because plan() does not reload an
    ## identical template
    tmpl <- tempfile(fileext = ".tmpl")
    writeLines(c(
      "#!/bin/bash",
      sprintf("echo 'Job script failed with %d on purpose' >&2", code),
      sprintf("exit %d", code)
    ), con = tmpl)
    plan(future.batchtools::batchtools_bash, template = tmpl)
    res <- launch()
    print(res)
    stopifnot(
      inherits(res, "FutureLaunchError"),
      grepl(sprintf("exited with status %d", code), conditionMessage(res)),
      grepl(sprintf("failed with %d on purpose", code), conditionMessage(res))
    )
    file.remove(tmpl)
  }

  message("- Valid template")
  plan(future.batchtools::batchtools_bash)
  stopifnot(value(future(42L)) == 42L)
} else {
  message("Skipping: 'bash' is not available")
}

message("*** batchtools_bash() - failing job script ... DONE")
