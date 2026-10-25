#' @tags skip_on_cran

library(future)

message("*** Job-script validation ...")

bin <- Sys.which("bash")
if (utils::file_test("-f", bin) && .Platform[["OS.type"]] != "windows") {
  tmpl <- tempfile(fileext = ".tmpl")
  on.exit(file.remove(tmpl), add = TRUE)

  message("- Template with syntax error")
  writeLines(c("#!/bin/bash", "if true; then", "echo 'oops'"), con = tmpl)
  plan(future.batchtools::batchtools_bash, template = tmpl)
  res <- tryCatch({
    f <- future(42L)
    value(f)
  }, error = identity)
  print(res)
  stopifnot(inherits(res, "error"), grepl("syntax error", conditionMessage(res)))

  message("- Validation disabled")
  script <- tempfile(fileext = ".job")
  on.exit(file.remove(script), add = TRUE)
  writeLines(c("#!/bin/bash", "if true; then"), con = script)
  validate <- future.batchtools:::validate_job_script
  res <- tryCatch(validate(script), error = identity)
  stopifnot(inherits(res, "error"))
  options(future.batchtools.script.validate = FALSE)
  res <- tryCatch(validate(script), error = identity)
  stopifnot(!inherits(res, "error"))
  options(future.batchtools.script.validate = TRUE)

  message("- Valid template")
  plan(future.batchtools::batchtools_bash)
  stopifnot(value(future(42L)) == 42L)
} else {
  message("Skipping: 'bash' is not available")
}

message("*** Job-script validation ... DONE")
