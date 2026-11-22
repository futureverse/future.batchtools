message("*** validate_job_script() ...")

validate <- future.batchtools:::validate_job_script
patch <- future.batchtools:::patch_cluster_functions_validate

oopts <- options(future.batchtools.script.validate = TRUE)

has_bash <- nzchar(Sys.which("bash")) && (.Platform[["OS.type"]] != "windows")
has_sh <- nzchar(Sys.which("sh")) && (.Platform[["OS.type"]] != "windows")

script <- tempfile(fileext = ".job")

message("- Scripts that are not validated")

## Empty file
writeLines(character(0L), con = script)
stopifnot(identical(validate(script), script))

## No shebang
writeLines(c("if true; then"), con = script)
stopifnot(identical(validate(script), script))

## Empty shebang
writeLines(c("#!", "if true; then"), con = script)
stopifnot(identical(validate(script), script))

## Non-shell shebang
writeLines(c("#!/usr/bin/env Rscript", "if (TRUE) {"), con = script)
stopifnot(identical(validate(script), script))

writeLines(c("#!/bin/zsh-does-not-exist", "if true; then"), con = script)
stopifnot(identical(validate(script), script))


if (has_bash) {
  message("- Bash scripts")

  writeLines(c("#!/bin/bash", "if true; then", "  echo 'ok'", "fi"), con = script)
  stopifnot(identical(validate(script), script))

  writeLines(c("#!/usr/bin/env bash", "if true; then", "  echo 'ok'", "fi"), con = script)
  stopifnot(identical(validate(script), script))

  writeLines(c("#!/bin/bash", "if true; then"), con = script)
  res <- tryCatch(validate(script), error = identity)
  print(res)
  stopifnot(
    inherits(res, "error"),
    grepl("syntax error", conditionMessage(res)),
    grepl("bash -n", conditionMessage(res), fixed = TRUE)
  )

  writeLines(c("#!/usr/bin/env bash -l", "if true; then"), con = script)
  res <- tryCatch(validate(script), error = identity)
  stopifnot(inherits(res, "error"))

  message("- Validation disabled")
  options(future.batchtools.script.validate = FALSE)
  writeLines(c("#!/bin/bash", "if true; then"), con = script)
  stopifnot(identical(validate(script), script))
  options(future.batchtools.script.validate = TRUE)
} else {
  message("Skipping Bash tests: 'bash' is not available")
}


if (has_sh) {
  message("- POSIX shell scripts")

  writeLines(c("#!/bin/sh", "if true; then", "  echo 'ok'", "fi"), con = script)
  stopifnot(identical(validate(script), script))

  writeLines(c("#!/bin/sh", "if true; then"), con = script)
  res <- tryCatch(validate(script), error = identity)
  print(res)
  stopifnot(inherits(res, "error"), grepl("sh -n", conditionMessage(res), fixed = TRUE))
} else {
  message("Skipping POSIX shell tests: 'sh' is not available")
}

file.remove(script)


message("*** patch_cluster_functions_validate() ...")

## Cluster functions without a submitJob() function are left as-is
cf <- batchtools::makeClusterFunctionsInteractive()
cf$submitJob <- NULL
stopifnot(identical(patch(cf), cf))

tmpl <- system.file("templates", "slurm.tmpl", package = "future.batchtools")
cf <- batchtools::makeClusterFunctionsSlurm(template = tmpl)
cf2 <- patch(cf)
env <- environment(cf2$submitJob)
stopifnot(
  inherits(cf2, "ClusterFunctions"),
  isTRUE(env[["..future.batchtools.validated.."]]),
  is.function(env[["cfBrewTemplate"]]),
  identical(parent.env(env), environment(cf$submitJob))
)

## Patching is idempotent
cf3 <- patch(cf2)
stopifnot(identical(cf3, cf2))

if (has_bash) {
  message("- Patched cfBrewTemplate() validates the rendered job script")
  reg_dir <- tempfile()
  oopts2 <- options(batchtools.verbose = FALSE, batchtools.progress = FALSE)
  reg <- batchtools::makeRegistry(file.dir = reg_dir, make.default = FALSE, conf.file = NA_character_)
  reg$cluster.functions <- cf2
  id <- batchtools::batchMap(function(x) x, 1, reg = reg)
  jc <- batchtools::makeJobCollection(ids = id, resources = list(), reg = reg)
  cfBrewTemplate <- env[["cfBrewTemplate"]]

  ## A valid template
  text <- batchtools::cfReadBrewTemplate(tmpl)
  pathname <- cfBrewTemplate(reg, text = text, jc = jc)
  stopifnot(file_test("-f", pathname))

  ## A template with a syntax error
  res <- tryCatch({
    cfBrewTemplate(reg, text = "#!/bin/bash\nif true; then\n", jc = jc)
  }, error = identity)
  print(res)
  stopifnot(inherits(res, "error"), grepl("syntax error", conditionMessage(res)))

  options(oopts2)
  unlink(reg_dir, recursive = TRUE)
}

options(oopts)

message("*** validate_job_script() ... DONE")
