#' @tags skip_on_cran

library(future)

message("*** Built-in templates do not modify the job's resources ...")

## A template is brewed in the environment of the batchtools
## JobCollection, which means that any modifications to 'resources'
## inside the template would modify the job's resources. If so,
## a resubmission, e.g. after a temporary error, would use other
## resources than the ones requested
reg <- batchtools::makeRegistry(file.dir = tempfile(), make.default = FALSE)
reg$cluster.functions$store.job.collection <- TRUE
batchtools::batchMap(identity, 1L, reg = reg)

resources <- list(
  details = TRUE,
  startup = "echo 'startup'",
  shutdown = "echo 'shutdown'",
  modules = "r",
  envs = c(FOO = "1"),
  rscript = "Rscript",
  rscript_args = "--vanilla",
  asis = "--foo=bar",
  mem = "1G"
)

templates <- c("bash", "lsf", "openlava", "sge", "slurm", "torque")
for (name in templates) {
  message(sprintf("- Template %s", sQuote(name)))
  file <- system.file("templates", sprintf("%s.tmpl", name), package = "future.batchtools", mustWork = TRUE)
  text <- batchtools::cfReadBrewTemplate(file)
  jc <- batchtools::makeJobCollection(1L, resources = resources, reg = reg)
  stopifnot(identical(jc[["resources"]], resources))

  script1 <- readLines(batchtools::cfBrewTemplate(reg, text = text, jc = jc))
  stopifnot(identical(jc[["resources"]], resources))

  ## Brewing again, e.g. on resubmission, gives the same job script
  script2 <- readLines(batchtools::cfBrewTemplate(reg, text = text, jc = jc))
  stopifnot(identical(jc[["resources"]], resources))
  stopifnot(identical(script2, script1))
}

batchtools::removeRegistry(wait = 0, reg = reg)

message("*** Built-in templates do not modify the job's resources ... DONE")
