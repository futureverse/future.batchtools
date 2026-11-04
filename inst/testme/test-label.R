#' @tags label

library(future)

message("*** Futures with labels that are not valid job names ...")

plan(future.batchtools::batchtools_local)

## Job schedulers have restrictions on the job names, e.g. SGE does not
## allow colons.  The job name is also written to the job script, so a
## double quote, a command substitution, or a backtick would break the script
## or execute a command, respectively.  The job name is only used by the
## scheduler, so the label of the future itself must stay as-is.
labels <- c(
  "fz:2",
  "a b",
  "x/y",
  "x\\y",
  "a@b",
  "star*",
  "q?",
  "it's",
  "$HOME",
  "a\"b",
  "x$(echo hi)y",
  "a`echo hi`b",
  "a;b|c&d",
  "2fast",
  "-dash",
  "élève",
  "ok-name_1.0",
  "future_lapply-1"
)

valid_job_name <- "^[A-Za-z][A-Za-z0-9._-]*$"

for (label in labels) {
  message(sprintf("- label = %s", sQuote(label)))
  f <- future(42L, label = label)
  
  ## The label of the future is not changed
  stopifnot(identical(f[["label"]], label))

  ## The job name passed on to the job scheduler is valid
  reg <- f[["config"]][["reg"]]
  job_name <- batchtools::getJobNames(reg = reg)[["job.name"]]
  message(sprintf("  job name: %s", sQuote(job_name)))
  stopifnot(length(job_name) == 1L, grepl(valid_job_name, job_name))

  ## Labels that are already valid are used as-is
  if (grepl(valid_job_name, label)) stopifnot(identical(job_name, label))

  v <- value(f)
  stopifnot(identical(v, 42L))
}

message("*** Futures with labels that are not valid job names ... done")
