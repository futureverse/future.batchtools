library(future)

message("*** render_template() ...")

oplan <- plan()

message("- Slurm template with resources passed via '...'")
script <- future.batchtools::render_template(
  future.batchtools::batchtools_slurm,
  time = "00:05:00", mem = "200M"
)
cat(script)
stopifnot(
  is.character(script), length(script) == 1L,
  grepl("^#!/bin/bash", script),
  grepl("#SBATCH --time=00:05:00", script, fixed = TRUE),
  grepl("#SBATCH --mem=200M", script, fixed = TRUE),
  grepl("batchtools::doJobCollection", script, fixed = TRUE)
)

## The plan should be unchanged
stopifnot(identical(plan(), oplan))


message("- SGE template with resources set by tweak()")
strategy <- tweak(future.batchtools::batchtools_sge, resources = list(slots = 8))
script <- future.batchtools::render_template(strategy)
stopifnot(grepl("#$ -l slots=8", script, fixed = TRUE))

message("- '...' resources override those of the strategy")
script <- future.batchtools::render_template(strategy, h_rt = "00:10:00")
stopifnot(
  grepl("#$ -l h_rt=00:10:00", script, fixed = TRUE),
  !grepl("slots=8", script, fixed = TRUE)
)


message("- Using the current plan (strategy = NULL)")
plan(future.batchtools::batchtools_slurm, resources = list(mem = "1G"))
script <- future.batchtools::render_template()
stopifnot(grepl("#SBATCH --mem=1G", script, fixed = TRUE))
script <- future.batchtools::render_template(mem = "2G")
stopifnot(
  grepl("#SBATCH --mem=2G", script, fixed = TRUE),
  !grepl("#SBATCH --mem=1G", script, fixed = TRUE)
)


message("- Other built-in templates")
for (name in c("lsf", "openlava", "torque")) {
  strategy <- getExportedValue("future.batchtools", sprintf("batchtools_%s", name))
  script <- future.batchtools::render_template(strategy)
  stopifnot(is.character(script), length(script) == 1L, nzchar(script))
}


message("- Exceptions")
res <- tryCatch({
  future.batchtools::render_template(future.batchtools::batchtools_local)
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

plan(sequential)
res <- tryCatch(future.batchtools::render_template(), error = identity)
print(res)
stopifnot(inherits(res, "error"))


message("- Printing template backends")
plan(future.batchtools::batchtools_slurm)
print(plan("backend"))
plan(future.batchtools::batchtools_sge)
print(plan("backend"))

plan(oplan)

message("*** render_template() ... DONE")
