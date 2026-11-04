## When running R CMD check on HPC environment with a job scheduler,
## the batchtools registry files must be written to a location on the
## file system that is accessible from all compute nodes.  This is
## typically not the case with the default tempdir()/TMPDIR, which
## often is local and unique to each machine

path <- tools::R_user_dir("future.batchtools", "cache")
path <- file.path(path, "R_CMD_check")
if (!utils::file_test("-d", path)) dir.create(path, recursive = TRUE)

tmpdir <- tempfile(tmpdir = path)
if (!utils::file_test("-d", tmpdir)) {
  dir.create(tmpdir, recursive = TRUE)
  if (!utils::file_test("-d", tmpdir)) {
    stop("R_BATCHTOOLS_SEARCH_PATH/setup.R: Failed to create folder: ",
         sQuote(tmpdir))
  }
}

## Force the .future/ folders to be in this folder
## (The environment variable is only read when the package is loaded, so
## the R option must be set as well)
Sys.setenv("R_FUTURE_CACHE_PATH" = file.path(tmpdir, ".future"))
options(future.cache.path = file.path(tmpdir, ".future"))

## Make batchtools_<hpc> backends use this as their working directory
registry <- list(work.dir = tmpdir)
batchtools_lsf      <- future::tweak(future.batchtools::batchtools_lsf, registry = registry)
batchtools_openlava <- future::tweak(future.batchtools::batchtools_openlava, registry = registry)
batchtools_sge      <- future::tweak(future.batchtools::batchtools_sge, registry = registry)
batchtools_slurm    <- future::tweak(future.batchtools::batchtools_slurm, registry = registry)
batchtools_torque   <- future::tweak(future.batchtools::batchtools_torque, registry = registry)
