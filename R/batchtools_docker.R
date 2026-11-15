#' Batchtools Docker futures
#'
#' A batchtools Docker future is an asynchronous multiprocess future
#' that is evaluated in a separate Docker container.
#'
#' @inheritParams BatchtoolsFutureBackend
#'
#' @param image (character) The Docker image to run, e.g.
#' `"rocker/r-ver:4.6.1"`. The image must have R installed, and the
#' \R packages \pkg{batchtools} and \pkg{future}, plus any packages
#' that the futures depend on, must be available inside the container.
#'
#' @param workers (numeric) The maximum number of Docker containers that
#' may run concurrently.
#'
#' @param docker.args (character) Command-line options passed to `docker`
#' before the subcommand, e.g. `c("--host", "ssh://user@remote")`.
#'
#' @param image.args (character) Command-line options passed to
#' `docker run` before the image name, e.g.
#' `c("--volume=/opt/R-libs:/opt/R-libs:ro", "--env=R_LIBS_USER=/opt/R-libs")`.
#'
#' @param fs.latency,scheduler.latency (numeric) Passed to
#' [batchtools::makeClusterFunctions()].
#'
#' @param \ldots Additional arguments passed to [BatchtoolsFutureBackend()].
#'
#' @importFrom parallelly availableCores
#' @keywords internal
#' @export
BatchtoolsDockerFutureBackend <- function(image = getOption("future.batchtools.docker.image", NULL), workers = availableCores(), docker.args = character(0L), image.args = character(0L), fs.latency = 0.0, scheduler.latency = 1.0, ...) {
  assert_no_positional_args_but_first()

  if (.Platform[["OS.type"]] == "windows") {
    stop("The 'batchtools_docker' backend is not supported on MS Windows, because the registry folder has to be mounted into the Linux container at the same path as on the host")
  }

  if (is.null(image)) {
    stop("Argument 'image' must be specified for the 'batchtools_docker' backend, e.g. plan(future.batchtools::batchtools_docker, image = \"<image>\"), where the Docker image has R and the 'batchtools' and 'future' packages installed")
  }
  stop_if_not(is.character(image), length(image) == 1L, !is.na(image), nzchar(image))

  if (is.function(workers)) workers <- workers()
  stop_if_not(
    is.numeric(workers),
    length(workers) == 1L,
    !is.na(workers),
    workers >= 1
  )

  cluster.functions <- makeClusterFunctionsDocker2(
    image = image,
    docker.args = docker.args,
    image.args = image.args,
    scheduler.latency = scheduler.latency,
    fs.latency = fs.latency
  )

  core <- BatchtoolsMultiprocessFutureBackend(
    workers = workers,
    cluster.functions = cluster.functions,
    ...
  )
  core[["futureClasses"]] <- c("BatchtoolsDockerFuture", "BatchtoolsMultiprocessFuture", core[["futureClasses"]])
  core <- structure(core, class = c("BatchtoolsDockerFutureBackend", class(core)))
  core
}


#' A batchtools backend that resolves futures in parallel via Docker containers
#'
#' @inheritParams BatchtoolsDockerFutureBackend
#' @inheritParams BatchtoolsFutureBackend
#'
#' @param \ldots Not used.
#'
#' @return Nothing.
#'
#' @details
#' Batchtools Docker futures use \pkg{batchtools} cluster functions
#' created by [makeClusterFunctionsDocker2()].
#' Each future is evaluated in its own Docker container, launched via
#' `docker run --detach --rm <image> Rscript -e "batchtools::doJobCollection(...)"`.
#' This requires that the `docker` command-line tool is available and
#' that the current user has permission to use it.
#'
#' The \pkg{batchtools} registry folder (by default, a subfolder of
#' `.future/` in the current working directory) and the current working
#' directory are bind-mounted into the container at the same paths as
#' on the host. The container runs as the current user and group, so
#' that files written to these folders are owned by the current user.
#' Environment variable `HOME` is set to `/tmp` inside the container.
#'
#' The following `resources` are supported:
#'
#' * `ncpus` - the number of CPUs (`docker run --cpus=<ncpus>`)
#' * `memory` - the maximum amount of memory in MiB
#'   (`docker run --memory=<memory>m`)
#' * `walltime` - the maximum runtime in seconds, which requires that
#'   the `timeout` command is available in the container
#' * `threads`, `omp.threads`, and `blas.threads` - set environment
#'   variables `OMP_NUM_THREADS`, `OPENBLAS_NUM_THREADS`, and
#'   `MKL_NUM_THREADS`
#'
#' To use a remote Docker daemon, e.g.
#' `docker.args = c("--host", "ssh://user@remote")`, the registry folder
#' and the working directory must be available on the remote host at
#' the same paths as on the current machine, e.g. via a shared file
#' system. On such network file systems, you may have to increase
#' `fs.latency` to account for delays in file synchronization.
#'
#' This backend is not supported on MS Windows.
#'
#' @section Setting up a Docker image and an R package library:
#' The \pkg{batchtools} and \pkg{future} packages, and any packages that
#' the futures depend on, must be installed in the container. Instead of
#' building a custom image with these packages, they can be installed
#' into an R package library on the host, which is then mounted into the
#' containers. Installing them from within the container makes sure they
#' are compatible with the R version and the operating system of the image.
#'
#' First, the image must provide the system libraries that the packages
#' need. For instance, the \pkg{fs} package, which \pkg{batchtools}
#' depends on, requires the `libuv1` system library, which is not part
#' of the `rocker/r-ver` images. A minimal image that adds it can be
#' built from the following `Dockerfile`:
#'
#' ```dockerfile
#' FROM rocker/r-ver:4.6.1
#' RUN apt-get update \
#'  && apt-get install -y --no-install-recommends libuv1 \
#'  && rm -rf /var/lib/apt/lists/*
#' ```
#'
#' by calling the following in the folder of that file:
#'
#' ```sh
#' docker build -t r-ver-libuv:4.6.1 .
#' ```
#'
#' Second, install the R packages into a folder on the host, by running
#' `install.packages()` from within the container:
#'
#' ```sh
#' lib=~/R/rocker_r-ver-x86_64-pc-linux-gnu-library/4.6
#' mkdir -p "$lib"
#' docker run --rm --user="$(id -u):$(id -g)" --env=HOME=/tmp \
#'   --volume="$lib:$lib" --env=R_LIBS_USER="$lib" \
#'   r-ver-libuv:4.6.1 \
#'   Rscript -e 'install.packages(c("batchtools", "future"), lib = Sys.getenv("R_LIBS_USER"))'
#' ```
#'
#' Argument `lib` is needed, because the `rocker/r-ver` images put the
#' system-wide site library first in `.libPaths()`. Since R packages are
#' specific to the R version and the operating system, use one such
#' library per image. See below example for how to use this setup.
#'
#' @examplesIf interactive()
#' library(future)
#'
#' ## Use the image and the R package library set up as explained in
#' ## Section 'Setting up a Docker image and an R package library'.
#' ## The library is mounted read-only
#' lib <- path.expand("~/R/rocker_r-ver-x86_64-pc-linux-gnu-library/4.6")
#' plan(future.batchtools::batchtools_docker,
#'      image = "r-ver-libuv:4.6.1",
#'      image.args = c(
#'        sprintf("--volume=%s:%s:ro", lib, lib),
#'        sprintf("--env=R_LIBS_USER=%s", lib)
#'      ),
#'      workers = 2)
#'
#' f <- future({
#'   data.frame(
#'     hostname = Sys.info()[["nodename"]],
#'           os = Sys.info()[["sysname"]],
#'          pid = Sys.getpid()
#'   )
#' })
#' info <- value(f)
#' print(info)
#'
#' @export
batchtools_docker <- function(..., image = getOption("future.batchtools.docker.image", NULL), workers = availableCores(), docker.args = character(0L), image.args = character(0L), fs.latency = 0.0, scheduler.latency = 1.0, resources = list(), delete = getOption("future.batchtools.delete", "on-success")) {
  stop("INTERNAL ERROR: The future.batchtools::batchtools_docker() must never be called directly")
}
class(batchtools_docker) <- c(
  "batchtools_docker", "batchtools_custom",
  "batchtools_multiprocess", "batchtools",
  "multiprocess", "future", "function"
)
attr(batchtools_docker, "tweakable") <- c("finalize")
attr(batchtools_docker, "init") <- TRUE
attr(batchtools_docker, "factory") <- BatchtoolsDockerFutureBackend
