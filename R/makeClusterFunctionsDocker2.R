## SPDX-License-Identifier: LGPL-3.0-or-later
## Code in this file is adapted from the 'batchtools' package
## (https://cran.r-project.org/package=batchtools)
## which is licensed under the LGPL (>= 3)

#' ClusterFunctions for Docker (patched)
#'
#' This function is an alternative to
#' [batchtools::makeClusterFunctionsDocker()] with a few patches.
#'
#' 1. The `listJobsRunning()` cluster function uses a valid `docker ps`
#'    filter. The \pkg{batchtools} version uses `--filter user=<user>`,
#'    which Docker rejects with "invalid filter 'user'", making it
#'    impossible to query the status of running jobs.
#'
#' 2. The \pkg{batchtools} registry folder and the registry's working
#'    directory are bind-mounted into the container at the same paths
#'    as on the host, because \pkg{batchtools} passes absolute paths
#'    to the job.
#'
#' 3. On Unix-like hosts, the container runs as the current user and
#'    group, with environment variable `HOME` set to `/tmp`, so that
#'    files written to the registry folder are owned by the current user.
#'
#' 4. Containers are launched using `docker run --rm`, so that they are
#'    removed when they exit.
#'
#' 5. All output of the job, including errors occurring before
#'    `batchtools::doJobCollection()` is called, e.g. when \pkg{batchtools}
#'    is not installed in the image, is written to the job's log file.
#'
#' 6. Resources `ncpus` and `memory` (in MiB) are optional. Resources
#'    `walltime`, `threads`, `omp.threads`, and `blas.threads` are also
#'    supported, as in \pkg{batchtools}.
#'
#' @param image (character) The Docker image to run. It must have R
#' and the \pkg{batchtools} package installed.
#'
#' @param docker.args (character) Command-line options passed to `docker`
#' before the subcommand, e.g. `c("--host", "ssh://user@remote")`.
#'
#' @param image.args (character) Command-line options passed to
#' `docker run` before the image name.
#'
#' @inheritParams batchtools::makeClusterFunctions
#'
#' @return
#' A [batchtools::ClusterFunctions] object.
#'
#' @details
#' Contrary to [batchtools::makeClusterFunctionsDocker()], there is no
#' `docker.user` argument, and the default file-system latency is
#' `fs.latency = 0`, which is suitable for local Docker daemons. If the
#' registry folder is on a network file system, e.g. when using a
#' remote Docker daemon, you may have to increase `fs.latency`.
#'
#' These cluster functions are used by the [batchtools_docker] backend.
#' They can also be used with \pkg{batchtools} directly, e.g. by setting
#' `cluster.functions <- future.batchtools::makeClusterFunctionsDocker2(image = "<image>")`
#' in a \pkg{batchtools} configuration file.
#'
#' @examplesIf interactive()
#' library(batchtools)
#' cf <- future.batchtools::makeClusterFunctionsDocker2(image = "<image>")
#' reg <- makeRegistry(file.dir = NA)
#' reg$cluster.functions <- cf
#' batchMap(function(x) x^2, x = 1:3, reg = reg)
#' submitJobs(reg = reg)
#' waitForJobs(reg = reg)
#' reduceResultsList(reg = reg)
#'
#' @importFrom batchtools makeClusterFunctions makeSubmitJobResult cfKillJob cfHandleUnknownSubmitError assertRegistry
#' @importFrom checkmate assertString assertCharacter assertClass assertIntegerish asInt %??%
#' @importFrom stringi stri_detect_regex stri_sub stri_flatten
#' @export
makeClusterFunctionsDocker2 <- function(image, docker.args = character(0L), image.args = character(0L), scheduler.latency = 1.0, fs.latency = 0.0) {
  OSError <- import_from("OSError", package = "batchtools")
  assertString(image, min.chars = 1L)
  assertCharacter(docker.args, any.missing = FALSE)
  assertCharacter(image.args, any.missing = FALSE)

  user <- Sys.info()[["user"]]
  label_user <- sprintf("future.batchtools.user=%s", user)

  ## Run container as the current user (only on Unix-like hosts)
  user_args <- character(0L)
  if (.Platform[["OS.type"]] == "unix") {
    uid <- tryCatch(system2("id", args = "-u", stdout = TRUE, stderr = FALSE), error = function(e) character(0L), warning = function(w) character(0L))
    gid <- tryCatch(system2("id", args = "-g", stdout = TRUE, stderr = FALSE), error = function(e) character(0L), warning = function(w) character(0L))
    if (length(uid) == 1L && length(gid) == 1L && grepl("^[0-9]+$", uid) && grepl("^[0-9]+$", gid)) {
      user_args <- c(sprintf("--user=%s:%s", uid, gid), "--env=HOME=/tmp")
    }
  }

  submitJob <- function(reg, jc) {
    assertRegistry(reg, writeable = TRUE)
    assertClass(jc, "JobCollection")
    resources <- jc$resources

    mount_args <- character(0L)
    paths <- unique(c(reg$file.dir, reg$work.dir))
    paths <- unique(normalizePath(paths, mustWork = FALSE))
    for (path in paths) {
      mount_args <- c(mount_args, sprintf("--volume=%s:%s", shQuote(path), shQuote(path)))
    }

    resource_args <- character(0L)
    if (!is.null(resources$ncpus)) {
      assertIntegerish(resources$ncpus, lower = 1L, any.missing = FALSE, len = 1L, .var.name = "resources$ncpus")
      resource_args <- c(resource_args, sprintf("--cpus=%d", as.integer(resources$ncpus)))
    }
    if (!is.null(resources$memory)) {
      assertIntegerish(resources$memory, lower = 6L, any.missing = FALSE, len = 1L, .var.name = "resources$memory")
      resource_args <- c(resource_args, sprintf("--memory=%dm", as.integer(resources$memory)), sprintf("--memory-swap=%dm", as.integer(resources$memory)))
    }
    threads <- resources$omp.threads %??% resources$threads
    if (!is.null(threads)) {
      resource_args <- c(resource_args, sprintf("--env=OMP_NUM_THREADS=%d", as.integer(threads)))
    }
    threads <- resources$blas.threads %??% resources$threads
    if (!is.null(threads)) {
      resource_args <- c(resource_args, sprintf("--env=OPENBLAS_NUM_THREADS=%d", as.integer(threads)), sprintf("--env=MKL_NUM_THREADS=%d", as.integer(threads)))
    }

    timeout <- character(0L)
    if (!is.null(resources$walltime)) {
      timeout <- sprintf("timeout %d", asInt(resources$walltime, lower = 0L))
    }

    ## Like the job-script templates, redirect all output to the
    ## batchtools log file at the shell level. This way, also errors
    ## occurring before R is up and running, e.g. failing to load
    ## 'batchtools', end up in the log file, which is otherwise lost
    ## because of 'docker run --rm'
    rexpr <- sprintf('batchtools::doJobCollection("%s")', jc$uri)
    script <- sprintf("exec > %s 2>&1; exec %s", shQuote(jc$log.file), paste(c(timeout, "Rscript", "-e", shQuote(rexpr)), collapse = " "))

    cmd <- c(
      docker.args,
      "run", "--detach", "--rm",
      user_args,
      mount_args,
      sprintf("--workdir=%s", shQuote(reg$work.dir)),
      resource_args,
      sprintf("--label=batchtools=%s", jc$job.hash),
      sprintf("--label=%s", label_user),
      sprintf("--name=%s_bt_%s", user, jc$job.hash),
      image.args,
      image,
      "/bin/sh", "-c", shQuote(script)
    )
    res <- runOSCommand("docker", cmd)

    if (res$exit.code > 0L) {
      return(cfHandleUnknownSubmitError(stri_flatten(c("docker", cmd), " "), res$exit.code, res$output))
    }

    ## 'docker run --detach' outputs the 64-character container ID;
    ## ignore any other output, e.g. from pulling the image
    idx <- which(stri_detect_regex(res$output, "^[[:alnum:]]{64}$"))
    if (length(idx) != 1L) {
      stopf("Command %s did not return a container ID: %s", sQuote(stri_flatten(c("docker", cmd), " ")), paste(res$output, collapse = "\n"))
    }
    makeSubmitJobResult(status = 0L, batch.id = stri_sub(res$output[idx], 1L, 12L))
  } ## submitJob()

  listJobsRunning <- function(reg) {
    assertRegistry(reg, writeable = FALSE)
    args <- c(docker.args, "ps", "--no-trunc", "--format={{.ID}}", "--filter=label=batchtools", sprintf("--filter=label=%s", label_user))
    res <- runOSCommand("docker", args)
    if (res$exit.code > 0L) OSError("Listing of Docker containers failed", res)
    ids <- res$output
    ids <- ids[nzchar(ids)]
    if (length(ids) == 0L) return(character(0L))
    stri_sub(ids, 1L, 12L)
  } ## listJobsRunning()

  killJob <- function(reg, batch.id) {
    assertRegistry(reg, writeable = TRUE)
    assertString(batch.id)
    cfKillJob(reg, "docker", c(docker.args, "kill", batch.id))
  } ## killJob()

  makeClusterFunctions(
    name = "Docker",
    submitJob = submitJob,
    killJob = killJob,
    listJobsRunning = listJobsRunning,
    store.job.collection = TRUE,
    scheduler.latency = scheduler.latency,
    fs.latency = fs.latency
  )
}
