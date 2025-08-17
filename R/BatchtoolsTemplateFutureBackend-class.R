#' Batchtools futures for LSF, OpenLava, SGE, Slurm, TORQUE etc.
#'
#' Batchtools futures for LSF, OpenLava, SGE, Slurm, TORQUE etc. are
#' asynchronous multiprocess futures that will be evaluated on a compute
#' cluster via a job scheduler.
#'
#' @inheritParams BatchtoolsFutureBackend
#' @inheritParams batchtools::makeClusterFunctions
#'
#' @param template (optional) A batchtools template file or a template string
#' (in \pkg{brew} format).  If not specified, it is left to the
#' \pkg{batchtools} package to locate such file using its search rules.
#'
#' @param type (character string) Type of job scheduler.
#'
#' @param \ldots Additional arguments passed to [BatchtoolsFutureBackend()].
#'
#' @return An object of class `BatchtoolsFutureBackend`.
#'
#' @details
#' These type of batchtools futures rely on batchtools backends set
#' up using the following \pkg{batchtools} functions:
#'
#'  * [batchtools::makeClusterFunctionsLSF()] for
#'    [Load Sharing Facility (LSF)](https://en.wikipedia.org/wiki/Platform_LSF)
#'  * [batchtools::makeClusterFunctionsOpenLava()] for
#'    [OpenLava](https://en.wikipedia.org/wiki/OpenLava)
#'  * [batchtools::makeClusterFunctionsSGE()] for
#'    [Sun/Oracle Grid Engine (SGE)](https://en.wikipedia.org/wiki/Oracle_Grid_Engine)
#'  * [batchtools::makeClusterFunctionsSlurm()] for
#'    [Slurm](https://en.wikipedia.org/wiki/Slurm_Workload_Manager)
#'  * [batchtools::makeClusterFunctionsTORQUE()] for
#'    [TORQUE](https://en.wikipedia.org/wiki/TORQUE) / PBS
#'
#' @aliases BatchtoolsLsfFutureBackend BatchtoolsOpenLavaFutureBackend BatchtoolsSGEFutureBackend BatchtoolsSlurmFutureBackend BatchtoolsTorqueFutureBackend
#'     
#' @importFrom batchtools makeClusterFunctionsLSF
#' @importFrom batchtools makeClusterFunctionsOpenLava
#' @importFrom batchtools makeClusterFunctionsSGE
#' @importFrom batchtools makeClusterFunctionsSlurm
#' @importFrom batchtools makeClusterFunctionsTORQUE
#' @export
BatchtoolsTemplateFutureBackend <- function(type = c("lsf", "openlava", "sge", "slurm", "torque"), scheduler.latency = 1.0, fs.latency = 65.0, template = NULL, workers = getOption("future.batchtools.workers", default = 100L), ...) {
  assert_no_positional_args_but_first()
  type <- match.arg(type)

  if (is.function(workers)) workers <- workers()
  stop_if_not(
    is.numeric(workers),
    length(workers) == 1L,
    !is.na(workers),
    is.finite(workers),
    workers >= 1
  )

  dotdotdot <- list(...)

  make_cfs <- switch(type,
    lsf      = makeClusterFunctionsLSF,
    openlava = makeClusterFunctionsOpenLava,
    sge      = makeClusterFunctionsSGE,
    slurm    = makeClusterFunctionsSlurm,
    torque   = makeClusterFunctionsTORQUE
  )

  make_cfs_formals <- formals(make_cfs)
  
  ## Get the default template?
  if (is.null(template)) {
    template <- make_cfs_formals$template
  }

  stop_if_not(is.character(template), length(template) == 1L,
              !is.na(template), nzchar(template))

  template <- find_template_file(template)

  keep <- which(names(dotdotdot) %in% names(make_cfs_formals))
  args <- c(list(template = template), dotdotdot[keep], scheduler.latency = scheduler.latency, fs.latency = fs.latency)
  cluster.functions <- do.call(make_cfs, args = args)
  attr(cluster.functions, "template") <- template

  ## Drop used '...' arguments
  if (length(keep) > 0) dotdotdot <- dotdotdot[-keep]

  args <- dotdotdot
  args[["cluster.functions"]] <- cluster.functions
  args[["workers"]] <- workers
  
  core <- do.call(BatchtoolsMultiprocessFutureBackend, args = args)
  
  core[["futureClasses"]] <- c("BatchtoolsTemplateFuture", "BatchtoolsMultiprocessFuture", core[["futureClasses"]])
  core <- structure(core, class = c("BatchtoolsTemplateFutureBackend", class(core)))
  core
}
