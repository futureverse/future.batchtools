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
#' @param workers The maximum number of workers the batchtools backend may
#' use at any time, which for HPC schedulers corresponds to the maximum number
#' of queued jobs. The default is
#' \code{getOption("\link{future.batchtools.workers}", 100)}.
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
#' @keywords internal
#'     
#' @importFrom batchtools makeClusterFunctionsLSF
#' @importFrom batchtools makeClusterFunctionsOpenLava
#' @importFrom batchtools makeClusterFunctionsSGE
#' @importFrom batchtools makeClusterFunctionsSlurm
#' @importFrom batchtools makeClusterFunctionsTORQUE
#' @export
BatchtoolsTemplateFutureBackend <- function(type, scheduler.latency = 1.0, fs.latency = 65.0, resources = list(), delete = "on-success", template = NULL, makeClusterFunctions = NULL, workers = getOption("future.batchtools.workers", default = 100L), ...) {
  assert_no_positional_args_but_first()
  stop_if_not(
    is.character(type),
    length(type) == 1,
    !is.na(type),
    nzchar(type)
  )
  
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
    torque   = makeClusterFunctionsTORQUE,
               makeClusterFunctions 
  )
  if (is.null(make_cfs)) {
    stop("Argument 'makeClusterFunctions' is not specified")
  } else if (!is.function(make_cfs)) {
    stop("Argument 'makeClusterFunctions' should be a function: ", mode(make_cfs))
  }
  
  make_cfs_formals <- formals(make_cfs)
  
  ## Get the default template?
  if (is.null(template)) {
    template <- make_cfs_formals$template
  }

  stop_if_not(is.character(template), length(template) == 1L,
              !is.na(template), nzchar(template))

  template <- find_template_file(template)
  dotdotdot[["resources"]] <- resources
  
  keep <- which(names(dotdotdot) %in% names(make_cfs_formals))
  args <- c(list(template = template), dotdotdot[keep], scheduler.latency = scheduler.latency, fs.latency = fs.latency)
  cluster.functions <- do.call(make_cfs, args = args)
  attr(cluster.functions, "template") <- template

  ## Drop used '...' arguments
  if (length(keep) > 0) dotdotdot <- dotdotdot[-keep]

  args <- dotdotdot
  args[["cluster.functions"]] <- cluster.functions
  args[["workers"]] <- workers
  args[["delete"]] <- delete
  
  core <- do.call(BatchtoolsMultiprocessFutureBackend, args = args)
  
  core[["futureClasses"]] <- c("BatchtoolsTemplateFuture", "BatchtoolsMultiprocessFuture", core[["futureClasses"]])
  core <- structure(core, class = c("BatchtoolsTemplateFutureBackend", class(core)))
  core
}
