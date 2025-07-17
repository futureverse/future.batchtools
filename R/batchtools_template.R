#' Batchtools futures for LSF, OpenLava, SGE, Slurm, TORQUE etc.
#'
#' Batchtools futures for LSF, OpenLava, SGE, Slurm, TORQUE etc. are
#' asynchronous multiprocess futures that will be evaluated on a compute
#' cluster via a job scheduler.
#'
#' @inheritParams BatchtoolsFutureBackend
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
#' @aliases batchtools_lsf batchtools_openlava batchtools_torque
#'     
#' @importFrom batchtools makeClusterFunctionsLSF
#' @importFrom batchtools makeClusterFunctionsOpenLava
#' @importFrom batchtools makeClusterFunctionsSGE
#' @importFrom batchtools makeClusterFunctionsSlurm
#' @importFrom batchtools makeClusterFunctionsTORQUE
#' @export
BatchtoolsTemplateFutureBackend <- function(..., template = NULL, type = c("lsf", "openlava", "sge", "slurm", "torque")) {
  assert_no_positional_args_but_first()
  type <- match.arg(type)


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
  args <- c(list(template = template), dotdotdot[keep])
  cluster.functions <- do.call(make_cfs, args = args)
  attr(cluster.functions, "template") <- template

  ## Drop used '...' arguments
  if (length(keep) > 0) dotdotdot <- dotdotdot[-keep]

  args <- dotdotdot
  args[["cluster.functions"]] <- cluster.functions
  
  core <- do.call(BatchtoolsMultiprocessFutureBackend, args = args)
  
  core[["futureClasses"]] <- c("BatchtoolsTemplateFuture", "BatchtoolsMultiprocessFuture", core[["futureClasses"]])
  core <- structure(core, class = c("BatchtoolsTemplateFutureBackend", class(core)))
  core
}


#' @export
BatchtoolsLsfFutureBackend <- function(...) {
  core <- BatchtoolsTemplateFutureBackend(..., type = "lsf")
  core[["futureClasses"]] <- c("BatchtoolsLsfFuture", core[["futureClasses"]])
  core <- structure(core, class = c("BatchtoolsLsfFutureBackend", class(core)))
  core
}

#' @export
batchtools_lsf <- function(...) {
 stop("INTERNAL ERROR: The future.batchtools::batchtools_lsf() must never be called directly")
}
class(batchtools_lsf) <- c(
  "batchtools_lsf", "batchtools_template",
  "batchtools_multiprocess", "batchtools",
  "multiprocess", "future", "function"
)
attr(batchtools_lsf, "tweakable") <- c(
  "workers",
  "finalize",
  ## Arguments to batchtools::makeClusterFunctionsLSF()
  "scheduler.latency", "fs.latency"
)
attr(batchtools_lsf, "init") <- TRUE
attr(batchtools_lsf, "factory") <- BatchtoolsLsfFutureBackend


#' @export
BatchtoolsOpenLavaFutureBackend <- function(...) {
  core <- BatchtoolsTemplateFutureBackend(..., type = "openlava")
  core[["futureClasses"]] <- c("BatchtoolsOpenLavaFuture", core[["futureClasses"]])
  core <- structure(core, class = c("BatchtoolsOpenLavaFutureBackend", class(core)))
  core
}

#' @export
batchtools_openlava <- function(...) {
 stop("INTERNAL ERROR: The future.batchtools::batchtools_openlava() must never be called directly")
}
class(batchtools_openlava) <- c(
  "batchtools_openlava", "batchtools_template",
  "batchtools_multiprocess", "batchtools",
  "multiprocess", "future", "function"
)
attr(batchtools_openlava, "tweakable") <- c(
  "workers",
  "finalize",
  ## Arguments to batchtools::makeClusterFunctionsOpenLava()
  "scheduler.latency", "fs.latency"
)
attr(batchtools_openlava, "init") <- TRUE
attr(batchtools_openlava, "factory") <- BatchtoolsOpenLavaFutureBackend



#' @export
BatchtoolsTorqueFutureBackend <- function(...) {
  core <- BatchtoolsTemplateFutureBackend(..., type = "torque")
  core[["futureClasses"]] <- c("BatchtoolsTorqueFuture", core[["futureClasses"]])
  core <- structure(core, class = c("BatchtoolsTorqueFutureBackend", class(core)))
  core
}

#' @export
batchtools_torque <- function(...) {
 stop("INTERNAL ERROR: The future.batchtools::batchtools_torque() must never be called directly")
}
class(batchtools_torque) <- c(
  "batchtools_torque", "batchtools_template",
  "batchtools_multiprocess", "batchtools",
  "multiprocess", "future", "function"
)
attr(batchtools_torque, "tweakable") <- c(
  "workers",
  "finalize",
  ## Arguments to batchtools::makeClusterFunctionsTORQUE()
  "scheduler.latency", "fs.latency"
)
attr(batchtools_torque, "init") <- TRUE
attr(batchtools_torque, "factory") <- BatchtoolsTorqueFutureBackend
