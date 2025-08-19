#' Options used for batchtools futures
#'
#' Below are the \R options and environment variables that are used by the
#' \pkg{future.batchtools} package.
#' See [future::future.options] for additional ones that apply to futures
#' in general.\cr
#' \cr
#' _WARNING: Note that the names and the default values of these options
#' may change in future versions of the package.  Please use with care
#' until further notice._
#'
#' @section Settings for batchtools futures:
#' \describe{
#'   \item{\option{future.batchtools.workers}:}{(a positive numeric or `+Inf`)
#'     The default number of workers available on HPC schedulers with
#'     job queues.  (Default: `100`)}
#'
#'   \item{\option{future.batchtools.output}:}{(logical)
#'     If TRUE, \pkg{batchtools} will produce extra output.
#'     If FALSE, such output will be disabled by setting \pkg{batchtools}
#'     options \option{batchtools.verbose} and \option{batchtools.progress}
#'     to FALSE.
#'     (Default: `getOption("future.debug", FALSE)`)}
#'
#'   \item{\option{future.batchtools.expiration.tail}:}{(a positive numeric)
#'     When a \pkg{batchtools} job expires, the last few lines will be
#'     relayed by batchtools futures to help troubleshooting.
#'     This option controls how many lines are displayed.
#'     (Default: `48L`)}
#'
#'   \item{\option{future.cache.path}:}{
#'     (character string)
#'     An absolute or relative path specifying the root folder in which
#'     \pkg{batchtools} registry folders are stored.
#'     This folder needs to be accessible from all hosts ("workers").
#'     Specifically, it must _not_ be a folder that is only local to the
#'     machine such as `file.path(tempdir(), ".future"` if an job scheduler
#'     on a HPC environment is used.
#'     (Default: `.future` in the current working directory)}
#' }
#'
#' @section Environment variables that set R options:
#' All of the above \R \option{future.batchtools.*} options can be set by
#' corresponding environment variable \env{R_FUTURE_BATCHTOOLS_*} _when
#' the \pkg{future.batchtools} package is loaded_.  This means that those
#' environment variables must be set before the \pkg{future.batchtools}
#' package is loaded in order to have an effect.
#' For example, if `R_FUTURE_BATCHTOOLS_WORKERS="200"` is set, then option
#' \option{future.batchtools.workers} is set to `200` (numeric).
#'
#' @examples
#' # Set an R option:
#' options(future.cache.path = "/cluster-wide/folder/.future")
#'
#' @aliases
#' future.batchtools.options
#'
#' future.cache.path
#' future.batchtools.expiration.tail
#' future.batchtools.output
#' future.batchtools.workers
#'
#' R_FUTURE_CACHE_PATH
#' R_FUTURE_BATCHTOOLS_EXPIRATION_TAIL
#' R_FUTURE_BATCHTOOLS_OUTPUT
#' R_FUTURE_BATCHTOOLS_WORKERS
#'
#' @name zzz-future.batchtools.options
NULL






# Set an R option from an environment variable
update_package_option <- function(name, mode = "character", default = NULL, split = NULL, trim = TRUE, disallow = c("NA"), force = FALSE, debug = FALSE) {
  ## Nothing to do?
  value <- getOption(name, NULL)
  if (!force && !is.null(value)) return(getOption(name, default = default))

  ## name="future.plan.disallow" => env="R_FUTURE_PLAN_DISALLOW"
  env <- gsub(".", "_", toupper(name), fixed = TRUE)
  env <- paste("R_", env, sep = "")

  env_value <- value <- Sys.getenv(env, unset = NA_character_)
  ## Nothing to do?
  if (is.na(value)) {  
    if (debug) mdebugf("Environment variable %s not set", sQuote(env))
    return(getOption(name, default = default))
  }
  
  if (debug) mdebugf("%s=%s", env, sQuote(value))

  ## Trim?
  if (trim) value <- trim(value)

  ## Nothing to do?
  if (!nzchar(value)) return(getOption(name, default = default))

  ## Split?
  if (!is.null(split)) {
    value <- strsplit(value, split = split, fixed = TRUE)
    value <- unlist(value, use.names = FALSE)
    if (trim) value <- trim(value)
  }

  ## Coerce?
  mode0 <- storage.mode(value)
  if (mode0 != mode) {
    suppressWarnings({
      storage.mode(value) <- mode
    })
    if (debug) {
      mdebugf("Coercing from %s to %s: %s", mode0, mode, commaq(value))
    }
  }

  if (length(disallow) > 0) {
    if ("NA" %in% disallow) {
      if (any(is.na(value))) {
        stopf("Coercing environment variable %s=%s to %s would result in missing values for option %s: %s", sQuote(env), sQuote(env_value), sQuote(mode), sQuote(name), commaq(value))
      }
    }
    if (is.numeric(value)) {
      if ("non-positive" %in% disallow) {
        if (any(value <= 0, na.rm = TRUE)) {
          stopf("Environment variable %s=%s specifies a non-positive value for option %s: %s", sQuote(env), sQuote(env_value), sQuote(name), commaq(value))
        }
      }
      if ("negative" %in% disallow) {
        if (any(value < 0, na.rm = TRUE)) {
          stopf("Environment variable %s=%s specifies a negative value for option %s: %s", sQuote(env), sQuote(env_value), sQuote(name), commaq(value))
        }
      }
    }
  }
  
  if (debug) {
    mdebugf("=> options(%s = %s) [n=%d, mode=%s]",
            dQuote(name), commaq(value),
            length(value), storage.mode(value))
  }

  do.call(options, args = structure(list(value), names = name))
  
  getOption(name, default = default)
}


## Set future options based on environment variables
update_package_options <- function(debug = FALSE) {
  update_package_option("future.cache.path", mode = "character", debug = debug)
  
  update_package_option("future.batchtools.expiration.tail", mode = "integer", debug = debug)
  update_package_option("future.batchtools.output", mode = "logical", debug = debug)
  update_package_option("future.batchtools.workers", mode = "numeric", debug = debug)
  update_package_option("future.batchtools.status.cache", mode = "logical", default = TRUE, debug = debug)
}
