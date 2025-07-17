#' @importFrom batchtools makeRegistry saveRegistry
temp_registry <- local({
  ## All known batchtools registries
  regs <- new.env()

  make_registry <- function(conf.file = NULL, cluster.functions = NULL, config = list(), ...) {
    ## Temporarily disable batchtools output?
    ## (i.e. messages and progress bars)
    debug <- isTRUE(getOption("future.debug"))
    batchtools_output <- getOption("future.batchtools.output", debug)

    work.dir <- config$work.dir
    if (is.null(work.dir)) work.dir <- getwd()
    config$work.dir <- NULL
    
    if (!batchtools_output) {
      oopts <- options(batchtools.verbose = FALSE, batchtools.progress = FALSE)
      on.exit(options(oopts))
    }

    ## WORKAROUND: batchtools::makeRegistry() updates the RNG state,
    ## which we must make sure to undo.
    with_stealth_rng({
      reg <- makeRegistry(work.dir = work.dir, conf.file = conf.file, ...)
    })

    if (!is.null(cluster.functions)) {    ### FIXME
      reg$cluster.functions <- cluster.functions
    }

    ## Post-tweak the batchtools registry?
    ## This avoids having to set up a custom batchtools 'conf.file' etc.
    if (length(config) > 0L) {
      names <- names(config)
      for (name in names) reg[[name]] <- config[[name]]
      with_stealth_rng({
        saveRegistry(reg)
      })
    }
    
    reg
  } ## make_registry()

  function(label = "batchtools", path = NULL, conf.file = NULL, cluster.functions = NULL, config = list(), ...) {
    if (is.null(label)) label <- "batchtools"
    ## The job label (the name on the job queue) - may be duplicated
    label <- as.character(label)
    stop_if_not(length(label) == 1L, nchar(label) > 0L)

    ## This session's path holding all of its future batchtools directories
    ##   e.g. .future/<datetimestamp>-<unique_id>/
    if (is.null(path)) path <- future_cache_path()

    if (length(config) > 0L) {
      stop_if_not(is.list(config))
      names <- names(config)
      stop_if_not(!is.null(names), all(nzchar(names)))
    }

    ## The batchtools subfolder for a specific future - must be unique
    prefix <- sprintf("%s_", label)

    ## FIXME: We need to make sure 'prefix' consists of only valid
    ## filename characters. /HB 2016-10-19
    prefix <- as_valid_directory_prefix(prefix)

    ## WORKAROUND: Avoid updating the RNG state
    with_stealth_rng({
      unique <- FALSE
      while (!unique) {
        ## The FutureRegistry key for this batchtools future - must be unique
        key <- tempvar(prefix = prefix, value = NA, envir = regs)
        ## The directory for this batchtools future
        ##   e.g. .future/<datetimestamp>-<unique_id>/<key>/
        path_registry <- file.path(path, key)
        ## Should not happen, but just in case.
        unique <- !file.exists(path_registry)
      }
    })

    ## FIXME: We need to make sure 'label' consists of only valid
    ## batchtools ID characters, i.e. it must match regular
    ## expression "^[a-zA-Z]+[0-9a-zA-Z_]*$".
    ## /HB 2016-10-19
    reg_id <- as_valid_registry_id(label)
    make_registry(file.dir = path_registry, conf.file = conf.file, cluster.functions = cluster.functions, config = config, ...)
  }
})



drop_non_valid_characters <- function(name, pattern, default = "batchtools") {
  as_string <- (length(name) == 1L)
  name <- unlist(strsplit(name, split = "", fixed = TRUE), use.names = FALSE)
  name[!grepl(pattern, name)] <- ""
  if (length(name) == 0L) return(default)
  if (as_string) name <- paste(name, collapse = "")
  name
}

as_valid_directory_prefix <- function(name) {
  pattern <- "^[-._a-zA-Z0-9]+$"
  ## Nothing to do?
  if (grepl(pattern, name)) return(name)
  name <- unlist(strsplit(name, split = "", fixed = TRUE), use.names = FALSE)
  ## All characters must be letters, digits, underscores, dash, or period.
  name <- drop_non_valid_characters(name, pattern = pattern)
  name <- paste(name, collapse = "")
  stop_if_not(grepl(pattern, name))
  name
}

as_valid_registry_id <- function(name) {
  pattern <- "^[a-zA-Z]+[0-9a-zA-Z_]*$"
  ## Nothing to do?
  if (grepl(pattern, name)) return(name)

  name <- unlist(strsplit(name, split = "", fixed = TRUE), use.names = FALSE)

  ## All characters must be letters, digits, or underscores
  name <- drop_non_valid_characters(name, pattern = "[0-9a-zA-Z_]")
  name <- name[nzchar(name)]

  ## First character must be a letter :/
  if (!grepl("^[a-zA-Z]+", name[1])) name[1] <- "z"

  name <- paste(name, collapse = "")

  stop_if_not(grepl(pattern, name))

  name
}
