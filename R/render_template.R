#' Render a batchtools job script without submitting it
#'
#' Renders the job script that \pkg{batchtools} would submit to the
#' scheduler for a given batchtools future strategy, without actually
#' submitting (or even creating) a real job. This is useful for
#' inspecting and troubleshooting a template, e.g. to see exactly how
#' `resources` are expanded into scheduler directives.
#'
#' @param strategy A batchtools future strategy as accepted by
#' [future::plan()], e.g. `future.batchtools::batchtools_sge` or
#' `future::tweak(future.batchtools::batchtools_sge, resources = list(slots = 8))`.
#' If `NULL` (default), the currently set future plan is used.
#'
#' @param \ldots Named resources overriding those already set by
#' `strategy`, e.g.
#' `render_template(future.batchtools::batchtools_slurm, time = "00:05:00")`.
#'
#' @return A character string holding the rendered job script.
#'
#' @examples
#' script <- render_template(
#'   future.batchtools::batchtools_slurm,
#'   time = "00:05:00", mem = "200M"
#' )
#' cat(script)
#'
#' @importFrom batchtools makeRegistry batchMap makeJobCollection cfReadBrewTemplate cfBrewTemplate
#' @importFrom future plan
#' @export
render_template <- function(strategy = NULL, ...) {
  resources <- list(...)

  if (is.null(strategy)) {
    backend <- plan("backend")
  } else {
    oplan <- plan(strategy, .cleanup = FALSE, .init = FALSE)
    on.exit(plan(oplan, .cleanup = FALSE, .init = FALSE), add = TRUE)
    backend <- plan("backend")
  }

  if (!inherits(backend, "BatchtoolsFutureBackend")) {
    stopf("'strategy' is not a batchtools future strategy: %s",
          paste(sQuote(class(backend)), collapse = ", "))
  }

  cf <- backend[["cluster.functions"]]
  template <- attr(cf, "template")
  if (is.null(template)) {
    stopf("Batchtools future strategy does not use a job-script template: %s",
          sQuote(cf[["name"]]))
  }

  if (length(resources) == 0) resources <- backend[["resources"]]

  reg_dir <- tempfile("future.batchtools_template_")
  on.exit(unlink(reg_dir, recursive = TRUE), add = TRUE)

  oopts <- options(batchtools.verbose = FALSE, batchtools.progress = FALSE)
  on.exit(options(oopts), add = TRUE)

  reg <- makeRegistry(file.dir = reg_dir, make.default = FALSE, conf.file = NA_character_)
  reg$cluster.functions <- cf

  id <- batchMap(function(x) x, 1, reg = reg)
  jc <- makeJobCollection(ids = id, resources = resources, reg = reg)

  text <- cfReadBrewTemplate(template)
  script_path <- cfBrewTemplate(reg, text = text, jc = jc)

  lines <- readLines(script_path)
  paste(c(lines, ""), collapse = "\n")
}
