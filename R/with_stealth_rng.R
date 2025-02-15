## Evaluates an R expression while preventing any changes to .Random.seed
with_stealth_rng <- function(expr, substitute = TRUE, envir = parent.frame(), ...) {
  if (substitute) expr <- substitute(expr)

  ## Record the original RNG state
  oseed <- .GlobalEnv$.Random.seed
  on.exit({
    if (is.null(oseed)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(list = ".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      }
    } else {
      .GlobalEnv$.Random.seed <- oseed
    }
  })

  ## Evaluate the R expression with "random" RNG state
  if (!is.null(oseed)) {
    rm(list = ".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  eval(expr, envir = envir, enclos = baseenv())
}
