library(future)

message("*** tweak() - tweakable arguments ...")

## Tweaking a batchtools backend with one of its 'tweakable' arguments
## should not produce an "unknown future arguments" warning, also when
## the package is not attached
stopifnot(!is.element("package:future.batchtools", search()))

names <- getNamespaceExports("future.batchtools")
names <- sort(grep("^batchtools_", names, value = TRUE))
for (name in names) {
  strategy <- getExportedValue("future.batchtools", name)
  if (!inherits(strategy, "future")) next
  tweakable <- eval(attr(strategy, "tweakable", exact = TRUE))
  ## All batchtools backends support these arguments
  tweakable <- unique(c(tweakable, "finalize", "registry"))
  message(sprintf("- %s: %s", name, paste(sQuote(tweakable), collapse = ", ")))
  for (arg in tweakable) {
    args <- list(strategy)
    args[arg] <- list(NULL)
    res <- tryCatch(do.call(tweak, args = args), warning = identity)
    if (inherits(res, "warning")) {
      stop(sprintf("tweak(%s, %s = ...) produced a warning: %s",
                   name, arg, conditionMessage(res)))
    }
  }
}

message("- plan() with argument 'finalize'")
plan(future.batchtools::batchtools_local, finalize = FALSE)
stopifnot(!plan("backend")[["finalize"]])
plan(future.batchtools::batchtools_local, finalize = TRUE)
stopifnot(plan("backend")[["finalize"]])

plan(sequential)

message("*** tweak() - tweakable arguments ... DONE")
