message("*** Internals ...")

options(future.delete = TRUE)

Sys.setenv(R_FUTURE_BATCHTOOLS_DEBUG = "TRUE")
future.batchtools:::update_package_options(debug = TRUE)

future.batchtools:::update_package_option("future.batchtools.debug", mode = "logical", force = TRUE, debug = TRUE)

Sys.setenv(R_FUTURE_BATCHTOOLS_FOO = "abc,def")
future.batchtools:::update_package_option("future.batchtools.foo", mode = "character", split = ",", force = TRUE, debug = TRUE)

Sys.setenv(R_FUTURE_BATCHTOOLS_FOO = "")
future.batchtools:::update_package_option("future.batchtools.foo", mode = "character", split = ",", force = TRUE, debug = TRUE)

Sys.setenv(R_FUTURE_BATCHTOOLS_BAR = "NA")
tryCatch(future.batchtools:::update_package_option("future.batchtools.bar", mode = "integer", force = TRUE, debug = TRUE), error = identity)

Sys.setenv(R_FUTURE_BATCHTOOLS_BAR = "-1")
tryCatch(future.batchtools:::update_package_option("future.batchtools.bar", mode = "integer", disallow = "negative", force = TRUE, debug = TRUE), error = identity)

Sys.setenv(R_FUTURE_BATCHTOOLS_BAR = "0")
tryCatch(future.batchtools:::update_package_option("future.batchtools.bar", mode = "integer", disallow = "non-positive", force = TRUE, debug = TRUE), error = identity)


message("*** Internals ... done")

