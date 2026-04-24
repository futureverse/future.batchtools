library(future.batchtools)

message("*** Internals ...")

options(future.delete = TRUE)

Sys.setenv(R_FUTURE_BATCHTOOLS_DEBUG = "TRUE")
future.batchtools:::update_package_options(debug = TRUE)

future.batchtools:::update_package_option("future.batchtools.debug", mode = "logical", force = TRUE, debug = TRUE)
options(future.batchtools.debug = "always")

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


# 2. find_template_file
# Should find one of the built-in templates
path <- future.batchtools:::find_template_file("slurm")
stopifnot(file.exists(path))
# Should fail for non-existing template
res <- try(future.batchtools:::find_template_file("non-existing-template"), silent = TRUE)
stopifnot(inherits(res, "try-error"))


# 4. runOSCommand
if (future.batchtools:::is_os("linux") || future.batchtools:::is_os("macos")) {
  res <- future.batchtools:::runOSCommand("echo", "hello")
  stopifnot(res$exit.code == 0)
  stopifnot(grepl("hello", res$output))
}

# Test with non-existing command
res <- future.batchtools:::runOSCommand("non-existing-command-xyz")
stopifnot(res$exit.code == 127)

# 5. Utils in temp_registry.R
stopifnot(future.batchtools:::as_valid_directory_prefix("abc-123_.") == "abc-123_.")
stopifnot(future.batchtools:::as_valid_directory_prefix("abc!@#") == "abc")

stopifnot(future.batchtools:::as_valid_registry_id("A123_") == "A123_")
# Registry ID must start with a letter
stopifnot(grepl("^[a-zA-Z]", future.batchtools:::as_valid_registry_id("123")))


# 8. stopf and warnf
res <- try(future.batchtools:::stopf("error %d", 1), silent = TRUE)
stopifnot(inherits(res, "try-error"))

res <- tryCatch(future.batchtools:::warnf("warning %d", 1), warning = function(w) w)
stopifnot(inherits(res, "warning"))

# 9. BatchtoolsSSHRegistry
mockMakeCluster <- function(workers, ...) {
  list(workers = workers, ...)
}
res <- future.batchtools:::BatchtoolsSSHRegistry(action = "start", workers = 1, makeCluster = mockMakeCluster)
stopifnot(identical(res$workers, 1L))
res_get <- future.batchtools:::BatchtoolsSSHRegistry(action = "get")
stopifnot(identical(res, res_get))
future.batchtools:::BatchtoolsSSHRegistry(action = "stop")
res_null <- future.batchtools:::BatchtoolsSSHRegistry(action = "get")
stopifnot(is.null(res_null))

# 10. temp_registry
reg <- future.batchtools:::temp_registry(label = "test-reg")
stopifnot(inherits(reg, "Registry"))
# Cleanup
unlink(reg$file.dir, recursive = TRUE)


# 14. Debug functions
future.batchtools:::mdebug("test mdebug")
future.batchtools:::mdebugf("test mdebugf %d", 1)
future.batchtools:::mdebug_push("push")
future.batchtools:::mdebug_pop()
future.batchtools:::mdebugf_push("pushf")
future.batchtools:::mdebugf_pop()
future.batchtools:::mprint(list(a=1))
future.batchtools:::mstr(list(a=1))

# 15. Utils in utils.R
stopifnot(future.batchtools:::is_na(NA))
stopifnot(!future.batchtools:::is_na(1))

stopifnot(future.batchtools:::is_false(FALSE))
stopifnot(!future.batchtools:::is_false(TRUE))

# hpaste
stopifnot(future.batchtools:::hpaste(1:3) == "1, 2, 3")
stopifnot(future.batchtools:::hpaste(1:10, max_head = 2, max_tail = 1) == "1, 2, ..., 10")

# trim
stopifnot(future.batchtools:::trim("  abc  ") == "abc")

# comma
options(useFancyQuotes = FALSE)
stopifnot(future.batchtools:::comma(1:3) == "1, 2, 3")
stopifnot(future.batchtools:::commaq(1:3) == "'1', '2', '3'")

# file_info / dir_info
tmp_file <- tempfile()
writeLines("test", tmp_file)
info <- future.batchtools:::file_info(tmp_file)
stopifnot(grepl("bytes", info, fixed = TRUE))
stopifnot(grepl("lines", info, fixed = TRUE))
unlink(tmp_file)

info_na <- future.batchtools:::file_info(NA)
stopifnot(info_na == "<NA>")

info_non <- future.batchtools:::file_info("non-existing")
stopifnot(grepl("non-existing", info_non, fixed = TRUE))

tmp_dir <- tempdir()
info_dir <- future.batchtools:::dir_info(tmp_dir)
stopifnot(grepl(tmp_dir, info_dir, fixed = TRUE))

# capture_output
out <- future.batchtools:::capture_output(cat("hello\nworld"))
stopifnot(identical(out, c("hello", "world")))


message("*** Internals ... done")
