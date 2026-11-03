#' @tags batch_id

library(future.batchtools)

message("*** Validation of scheduler job IDs ...")

is_valid_batch_id <- future.batchtools:::is_valid_batch_id

## Valid IDs, including Torque IDs with a host name
for (id in c("1234", "1234.5", "1234.pbs-server", "1234.head.cluster.edu")) {
  stopifnot(is_valid_batch_id(id))
}

## Invalid IDs, e.g. anything that is not safe on a shell command line
for (id in list("", NA_character_, "abc", "1234; ls", "1234 --foo",
                "1234.", "1234.$(id)", "1234\n5", 1234, c("1", "2"), NULL)) {
  stopifnot(!is_valid_batch_id(id))
}

message("*** Validation of scheduler job IDs ... done")
