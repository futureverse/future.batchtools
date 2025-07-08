library(future.batchtools)

message("*** %resources% ...")

plan(batchtools_local)

## This will test `%resources%` but it'll be ignored (with a warning)
## by batchtools_local()
y %<-% { 42 } %resources% list(memory = 16000) ## 16,000 MiB of memory

message("*** %resources% ... DONE")

