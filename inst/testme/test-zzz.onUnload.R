# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Load and unload of package
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
loadNamespace("future.batchtools")

message("*** .onUnload() ...")

libpath <- dirname(system.file(package = "future.batchtools"))
future.batchtools:::.onUnload(libpath)

message("*** .onUnload() ... DONE")

