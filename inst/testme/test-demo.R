library(future.batchtools)

plan(batchtools_local)

options(future.demo.mandelbrot.nrow = 2L)
options(future.demo.mandelbrot.resolution = 50L)
options(future.demo.mandelbrot.delay = FALSE)

message("*** Demos ...")

message("*** Mandelbrot demo of the 'future' package ...")

demo("mandelbrot", package = "future", ask = FALSE)

message("*** Demos ... DONE")

