## This test requires a Docker image with R, where the 'batchtools' and
## 'future' packages are installed. It is only run if environment
## variable 'R_FUTURE_BATCHTOOLS_DOCKER_IMAGE' (or R option
## 'future.batchtools.docker.image') specifies an image that is already
## available locally. The image is never pulled. If the packages are
## installed on the host, but not in the image, they can be mounted, e.g.
##
##   lib=$(Rscript -e "cat(.libPaths()[1])")
##   export R_FUTURE_BATCHTOOLS_DOCKER_IMAGE="rocker/r-ver:4.6.1"
##   export R_FUTURE_BATCHTOOLS_DOCKER_IMAGE_ARGS="--volume=${lib}:${lib}:ro --env=R_LIBS_USER=${lib}"
if (.Platform[["OS.type"]] != "windows") {
  library(future)

  message("*** batchtools_docker() ...")

  image <- getOption("future.batchtools.docker.image", NULL)
  image_args <- Sys.getenv("R_FUTURE_BATCHTOOLS_DOCKER_IMAGE_ARGS", "")
  image_args <- strsplit(image_args, split = "[[:space:]]+")[[1]]
  image_args <- image_args[nzchar(image_args)]

  supports_docker <- FALSE
  if (!is.null(image) && nzchar(Sys.which("docker"))) {
    res <- suppressWarnings(system2("docker", args = c("image", "inspect", shQuote(image)), stdout = FALSE, stderr = FALSE))
    supports_docker <- (res == 0L)
  }
  message("Docker image: ", if (is.null(image)) "<none>" else sQuote(image))
  message("Supports batchtools_docker: ", supports_docker)

  if (supports_docker) {
    plan(future.batchtools::batchtools_docker, image = image, image.args = image_args, workers = 2L)
    print(plan("backend"))

    message("future(42L) ...")
    f <- future(42L)
    v <- value(f)
    stopifnot(identical(v, 42L))

    message("future(a) ...")
    a0 <- a <- 42
    f <- future(a)
    stopifnot(identical(f$globals$a, a0))
    v <- value(f)
    stopifnot(identical(v, a0))

    message("future(a, lazy = TRUE) ...")
    a0 <- a <- 42
    f <- future(a, lazy = TRUE)
    rm(list = "a")
    stopifnot(identical(f$globals$a, a0))
    v <- value(f)
    stopifnot(identical(v, a0))

    message("future(stop()) ...")
    f <- future(stop("boom"))
    res <- tryCatch(value(f), error = identity)
    stopifnot(inherits(res, "error"), conditionMessage(res) == "boom")

    message("Running in separate container ...")
    f <- future(Sys.info()[["nodename"]])
    v <- value(f)
    print(v)
    stopifnot(is.character(v), v != Sys.info()[["nodename"]])

    message("Concurrent futures ...")
    fs <- lapply(1:3, FUN = function(x) future(x^2))
    vs <- value(fs)
    stopifnot(identical(vs, list(1, 4, 9)))

    message("listJobsRunning() ...")
    f <- future({ Sys.sleep(5); 42L })
    reg <- f$config$reg
    batch_id <- reg$status$batch.id
    cf <- plan("backend")[["cluster.functions"]]
    ids <- cf$listJobsRunning(reg)
    print(ids)
    stopifnot(is.character(ids), batch_id %in% ids)
    v <- value(f)
    stopifnot(identical(v, 42L))

    plan(sequential)
  } ## if (supports_docker)

  message("*** batchtools_docker() ... DONE")
}
