#' FutureError class for errors related to BatchtoolsFutures
#'
#' @param \ldots Arguments passed to [FutureError][future::FutureError].
#'
#' @export
#' @importFrom future FutureError
#'
#' @keywords internal
BatchtoolsFutureError <- function(...) {
  error <- FutureError(...)
  class(error) <- c("BatchtoolsFutureError", class(error))
  error
}
