## To be imported from 'future', if available
readImmediateConditions <- NULL
signalEarly <- NULL
FutureRegistry <- NULL
sQuoteLabel <- NULL
.debug <- NULL

## Import private functions from 'future'
import_future_functions <- function() {
  readImmediateConditions <<- import_future("readImmediateConditions")
  signalEarly <<- import_future("signalEarly")
  FutureRegistry <<- import_future("FutureRegistry")
  
  ## future (>= 1.49.0)
  sQuoteLabel <<- import_future("sQuoteLabel")

  .debug <<- import_future(".debug", mode = "environment", default = new.env(parent = emptyenv()))
}
