## To be imported from 'future', if available
assertOwner <- NULL
readImmediateConditions <- NULL
signalEarly <- NULL
FutureRegistry <- NULL
sQuoteLabel <- NULL
with_stealth_rng <- NULL
getFutureData <- NULL

.debug <- NULL

## Import private functions from 'future'
import_future_functions <- function() {
  assertOwner <<- import_future("assertOwner")
  readImmediateConditions <<- import_future("readImmediateConditions")
  signalEarly <<- import_future("signalEarly")
  FutureRegistry <<- import_future("FutureRegistry")
  with_stealth_rng <<- import_future("with_stealth_rng")
  getFutureData <<- import_future("getFutureData")  
  
  ## future (>= 1.49.0)
  sQuoteLabel <<- import_future("sQuoteLabel")

  .debug <<- import_future(".debug", mode = "environment", default = new.env(parent = emptyenv()))
}
