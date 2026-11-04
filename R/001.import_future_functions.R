## Private functions imported from 'future'
assertOwner <- import_future("assertOwner")
readImmediateConditions <- import_future("readImmediateConditions")
immediateConditionsPath <- import_future("immediateConditionsPath")
fileImmediateConditionHandler <- import_future("fileImmediateConditionHandler")
getFutureBackendConfigs <- import_future("getFutureBackendConfigs")
signalEarly <- import_future("signalEarly")
FutureRegistry <- import_future("FutureRegistry")
with_stealth_rng <- import_future("with_stealth_rng")
getFutureData <- import_future("getFutureData")
sQuoteLabel <- import_future("sQuoteLabel")

## An environment must be imported when the package is loaded, otherwise
## we would get a copy and not share the state with 'future'
.debug <- NULL

import_future_functions <- function() {
  .debug <<- import_future(".debug", mode = "environment")

  ## The generic is a private function in 'future', so the S3method()
  ## entry in NAMESPACE is not enough to register the method
  registerS3method("getFutureBackendConfigs", "BatchtoolsFuture", getFutureBackendConfigs.BatchtoolsFuture)
}
