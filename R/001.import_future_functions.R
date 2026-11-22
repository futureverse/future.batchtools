## Private functions imported from 'future'
assertOwner <- import_future("assertOwner")
readImmediateConditions <- import_future("readImmediateConditions")
immediateConditionsPath <- import_future("immediateConditionsPath")
fileImmediateConditionHandler <- import_future("fileImmediateConditionHandler")
getFutureBackendConfigs <- import_future("getFutureBackendConfigs")
signalEarly <- import_future("signalEarly")
with_stealth_rng <- import_future("with_stealth_rng")
getFutureData <- import_future("getFutureData")
sQuoteLabel <- import_future("sQuoteLabel")
stopf <- import_future("stopf")
warnf <- import_future("warnf")
stop_if_not <- import_future("stop_if_not")
assert_no_positional_args_but_first <- import_future("assert_no_positional_args_but_first")
hpaste <- import_future("hpaste")
trim <- import_future("trim")
commaq <- import_future("commaq")

## Debug functions imported from 'future'. Their output is controlled by
## argument 'debug', which defaults to R option 'future.debug'. Here, all
## calls are already conditioned on 'debug' by the caller, so we default
## to 'debug = TRUE'
import_future_debug <- function(name) {
  fcn <- import_future(name)
  formals(fcn)[["debug"]] <- TRUE
  fcn
}
mdebug <- import_future_debug("mdebug")
mdebugf <- import_future_debug("mdebugf")
mdebug_push <- import_future_debug("mdebug_push")
mdebugf_push <- import_future_debug("mdebugf_push")
mdebug_pop <- import_future_debug("mdebug_pop")
mdebugf_pop <- import_future_debug("mdebugf_pop")
mprint <- import_future_debug("mprint")
mstr <- import_future_debug("mstr")

## An environment must be imported when the package is loaded, otherwise
## we would get a copy and not share the state with 'future'
.debug <- NULL

## Same for FutureRegistry(), whose state lives in its closure. It must
## be shared with 'future', e.g. so that listFutures() sees our futures
FutureRegistry <- NULL

import_future_functions <- function() {
  .debug <<- import_future(".debug", mode = "environment")
  FutureRegistry <<- import_future("FutureRegistry")

  ## Use future:::waitForWorker(), if available, otherwise our local copy.
  ## This is done when the package is loaded, so that the one in the
  ## installed version of 'future' is used
  waitForWorker <<- import_future("waitForWorker", default = waitForWorker)

  ## The generic is a private function in 'future', so the S3method()
  ## entry in NAMESPACE is not enough to register the method
  registerS3method("getFutureBackendConfigs", "BatchtoolsFuture", getFutureBackendConfigs.BatchtoolsFuture)
}
