# Removes a batchtools future

Removes a batchtools future

## Usage

``` r
# S3 method for class 'BatchtoolsFuture'
delete(
  future,
  onRunning = c("warning", "error", "skip"),
  onFailure = c("error", "warning", "ignore"),
  onMissing = c("ignore", "warning", "error"),
  times = 10L,
  ...
)
```

## Arguments

- future:

  The future.

- onRunning:

  Action if future is running or appears to run.

- onFailure:

  Action if failing to delete future.

- onMissing:

  Action if future does not exist.

- times:

  The number of tries before giving up.

- ...:

  Not used.

## Value

(invisibly) TRUE if deleted and FALSE otherwise.
