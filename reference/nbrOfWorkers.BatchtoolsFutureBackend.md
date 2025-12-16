# Gets the number of batchtools workers

Tries to infer the total number of batchtools workers. This is done
using various ad-hoc procedures based on code inspection of batchtools
itself.

## Usage

``` r
# S3 method for class 'BatchtoolsFutureBackend'
nbrOfWorkers(evaluator)
```

## Arguments

- evaluator:

  A future evaluator function. If NULL (default), the current evaluator
  as returned by
  [`future::plan()`](https://future.futureverse.org/reference/plan.html)
  is used.

## Value

A number in \\\[1, Inf\]\\.
