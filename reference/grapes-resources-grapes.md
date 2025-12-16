# Temporarily tweaks the resources for the current batchtools strategy

Temporarily tweaks the resources for the current batchtools strategy

## Usage

``` r
fassignment %resources% tweaks
```

## Arguments

- fassignment:

  The future assignment, e.g. `x %<-% { expr }`.

- tweaks:

  A named list (or vector) of resource batchtools parameters (see
  Section 'Resources' in
  [`batchtools::submitJobs()`](https://batchtools.mlr-org.com/reference/submitJobs.html))
  that should be changed relative to the current strategy.
