# TODO

- Look into making the root path used for `immediateCondition` RDS files
  (`immediateConditionsPath()` in `future`, which defaults to
  `file.path(tempdir(), ".future", "immediateConditions")`)
  configurable, e.g. via an R option, rather than hardcoded to
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html) in `future`’s
  `MulticoreFutureBackend` internals (`readImmediateConditions()` /
  `saveImmediateCondition()`).
