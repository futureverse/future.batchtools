# Render a batchtools job script without submitting it

Renders the job script that batchtools would submit to the scheduler for
a given batchtools future strategy, without actually submitting (or even
creating) a real job. This is useful for inspecting and troubleshooting
a template, e.g. to see exactly how `resources` are expanded into
scheduler directives.

## Usage

``` r
render_template(strategy = NULL, ...)
```

## Arguments

- strategy:

  A batchtools future strategy as accepted by
  [`future::plan()`](https://future.futureverse.org/reference/plan.html),
  e.g.
  [`future.batchtools::batchtools_sge`](https://future.batchtools.futureverse.org/reference/batchtools_sge.md)
  or
  `future::tweak(future.batchtools::batchtools_sge, resources = list(slots = 8))`.
  If `NULL` (default), the currently set future plan is used.

- ...:

  Named resources overriding those already set by `strategy`, e.g.
  `render_template(future.batchtools::batchtools_slurm, time = "00:05:00")`.

## Value

A character string holding the rendered job script.

## Examples

``` r
script <- render_template(
  future.batchtools::batchtools_slurm,
  time = "00:05:00", mem = "200M"
)
cat(script)
#> #!/bin/bash
#> ######################################################################
#> # A batchtools launch script template for Slurm
#> #
#> # Author: Henrik Bengtsson
#> ######################################################################
#> ## Job name
#> #SBATCH --job-name=joba6b1120b21c31df4c53738c8fdaca492
#> ## Direct streams to logfile
#> #SBATCH --output=/tmp/hb/Rtmpgqyf5x/future.batchtools_template_1fe0e86a2edf05/logs/joba6b1120b21c31df4c53738c8fdaca492.log
#> ## Resources needed
#> #SBATCH --time=00:05:00
#> #SBATCH --mem=200M
#> 
#> ## Bash settings
#> set -e          # exit on error
#> set -u          # error on unset variables
#> set -o pipefail # fail a pipeline if any command fails
#> trap 'echo "ERROR: future.batchtools job script failed on line $LINENO" >&2; exit 1' ERR
#> echo 'Job submission declarations:'
#> echo '#SBATCH --time=00:05:00'
#> echo '#SBATCH --mem=200M'
#> echo
#> 
#> 
#> 
#> 
#> 
#> echo "Session information:"
#> echo "- timestamp: $(date +"%Y-%m-%d %H:%M:%S%z")"
#> echo "- hostname: $(hostname)"
#> echo "- Rscript call: Rscript"
#> if ! command -v Rscript &> /dev/null; then
#> >&2 echo "ERROR: Argument 'resources' specifies a non-existing 'Rscript' launch command: Rscript. Maybe you need to specify which environment modules to load in the 'resources' argument, e.g. 'plan(future.batchtools::batchtools_slurm, resources = list(modules = c(\"r\")))'. The search PATH for 'Rscript' was ${PATH}"
#> exit 1
#> fi
#> echo "- Rscript version: $(Rscript --version)"
#> echo "- R_LIBS_USER=${R_LIBS_USER:-<not set>}"
#> echo "- R_LIBS_SITE=${R_LIBS_SITE:-<not set>}"
#> echo "- R_LIBS=${R_LIBS:-<not set>}"
#> echo "- Rscript library paths: $(Rscript -e "cat(shQuote(.libPaths()), sep = ' ')")"
#> echo
#> ## Launch R and evaluate the batchtools R job
#> echo "Calling 'batchtools::doJobCollection()' ..."
#> echo "- job name: 'joba6b1120b21c31df4c53738c8fdaca492'"
#> echo "- job log file: '/tmp/hb/Rtmpgqyf5x/future.batchtools_template_1fe0e86a2edf05/logs/joba6b1120b21c31df4c53738c8fdaca492.log'"
#> echo "- job uri: '/tmp/hb/Rtmpgqyf5x/future.batchtools_template_1fe0e86a2edf05/jobs/joba6b1120b21c31df4c53738c8fdaca492.rds'"
#> Rscript -e 'batchtools::doJobCollection("/tmp/hb/Rtmpgqyf5x/future.batchtools_template_1fe0e86a2edf05/jobs/joba6b1120b21c31df4c53738c8fdaca492.rds")'
#> res=$?
#> echo " - exit code: ${res}"
#> echo "Calling 'batchtools::doJobCollection()' ... done"
#> echo
#> 
#> 
#> echo "End time: $(date +"%Y-%m-%d %H:%M:%S%z")"
#> ## Relay the exit code from Rscript
#> exit "${res}"
```
