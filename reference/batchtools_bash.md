# A batchtools Bash backend that resolves futures sequentially via a Bash template script

The `batchtools_bash` backend was added to illustrate how to write a
custom future.batchtools backend that uses a templated job script.
Please see the source code, for details.

## Usage

``` r
batchtools_bash(
  ...,
  template = "bash",
  fs.latency = 0,
  resources = list(),
  delete = getOption("future.batchtools.delete", "on-success")
)

makeClusterFunctionsBash(template = "bash", fs.latency = 0, ...)
```

## Arguments

- template:

  (optional) Name of job-script template to be searched for by
  [`batchtools::findTemplateFile()`](https://batchtools.mlr-org.com/reference/findTemplateFile.html).
  If not found, it defaults to the `templates/bash.tmpl` part of this
  package (see below).

- fs.latency:

  \[`numeric(1)`\]  
  Expected maximum latency of the file system, in seconds. Set to a
  positive number for network file systems like NFS which enables more
  robust (but also more expensive) mechanisms to access files and
  directories. Usually safe to set to `0` to disable the heuristic, e.g.
  if you are working on a local file system.

- resources:

  (optional) A named list passed to the batchtools job-script template
  as variable `resources`. This is based on how
  [`batchtools::submitJobs()`](https://batchtools.mlr-org.com/reference/submitJobs.html)
  works, with the exception for specially reserved names defined by the
  future.batchtools package;

  - `resources[["details"]]`, if TRUE, results in the job script
    outputting job details and job summaries at the beginning and at the
    end.

  - `resources[["startup"]]` and `resources[["shutdown"]]` are character
    vectors of shell code to be injected to the job script as-is.

  - `resources[["modules"]]` is character vector of Linux environment
    modules to be loaded.

  - `resources[["envs"]]`, is an optional names character vector
    specifying environment variables to be set.

  - `resources[["rscript"]]` is an optional character vector specifying
    how the 'Rscript' is launched. The `resources[["rscript_args"]]`
    field is an optional character vector specifying the 'Rscript'
    command-line arguments.

  - `resources[["asis"]]` is a character vector that are passed as-is to
    the job script and are injected as job resource declarations.

  - All remaining `resources` named elements are injected as named
    resource specification for the scheduler.

- delete:

  Controls if and when the batchtools job registry folder is deleted. If
  `"on-success"` (default), it is deleted if the future was resolved
  successfully *and* the expression did not produce an error. If
  `"never"`, then it is never deleted. If `"always"`, then it is always
  deleted.

- ...:

  Not used.

## Value

`makeClusterFunctionsBash()` returns a
[ClusterFunctions](https://batchtools.mlr-org.com/reference/makeClusterFunctions.html)
object.

## Details

Batchtools Bash futures use batchtools cluster functions created by
`makeClusterFunctionsBash()` and requires that `bash` is installed on
the current machine and the `timeout` command is available.

The default template script `templates/bash.tmpl` can be found in:

    system.file("templates", "bash.tmpl", package = "future.batchtools")

and comprises:

    #!/bin/bash
    ######################################################################
    # A batchtools launch script template
    #
    # Author: Henrik Bengtsson
    ######################################################################

    ## Bash settings
    set -e          # exit on error
    set -u          # error on unset variables
    set -o pipefail # fail a pipeline if any command fails
    trap 'echo "ERROR: future.batchtools job script failed on line $LINENO" >&2; exit 1' ERR

    ## Redirect stdout and stderr to the batchtools log file
    exec > <%= log.file %> 2>&1

    <%
      ## Shell "startup" code to evaluate
      startup <- resources[["startup"]]
      resources[["startup"]] <- NULL

      ## Shell "shutdown" code to evaluate
      shutdown <- resources[["shutdown"]]
      resources[["shutdown"]] <- NULL

      ## Environment modules specifications
      modules <- resources[["modules"]]
      resources[["modules"]] <- NULL

      ## Environment variables to be set
      envs <- resources[["envs"]]
      if (length(envs) > 0) {
        stopifnot(is.character(envs), !is.null(names(envs)))
      }
      resources[["envs"]] <- NULL

      ## Custom "Rscript" command and Rscript arguments
      rscript <- resources[["rscript"]]
      if (is.null(rscript)) {
        rscript <- "Rscript"
      } else if (length(rscript) == 0 || !nzchar(rscript)[1]) {
        stop("Argument 'resources' specifies an empty 'rscript' field")
      }
      resources[["rscript"]] <- NULL

      ## Maximum runtime?
      timeout <- resources[["timeout"]]
      resources[["timeout"]] <- NULL
      if (length(timeout) > 0) {
          rscript <- c("timeout", timeout, rscript)
      }
      rscript_args <- resources[["rscript_args"]]
      resources[["rscript_args"]] <- NULL
      rscript_call <- paste(c(rscript, rscript_args), collapse = " ")
    %>

    <% if (length(startup) > 0) {
      writeLines(startup)
    } %>

    <% if (length(modules) > 0) {
      writeLines(c(
        'echo "Load environment modules:"',
        sprintf('echo "- modules: %s"', paste(modules, collapse = ", ")),
        sprintf("module load %s", paste(modules, collapse = " ")),
        "module list"
      ))
    } %>

    <% if (length(envs) > 0) {
      writeLines(c(
        sprintf("echo 'Setting environment variables: [n=%d]'", length(envs)),
        sprintf("echo ' - %s=%s'", names(envs), shQuote(envs)),
        sprintf("export %s=%s", names(envs), shQuote(envs))
      ))
    } %>

    echo "Session information:"
    echo "- timestamp: $(date +"%Y-%m-%d %H:%M:%S%z")"
    echo "- hostname: $(hostname)"
    echo "- Rscript: <%= paste(rscript, collapse = " ") %>"
    echo "- Rscript args: <%= paste(rscript_args, collapse = " ") %>"
    echo "- Rscript call: <%= rscript_call %>"
    if ! command -v <%= rscript[1] %> &> /dev/null; then
        >&2 echo "ERROR: Argument 'resources' specifies a non-existing 'Rscript' launch command: <%= rscript[1] %>. Maybe you need to specify which environment modules to load in the 'resources' argument, e.g. 'plan(future.batchtools::batchtools_slurm, resources = list(modules = c(\"r\")))'. The search PATH for '%<= rscript[1] %>' was ${PATH}"
        exit 1
    fi
    echo "- Rscript version: $(<%= paste(rscript, collapse = " ") %> --version)"
    echo "- R_LIBS_USER=${R_LIBS_USER:-<not set>}"
    echo "- R_LIBS_SITE=${R_LIBS_SITE:-<not set>}"
    echo "- R_LIBS=${R_LIBS:-<not set>}"
    echo "- Rscript library paths: $(<%= rscript_call %> -e "cat(shQuote(.libPaths()), sep = ' ')")"
    echo

    # Launch R and evaluate the batchtools R job
    echo "Calling 'batchtools::doJobCollection()' ..."
    echo "- job name: '<%= job.name %>'"
    echo "- job log file: '<%= log.file %>'"
    echo "- job uri: '<%= uri %>'"
    <%= rscript_call %> -e 'batchtools::doJobCollection("<%= uri %>")'
    res=$?
    echo " - exit code: ${res}"
    echo "Calling 'batchtools::doJobCollection()' ... done"
    echo

    <% if (length(shutdown) > 0) {
      writeLines(shutdown)
    } %>

    echo "End time: $(date +"%Y-%m-%d %H:%M:%S%z")"

    ## Relay the exit code from Rscript
    exit "${res}"

## Examples

``` r
if (FALSE) { # interactive()
library(future)

# Limit runtime to 30 seconds per future
plan(future.batchtools::batchtools_bash, resources = list(runtime = 30))

message("Main process ID: ", Sys.getpid())

f <- future({
  data.frame(
    hostname = Sys.info()[["nodename"]],
          os = Sys.info()[["sysname"]],
       cores = unname(parallelly::availableCores()),
         pid = Sys.getpid(),
     modules = Sys.getenv("LOADEDMODULES")
  )
})
info <- value(f)
print(info)
}
```
