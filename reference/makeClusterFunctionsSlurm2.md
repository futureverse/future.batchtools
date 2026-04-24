# ClusterFunctions for Slurm Systems (patched)

This function enhances
[`batchtools::makeClusterFunctionsSlurm()`](https://batchtools.mlr-org.com/reference/makeClusterFunctionsSlurm.html)
with a few patches. Firstly, it patches the `listJobsQueued()` cluster
function such that it falls back to querying Slurm's account database
(`sacct`), if the future was *not* found in the Slurm job queue
(`squeue`), which might be the case when Slurm provisions a job that was
just submitted to the scheduler. Secondly, it patches the `submitJob()`
cluster function such that the system call to `sbatch` does not capture
stderr together with stdout, but rather separately such that any extra
INFO messages from `sbatch` do not corrupt the output intended to come
from stdout only.

## Usage

``` r
makeClusterFunctionsSlurm2(
  template = "slurm",
  array.jobs = TRUE,
  nodename = "localhost",
  scheduler.latency = 1,
  fs.latency = 65
)
```

## Arguments

- template:

  \[`character(1)`\]\
  Either a path to a brew template file (with extension “tmpl”), or a
  short descriptive name enabling the following heuristic for the file
  lookup:

  1.  “batchtools.\[template\].tmpl” in the path specified by the
      environment variable “R_BATCHTOOLS_SEARCH_PATH”.

  2.  “batchtools.\[template\].tmpl” in the current working directory.

  3.  “\[template\].tmpl” in the user config directory (see
      [`user_config_dir`](https://rappdirs.r-lib.org/reference/user_data_dir.html));
      on linux this is usually “~/.config/batchtools/\[template\].tmpl”.

  4.  “.batchtools.\[template\].tmpl” in the home directory.

  5.  “\[template\].tmpl” in the package installation directory in the
      subfolder “templates”.

- array.jobs:

  \[`logical(1)`\]\
  If array jobs are disabled on the computing site, set to `FALSE`.

- nodename:

  \[`character(1)`\]\
  Nodename of the master host. All commands are send via SSH to this
  host. Only works iff

  1.  Passwordless authentication (e.g., via SSH public key
      authentication) is set up.

  2.  The file directory is shared across machines, e.g. mounted via
      SSHFS.

  3.  Either the absolute path to the `file.dir` is identical on the
      machines, or paths are provided relative to the home directory.
      Symbolic links should work.

- scheduler.latency:

  \[`numeric(1)`\]\
  Time to sleep after important interactions with the scheduler to
  ensure a sane state. Currently only triggered after calling
  [`submitJobs`](https://batchtools.mlr-org.com/reference/submitJobs.html).

- fs.latency:

  \[`numeric(1)`\]\
  Expected maximum latency of the file system, in seconds. Set to a
  positive number for network file systems like NFS which enables more
  robust (but also more expensive) mechanisms to access files and
  directories. Usually safe to set to `0` to disable the heuristic, e.g.
  if you are working on a local file system.

## Value

A
[batchtools::ClusterFunctions](https://batchtools.mlr-org.com/reference/makeClusterFunctions.html)
object.
