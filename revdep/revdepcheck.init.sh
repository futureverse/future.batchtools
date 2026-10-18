#! /usr/bin/env bash

## Missing or outdated LaTeX packages
false && R --quiet --no-save <<EOF
    tinytex::install_tinytex(force = TRUE)
    message("TeX root: ", tinytex::tinytex_root())
    tinytex::tlmgr_update()
EOF


## ---------------------------------------------------------------------
## Phase 1
## ---------------------------------------------------------------------

## Add packages to check
revdep/run.R --add-children

## Drop packages failing on CRAN (2026-10-08)
#revdep/run.R --rm ...

## Drop packages failing on Bioconductor (2026-10-10)
# revdep/run.R --rm ...

## Drop packages no longer on CRAN (2026-10-08)
#revdep/run.R --rm ...

## Drop packages no longer on Bioconductor (2026-10-08)
#revdep/run.R --rm ...

## Too many cores due to detectCores() - fully skip
pkgs_detectCores=()
if [[ ${#pkgs_detectCores[@]} -gt 0 ]]; then
    revdep/run.R --rm "${pkgs_detectCores[@]}"
fi

## Too many threads
pkgs_threads=()
if [[ ${#pkgs_threads[@]} -gt 0 ]]; then
    revdep/run.R --rm "${pkgs_threads[@]}"
fi

## Too many cores
pkgs_cores=()
if [[ ${#pkgs_cores[@]} -gt 0 ]]; then
    revdep/run.R --rm "${pkgs_cores[@]}"
fi

## Requires sequential processing due to clashes, e.g. port and cache 
pkgs_seq=()
if [[ ${#pkgs_seq[@]} -gt 0 ]]; then
    revdep/run.R --rm "${pkgs_seq[@]}"
fi

## Run revdep check
revdep/run.R


## ---------------------------------------------------------------------
## Phase 2
## ---------------------------------------------------------------------
 ## Set: Too many threads
if [[ ${#pkgs_threads[@]} -gt 0 ]]; then
  revdep/run.R --add "${pkgs_threads[@]}"
  OMP_NUM_THREADS=4 revdep/run.R
fi

## Set: Too many cores
if [[ ${#pkgs_cores[@]} -gt 0 ]]; then
    revdep/run.R --add "${pkgs_cores[@]}"
    NSLOTS=4 revdep/run.R
fi

## Sequential
if [[ ${#pkgs_seq[@]} -gt 0 ]]; then
  revdep/run.R --add "${pkgs_seq[@]}"
  NSLOTS=1 revdep/run.R
fi
