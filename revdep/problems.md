# targets (1.12.0)

* GitHub: <https://github.com/ropensci/targets>
* Email: <mailto:will.landau.oss@gmail.com>
* GitHub mirror: <https://github.com/cran/targets>

Run `revdepcheck::revdep_details(, "targets")` for more info

## In both

*   checking examples ... ERROR
     ```
     Running examples in ‘targets-Ex.R’ failed
     The error most likely occurred in:
     
     > ### Name: tar_renv
     > ### Title: Set up package dependencies for compatibility with 'renv'
     > ### Aliases: tar_renv
     > 
     > ### ** Examples
     > 
     > tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
     +   tar_script({
     +     library(targets)
     +     library(tarchetypes)
     +     tar_option_set(packages = c("tibble", "qs"))
     +     list()
     +   }, ask = FALSE)
     +   tar_renv()
     +   writeLines(readLines("_targets_packages.R"))
     + })
     Error:
     ! Error in tar_renv():
       there is no package called ‘tarchetypes’
       See https://books.ropensci.org/targets/debugging.html
     Execution halted
     ```

