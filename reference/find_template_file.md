# Locate a batchtools job-script template file

Locate a batchtools job-script template file

## Usage

``` r
find_template_file(template)
```

## Arguments

- template:

  (character string) The name of a batchtools template, e.g. `"sge"` or
  `"slurm"`, or the full pathname to a template file.

## Value

(character string) The absolute pathname to the located template file.

## Examples

``` r
pathname <- find_template_file("sge")
print(pathname)
#> [1] "/tmp/hb/RtmpyTCEIt/temp_libpath3b36d255245b0/future.batchtools/templates/sge.tmpl"
```
