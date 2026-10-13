#' Locate a batchtools job-script template file
#'
#' @param template (character string) The name of a batchtools template,
#' e.g. `"sge"` or `"slurm"`, or the full pathname to a template file.
#'
#' @return (character string) The absolute pathname to the located
#' template file.
#'
#' @examples
#' pathname <- find_template_file("sge")
#' print(pathname)
#'
#' @importFrom batchtools findTemplateFile
#' @importFrom utils file_test
#' @export
find_template_file <- function(template) {
  pathname <- findTemplateFile(template)
  if (is.na(pathname)) {
    pathname <- system.file("templates", sprintf("%s.tmpl", template),
                            package = .packageName)
    if (!file_test("-f", pathname)) pathname <- NA_character_
  }
  if (is.na(pathname)) {
    stopf("Failed to locate a batchtools template file: *%s.tmpl", template)
  }
  pathname
}
