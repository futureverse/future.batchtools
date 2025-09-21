#' @importFrom checkmate assertCharacter assertString %??%
#' @importFrom stringi stri_replace_all_fixed stri_flatten
runOSCommand = function(sys.cmd, sys.args = character(0L), stdin = "", stdout = TRUE, stderr = TRUE, nodename = "localhost") {
  isLocalHost <- import_from("isLocalHost", package = "batchtools")
  assertCharacter(sys.cmd, any.missing = FALSE, len = 1L)
  assertCharacter(sys.args, any.missing = FALSE)
  assertString(nodename, min.chars = 1L)

  if (!isLocalHost(nodename)) {
    command = sprintf("%s %s", sys.cmd, stri_flatten(sys.args, " "))
    if (getRversion() < "4.0.0") {
      command = shQuote(command)
    }
    command = stri_replace_all_fixed(command, "\\$", "$")
    sys.args = c("-q", nodename, command)
    sys.cmd = "ssh"
  }

  "!DEBUG [runOSCommand]: cmd: `sys.cmd` `stri_flatten(sys.args, ' ')`"

  if (nzchar(Sys.which(sys.cmd))) {
    ## Capture stderr separately
    if (is.na(stderr)) {
      stderr_file = tempfile()
      on.exit(file.remove(stderr_file))
    } else {
      stderr_file = stderr
    }
    res = suppressWarnings(system2(command = sys.cmd, args = sys.args, stdin = stdin, stdout = stdout, stderr = stderr_file, wait = TRUE))
    output = as.character(res)
    exit.code = attr(res, "status") %??% 0L
    if (is.na(stderr)) {
      output_stderr = readLines(stderr_file, warn = FALSE)
      if (length(output_stderr) > 0 && exit.code > 0) {
        warning(sprintf("%s return with exit code %d and a standard error message:\n%s", sQuote(sys.cmd), exit.code, paste(output_stderr, collapse = "\n")), immediate. = TRUE)
      }
    } else {
      output_stderr = NULL
    }
  } else {
    output = "command not found"
    output_stderr = NULL
    exit.code = 127L
  }

  "!DEBUG [runOSCommand]: OS result (stdin '`stdin`', exit code `exit.code`):"
  "!DEBUG [runOSCommand]: `paste0(output, sep = '\n')`"
  "!DEBUG [runOSCommand]: `paste0(output_stderr, sep = '\n')`"

  return(list(sys.cmd = sys.cmd, sys.args = sys.args, exit.code = exit.code, output = output, stderr = output_stderr))
}
