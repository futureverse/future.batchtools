#' @importFrom checkmate assertCharacter assertString %??%
#' @importFrom stringi stri_replace_all_fixed stri_flatten
runOSCommand = function(sys.cmd, sys.args = character(0L), stdin = "", nodename = "localhost") {
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
    res = suppressWarnings(system2(command = sys.cmd, args = sys.args, stdin = stdin, stdout = TRUE, stderr = TRUE, wait = TRUE))
    output = as.character(res)
    exit.code = attr(res, "status") %??% 0L
  } else {
    output = "command not found"
    exit.code = 127L
  }

  "!DEBUG [runOSCommand]: OS result (stdin '`stdin`', exit code `exit.code`):"
  "!DEBUG [runOSCommand]: `paste0(output, sep = '\n')`"

  return(list(sys.cmd = sys.cmd, sys.args = sys.args, exit.code = exit.code, output = output))
}

isLocalHost = function(nodename) {
  is.null(nodename) || nodename %chin% c("localhost", "127.0.0.1", "::1")
}
