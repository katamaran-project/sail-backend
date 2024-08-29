open Base


let log message =
  ignore @@ Stdio.printf "%s" message


let info message =
  log message
