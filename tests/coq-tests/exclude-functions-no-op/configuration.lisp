(exclude-functions-as-no-op '("print_reg" "trace_write"))

$include ../shared-configuration.lisp

(template "base.template.v")
(template "machine.template.v")
(template "base.template.html")
(template "machine.template.html")
