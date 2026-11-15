(exclude-functions-as-no-op '("print_line"))
(exclude-functions-as-unconstrained '("show"))
(exclude-registers '("scratch"))

$include ../shared-configuration.lisp

(template "base.template.v")
(template "machine.template.v")
(template "base.template.html")
(template "machine.template.html")
