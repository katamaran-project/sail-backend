(include ../shared-configuration.sexp)

(exclude (as-no-op print_line))
(exclude (as-unconstrained show))
(exclude (registers scratch))
(templates base.template.v machine.template.v base.template.html machine.template.html)
