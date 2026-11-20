(include ../shared-configuration.sexp)

(rename (read_register read_register_model)
        (write_register write_register_model))
(templates base.template.v machine.template.v base.template.html machine.template.html)
