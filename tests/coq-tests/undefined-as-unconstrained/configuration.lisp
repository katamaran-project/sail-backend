;; Sail's undefined values (calls to undefined_<type> after Sail's rewrites) are unconstrained:
;; universally quantified, never a fixed default
(exclude-functions-as-unconstrained '("undefined_color" "undefined_pair"
                                      "undefined_bitvector" "undefined_bool" "undefined_int"))

$include ../shared-configuration.lisp

(template "base.template.v")
(template "machine.template.v")
(template "base.template.html")
(template "machine.template.html")
