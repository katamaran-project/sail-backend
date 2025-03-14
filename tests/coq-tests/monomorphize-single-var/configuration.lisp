$include ../shared-configuration.lisp

(monomorphize "foo" "foo_8" '(("'n" 8)))
(monomorphize "foo" "foo_16" '(("'n" 16)))
(monomorphize "foo" "foo_32" '(("'n" 32)))

(template "base.template.v")
(template "machine.template.v")
(template "base.template.html")
(template "machine.template.html")  
