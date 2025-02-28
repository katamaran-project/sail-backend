$include ../shared-configuration.lisp

(monomorphize "foo" "foo_8_8" '(
                                ("'n" 8)
                                ("'m" 8)
                                ))

(monomorphize "foo" "foo_4_1" '(
                                ("'n" 4)
                                ("'m" 1)
                                ))

(monomorphize "foo" "foo_1_2" '(
                                ("'n" 1)
                                ("'m" 2)
                                ))

(template "base.template.v")
(template "machine.template.v")
(template "base.template.html")
(template "machine.template.html")  
(template "polymorphic.template.txt")
