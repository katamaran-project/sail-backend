$include ../shared-configuration.lisp

(monomorphize "index"
              "index_5_0"
              '(
                ("'n" 5)
                ("'i" 1)))

(template "base.template.v")
(template "machine.template.v")
(template "base.template.html")
(template "machine.template.html")  
