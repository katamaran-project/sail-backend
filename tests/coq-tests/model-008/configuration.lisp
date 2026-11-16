$include ../shared-configuration.lisp

;; The model's read_register and write_register clash with Katamaran's (DefaultRegStoreKit)
(rename '(("read_register" "read_register_model") ("write_register" "write_register_model")))

(template "base.template.v")
(template "machine.template.v")
(template "base.template.html")
(template "machine.template.html")  
