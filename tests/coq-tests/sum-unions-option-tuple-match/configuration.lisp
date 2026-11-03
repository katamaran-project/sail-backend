$include ../shared-configuration.lisp

;; The shared configuration ignores option; here we want it translated
(ignore-type-definition-predicate (lambda (identifier)
                                    (contains? '("register_value"
                                                 "bits"
                                                 "regstate")
                                               identifier)))

(translate-union-as-sum "option" "Some" "None")

(template "base.template.v")
(template "machine.template.v")
(template "base.template.html")
(template "machine.template.html")
