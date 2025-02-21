(use-list-notations)

(ignore-all-overloads)
(ignore-pragmas "include_start"
                "include_end"
                "file_start"
                "file_end"
                "sail_internal")

(ignore-function-definition-predicate (lambda (identifier)
                                        (or
                                         (string-ends-with? "_of_num" identifier)
                                         (string-starts-with? "num_of_" identifier)
                                         (string-starts-with? "undefined_" identifier)
                                         (string-starts-with? "regval_of_" identifier)
                                         (string-ends-with? "_of_regval" identifier)
                                         (contains? '(
                                                      )
                                                      identifier))))


(ignore-type-definition-predicate (lambda (identifier)
                                    (contains? '(
                                                 "option"
                                                 "register_value"
                                                 )
                                               identifier)))

(ignore-value-definition-predicate
 (lambda (identifier)
   (contains? '(
                )
              identifier)))

(template "base.template.v")
(template "machine.template.v")
(template "base.template.html")
(template "machine.template.html")