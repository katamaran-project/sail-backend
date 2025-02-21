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
                                                      "is_some"
                                                      "is_none"
                                                      "neq_int"
                                                      "neq_bits"
                                                      "neq_bool"
                                                      "sail_mask"
                                                      "slice_mask"
                                                      "sail_ones"
                                                      "concat_str_bits"
                                                      "concat_str_dec"
                                                      "_shl_int_general"
                                                      "_shr_int_general"
                                                      "fdiv_int"
                                                      "fmod_int"
                                                      )
                                                      identifier))))


(ignore-type-definition-predicate (lambda (identifier)
                                    (contains? '(
                                                 "option"
                                                 "register_value"
                                                 "bits"
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