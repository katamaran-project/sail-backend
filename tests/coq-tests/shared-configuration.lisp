(use-list-notations)

(ignore-all-overloads)
(ignore-pragmas (lambda (identifier)
                  (contains? '("include_start"
                               "include_end"
                               "file_start"
                               "file_end"
                               "sail_internal")
                             identifier)))

(ignore-function-definition-and-top-level-type-constraints-predicate
 (lambda (identifier)
   (or
    (string-ends-with? "_of_num" identifier)
    (string-starts-with? "num_of_" identifier)
    (string-starts-with? "undefined_" identifier)
    (string-starts-with? "regval_of_" identifier)
    (string-ends-with? "_of_regval" identifier)
    (contains? '(
                 "eq_unit"
                 "neq_int"
                 "neq_bool"
                 "neq_bits"
                 "slice_mask"
                 "_shl_int_general"
                 "_shr_int_general"
                 "fdiv_int"
                 "fmod_int"
                 "is_none"
                 "is_some"
                 "__id"
                 "sail_mask"
                 "sail_ones"
                 "concat_str_bits"
                 "concat_str_dec"
                 "__deref")
               identifier))))
                                        

(ignore-type-definition-predicate (lambda (identifier)
                                    (contains? '(
                                                 "option"
                                                 "register_value"
                                                 "bits"
                                                 "regstate")
                                               identifier)))

(ignore-value-definition-predicate
 (lambda (identifier)
   (contains? '(
                "default_capability"
                "initial_regstate"
                "initial_Capability"
                )
              identifier)))
