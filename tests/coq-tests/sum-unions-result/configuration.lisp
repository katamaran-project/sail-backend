$include ../shared-configuration.lisp

;; The shared configuration ignores option; here we want it translated
(ignore-type-definition-predicate (lambda (identifier)
                                    (contains? '("register_value"
                                                 "bits"
                                                 "regstate")
                                               identifier)))

;; result.sail contains $option pragmas
(ignore-pragmas (lambda (identifier)
                  (contains? '("include_start"
                               "include_end"
                               "file_start"
                               "file_end"
                               "sail_internal"
                               "c_reserved"
                               "option")
                             identifier)))

;; result.sail defines functions that are polymorphic in types, which muSail does not support.
;; This replaces the shared configuration's predicate, so we repeat what it ignores.
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
                 "is_ok"
                 "is_err"
                 "ok_option"
                 "err_option"
                 "unwrap_or"
                 "__id"
                 "sail_mask"
                 "sail_ones"
                 "concat_str_bits"
                 "concat_str_dec"
                 "initialize_registers"
                 "__deref"
                 "to_bytes_le"
                 "from_bytes_le")
               identifier))))

(translate-union-as-sum "option" "Some" "None")
(translate-union-as-sum "result" "Ok" "Err")

(template "base.template.v")
(template "machine.template.v")
(template "base.template.html")
(template "machine.template.html")
