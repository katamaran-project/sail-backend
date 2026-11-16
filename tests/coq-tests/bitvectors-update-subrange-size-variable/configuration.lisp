$include ../shared-configuration.lisp

;; As the shared configuration, but __id is translated (Sail's rewrites call it in align_down, as in CHERIoT)
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
                 "sail_mask"
                 "sail_ones"
                 "concat_str_bits"
                 "concat_str_dec"
                 "initialize_registers"
                 "__deref"
                 "to_bytes_le"
                 "from_bytes_le")
               identifier))))

(template "base.template.v")
(template "machine.template.v")
(template "base.template.html")
(template "machine.template.html")
