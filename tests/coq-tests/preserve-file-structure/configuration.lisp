$include ../shared-configuration.lisp

(preserve-file-structure)

; As in shared-configuration.lisp, except that neq_bool, a function of Sail's library, is translated:
; with preserve-file-structure, it goes to machine.v
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
                 "initialize_registers"
                 "__deref"
                 "to_bytes_le"
                 "from_bytes_le")
               identifier))))

(template "base.template.v")
(template "machine.template.v")
