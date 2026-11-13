$include ../shared-configuration.lisp

;; The functions Sail generates for the mapping (size_mnemonic_forwards, ..._backwards,
;; ..._matches) compare strings, which has no muSail counterpart. Once the message is
;; dropped, nothing calls them, so they are left out, as the CHERIoT configuration is
;; meant to do with its string mappings. This setting replaces the one in
;; shared-configuration.lisp, so its list is repeated here.
(ignore-function-definition-and-top-level-type-constraints-predicate
 (lambda (identifier)
   (or
    (string-starts-with? "size_mnemonic" identifier)
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
                 "initialize_registers"
                 "__deref"
                 "to_bytes_le"
                 "from_bytes_le")
               identifier))))

(template "base.template.v")
(template "machine.template.v")
(template "base.template.html")
(template "machine.template.html")
