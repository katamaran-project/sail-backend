;; The settings of ../shared-configuration.sexp, except those replaced below (this test replaces some)
(dump-nanosail-ast)
(pretty-print-lists)
(annotate-functions-with-ast)
(include-original-code)
(ignore (overloads))
(ignore (pragmas include_start include_end file_start file_end sail_internal c_reserved))
(ignore (types option register_value bits regstate))
(ignore (values default_capability initial_regstate initial_Capability))

(ignore (functions "*_of_num"
                   "num_of_*"
                   "undefined_*"
                   "regval_of_*"
                   "*_of_regval"
                   eq_unit
                   neq_int
                   neq_bool
                   neq_bits
                   slice_mask
                   _shl_int_general
                   _shr_int_general
                   fdiv_int
                   fmod_int
                   is_none
                   is_some
                   sail_mask
                   sail_ones
                   concat_str_bits
                   concat_str_dec
                   initialize_registers
                   __deref
                   to_bytes_le
                   from_bytes_le))
(templates base.template.v machine.template.v base.template.html machine.template.html)
