;; The settings of ../shared-configuration.sexp, except those replaced below (this test replaces some)
(dump-nanosail-ast)
(pretty-print-lists)
(annotate-functions-with-ast)
(include-original-code)
(ignore (overloads))
(ignore (values default_capability initial_regstate initial_Capability))

(ignore (types register_value bits regstate))
(ignore (pragmas include_start include_end file_start file_end sail_internal c_reserved option))
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
                   is_ok
                   is_err
                   ok_option
                   err_option
                   unwrap_or
                   __id
                   sail_mask
                   sail_ones
                   concat_str_bits
                   concat_str_dec
                   initialize_registers
                   __deref
                   to_bytes_le
                   from_bytes_le))
(union-as-sum (option Some None))
(union-as-sum (result Ok Err))
(templates base.template.v machine.template.v base.template.html machine.template.html)
