;; dropped the obsolete option (use-list-notations ...): the plugin no longer has it
(ignore (overloads))
(ignore (pragmas include_start include_end file_start file_end sail_internal))
(ignore (function-definitions "*_of_num"
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
                              __id
                              sail_mask
                              sail_ones
                              concat_str_bits
                              concat_str_dec))
(ignore (types option register_value bits regstate))
(ignore (values default_capability initial_regstate initial_Capability))
(templates base.template.v machine.template.v base.template.html machine.template.html)
