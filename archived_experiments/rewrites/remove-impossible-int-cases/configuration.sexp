(ignore (overloads))
(ignore (pragmas include_start include_end file_start file_end sail_internal))
(ignore (function-definitions "*_of_num"
                              "num_of_*"
                              "undefined_*"
                              "regval_of_*"
                              "*_of_regval"
                              is_some
                              is_none
                              neq_int
                              neq_bits
                              neq_bool
                              sail_mask
                              slice_mask
                              sail_ones
                              concat_str_bits
                              concat_str_dec
                              _shl_int_general
                              _shr_int_general
                              fdiv_int
                              fmod_int))
(ignore (types option register_value bits))
(templates base.template.v machine.template.v base.template.html machine.template.html)
