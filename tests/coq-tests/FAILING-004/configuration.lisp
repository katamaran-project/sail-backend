(use-list-notations)

(ignore-all-overloads)
(ignore-pragmas "include_start"
                "include_end"
                "file_start"
                "file_end"
                "sail_internal")

((ignore-function-definition-predicate (lambda (identifier)
                                        (or
                                         (string-ends-with? "_of_num" identifier)
                                         (string-starts-with? "num_of_" identifier)
                                         (string-starts-with? "undefined_" identifier)
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
                                                      "bit_of_regval"
                                                      "__id"
                                                      "Capability_of_regval"
                                                      "sail_mask"
                                                      "sail_ones"
                                                      "concat_str_bits"
                                                      "concat_str_dec"
                                                      "regval_of_bit")
                                                    identifier))))
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
                  "bit_of_regval"
                  "num_of_foo"
                  "__id"
                  "Capability_of_regval"
                  "sail_mask"
                  "sail_ones"
                  "concat_str_bits"
                  "concat_str_dec"
                  "regval_of_bit"
                  "MyEnum_of_num"
                  "num_of_MyEnum"
                  "MyOtherEnum_of_num"
                  "num_of_MyOtherEnum"
                  "Capability_of_regval"
                  "regval_of_Capability"
                  "bit_of_regval"
                  "regval_of_bit"
                  "word_of_regval"
                  "regval_of_word"
                  "Permission_of_num"
                  "num_of_Permission"
                  )

(ignore-type-definition-predicate (lambda (identifier)
                               (contains? '(
                                            "option"
                                            "register_value"
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

(template "base.template.v")
(template "machine.template.v")

