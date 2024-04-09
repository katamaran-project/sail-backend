(use-list-notations)
(include-original-code)
; (include-ignored-definitions)

(ignore-pragmas "file_start"
                "include_start"
                "include_end"
                "sail_internal"
                "file_end")

(ignore-functions "eq_unit"
                  "neq_int"
                  "neq_bool"
                  "__id"
                  "neq_bits"
                  "slice_mask"
                  "_shl_int_general"
                  "_shr_int_general"
                  "fdiv_int"
                  "fmod_int"
                  "is_none"
                  "is_some"
                  "read_kind_of_num"
                  "write_kind_of_num"
                  "a64_barrier_domain_of_num"
                  "a64_barrier_type_of_num arg#"
                  "trans_kind_of_num"
                  "a64_barrier_type_of_num"
                  "cache_op_kind_of_num"
                  "perm_bits_forwards"
                  "perm_bits_backwards"
                  "Capability_of_regval"
                  "bit_of_regval"
                  "word_of_regval"
                  )

(ignore-all-overloads)

(ignore-definition-predicate
 (lambda (identifier)
   (contains? '(
                "option"
                "regfp"
                "regfps"
                "niafp"
                "niafps"
                "diafp"
                "diafpConstructor"
                )
              identifier)))

(ignore-value-definition-predicate
 (lambda (identifier)
   (contains? '(
                "default_capability"
                "initial_regstate"
                "initial_Capability"
                )
              identifier)))

(template "template.v" "microsail.v")
