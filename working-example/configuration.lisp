(use-list-notations)
(include-untranslated-definitions)
(include-original-code)

(ignore-all-overloads)
(ignore-pragmas "include_start"
                "include_end"
                "file_start"
                "file_end"
                "sail_internal")

(ignore-functions "eq_unit"
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
                  "__id")

(ignore-definition-predicate (lambda (identifier)
                               (contains? '("option") identifier)))
