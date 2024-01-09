(use-list-notations)
(include-untranslated-definitions)
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
                  )

(ignore-all-overloads)
