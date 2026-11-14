(dump-nanosail-ast)

(pretty-print-lists)

;(annotate-functions-with-ast)

;; Overloads are resolved by Sail's type checker, so the translation needs
;; none of them. The pragmas below define nothing: they mark file boundaries,
;; or are directives for Sail's other backends ($option -lem_extern_type,
;; $target_set, $c_reserved, $abstract on the extern sail_barrier).
(ignore-all-overloads)
(ignore-pragmas (lambda (identifier)
                  (contains? '("include_start"
                               "include_end"
                               "file_start"
                               "file_end"
                               "sail_internal"
                               "option"
                               "target_set"
                               "c_reserved"
                               "abstract")
                             identifier)))

;; Types that no translated CHERIoT code uses.
(ignore-type-definition-predicate
 (lambda (identifier)
   (contains? '(
                ;; Sail's concurrency interface (lib/concurrency_interface):
                ;; only its own helpers (mem_read_request_is_exclusive, ...)
                ;; use these, and those are outside the scope. Access_kind
                ;; is a union over a type parameter, which muSail lacks.
                "Access_variety"
                "Access_strength"
                "Explicit_access_kind"
                "Access_kind"
                "Mem_read_request"
                "Mem_write_request"
                ;; Sail's lib/regfp.sail, for rmem's instruction analysis
                ;; (riscv_analysis.sail, not part of the model). read_kind,
                ;; write_kind and barrier_kind stay: memory accesses and
                ;; FENCE use them.
                "regfp"
                "regfps"
                "niafp"
                "niafps"
                "diafp"
                "trans_kind"
                "cache_op_kind"
                "instruction_kind"
                ;; Sv32 TLB entries: virtual memory is out of scope
                ;; (cheriot-inventory/PROJECT-STATUS.md, section 1a).
                ;; TLB_Entry is a struct with Int parameters, which
                ;; Rocq rejects; the TLB functions become untranslated.
                "TLB_Entry"
                "TLB32_Entry"
                )
              identifier)))

(ignore-function-definition-and-top-level-type-constraints-predicate
 (lambda (identifier)
   (or
    (string-ends-with? "_of_num" identifier)
    (string-starts-with? "num_of_" identifier)
    (string-starts-with? "undefined_" identifier)
    (string-starts-with? "regval_of_" identifier)
    (string-ends-with? "_of_regval" identifier)
    (= "reverse_bits_in_byte" identifier)
    (= "log2" identifier)
    (= "zeros_implicit" identifier)
    (= "ones" identifier)
    (contains? '(
                 "sep"
                 "sail_zeros"
                 "sail_ones"
                 "slice_mask"
                 ;; Extern of lib/regfp.sail over the ignored type
                 ;; cache_op_kind (see below); nothing calls it.
                 "__cache_maintenance"
                 )
               identifier))))

;; Translate these polymorphic unions to muSail's ty.sum instead of to a
;; ty.union of their own. The first constructor becomes inl, the second inr,
;; so option('a) becomes ty.sum 'a ty.unit, i.e., ty.option 'a.
(translate-union-as-sum "option" "Some" "None")
(translate-union-as-sum "result" "Ok" "Err")

;; (ignore-type-definition-predicate (lambda (identifier)
;;                                (contains? '("real"
;;                                             "option"
;;                                             "register_value"
;;                                             "bits"
;;                                             "regstate")
;;                                           identifier)))

;; (ignore-value-definition-predicate
;;  (lambda (identifier)
;;    (contains? '(
;;                 "default_capability"
;;                 "initial_regstate"
;;                 "initial_Capability"
;;                 )
;;               identifier)))

(template "base.template.v")
(template "machine.template.v")
