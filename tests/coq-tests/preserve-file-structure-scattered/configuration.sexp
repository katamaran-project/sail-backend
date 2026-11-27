(include ../shared-configuration.sexp)

(preserve-file-structure)

;; the files of the Sail files go to Model/ instead of Machine/
(preserve-file-structure-directory Model)

;; other names than base.v and machine.v: the declarations go to program_decl.v
(templates types.template.v program.template.v)
