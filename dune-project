(lang dune 3.8)

(using dune_site 0.1)

(name sail_katamaran_backend)
(version 0.1)

(generate_opam_files true)

(source
	(github katamaran-project/sail-backend))

(authors "Marius Goyet" "Frederic Vogels")

(maintainers "Frederic Vogels")

(package
	(name nanosail)
	(synopsis
		"Intermediate representation language for the Sail to µSail translation")
	(depends ocaml
                 dune
                 sail
                 ppx_here
                 pprint
                 base
                 stdio
                 auxlib)
	(allow_empty))

(package
	(name auxlib)
	(synopsis
		"Auxlib")
	(depends ocaml
                 dune
                 base
                 pprint)
	(allow_empty))

(package
	(name monads)
	(synopsis
		"Monad Library")
	(depends ocaml
                 dune
                 base
                 auxlib)
	(allow_empty))

(package
	(name sail_katamaran_backend)
	(synopsis "Sail to µSail translation")
	(depends ocaml
                 ocaml_intrinsics_kernel
                 dune
                 sail
                 base
                 stdio
                 pprint
                 auxlib)
	(allow_empty))
