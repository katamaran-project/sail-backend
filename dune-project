(lang dune 3.8)

(using dune_site 0.1)

(name sail_katamaran_backend)
(version 0.1)

(generate_opam_files true)

(source
	(github katamaran-project/sail-backend))

(authors "Marius Goyet")

(maintainers "Marius Goyet")

(package
	(name nanosail)
	(synopsis
		"Intermediate representation language for the Sail to µSail translation")
	(depends ocaml
                 dune
                 sail
                 pprint
                 auxlib
                 accessor_base
                 base
                 ppx_accessor
                 ppx_here
                 configlib)
	(allow_empty))

(package
	(name slang)
	(synopsis
		"S-Expression Language")
	(depends ocaml
                 dune
                 pprint
                 auxlib)
	(allow_empty))

(package
	(name auxlib)
	(synopsis
		"Auxlib")
	(depends ocaml
                 dune
                 pprint)
	(allow_empty))

(package
	(name monads)
	(synopsis
		"Monad Library")
	(depends ocaml
                 dune
                 pprint)
	(allow_empty))

(package
	(name ConfigLib)
	(synopsis
		"Configuration library")
	(depends ocaml
                 dune
                 base
                 slang)
	(allow_empty))

(package
	(name sail_katamaran_backend)
	(synopsis "Sail to µSail translation")
	(depends ocaml
                 dune
                 sail
                 pprint
                 auxlib)
	(allow_empty))
