(lang dune 3.8)

(using dune_site 0.1)

(name sail_katamaran_backend)
(version 0.1)

(generate_opam_files true)

(source
	(github mariusgoyet/sail-katamaran-backend))

(authors "Marius Goyet")

(maintainers "Marius Goyet")

(package
	(name nanosail)
	(synopsis
		"Intermediate representation language for the Sail to µSail translation")
	(depends ocaml dune sail pprint)
	(allow_empty))

(package
	(name sail_katamaran_backend)
	(synopsis "Sail to µSail translation")
	(depends ocaml dune sail pprint)
	(allow_empty))

