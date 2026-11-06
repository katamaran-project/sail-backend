# Installation

These are the same steps the GitHub workflow (`.github/workflows/build.yaml`) runs.
The project targets OCaml 5.2.1 and Sail 0.20.3.

```sh
# Install Sail's system dependencies
# (see https://github.com/rems-project/sail/blob/sail2/INSTALL.md)
$ sudo apt install build-essential libgmp-dev z3 opam pkg-config zlib1g-dev

# Create new switch (optional)
# "katamaran" is the name of the switch and can be chosen freely
$ opam switch create katamaran ocaml.5.2.1

# Install Sail, pinned to the version this project targets
$ opam pin add sail 0.20.3

# Generate the .opam files from dune-project (they are not tracked in git),
# then install the remaining dependencies from them
$ opam install dune
$ dune build monads.opam nanosail.opam sail_katamaran_backend.opam
$ opam install . --deps-only
$ opam install ounit2

# Build
$ dune build

# Run the unit tests (optional)
$ dune test

# Install plugin
$ dune install
```

`dune install` overwrites any previously installed plugin in the switch.
You can also avoid installing the plugin after building the project, and load
the plugin on demand each time you want to access the Katamaran backend options
with the `-plugin` option of sail:

```sh
sail -plugin _build/default/sail_plugin/sail_plugin_katamaran.cmxs ...
```

`bin/sail-local` does this for you: it is a drop-in replacement for `sail` that loads the plugin built in this checkout.

## Checking Installation

The plugin is installed correctly if the following command produces the output shown.

```sh
$ sail --katamaran
No configuration file specified; use --katamaran-config to specify one
```

## End-to-end tests

The steps above suffice to build the plugin and run the unit tests.
The end-to-end tests (`make e2e`, see `docs/tests.md`) also compile the generated Rocq code with `coqc`,
so they additionally need Rocq and the Katamaran library in the same switch.
Katamaran main needs Rocq 9.2, rocq-iris 4.5, rocq-stdpp 1.13 and rocq-equations 1.3.2+9.2
(see the [Katamaran README](https://github.com/katamaran-project/katamaran)).
The generated code imports `From Stdlib`, so it no longer compiles with Coq 8.x.

```sh
$ opam repo add rocq-released https://rocq-prover.org/opam/released
$ opam pin add rocq-core 9.2.0
$ opam pin add rocq-iris 4.5.0
$ opam pin add rocq-equations 1.3.2+9.2
$ opam install rocq-core rocq-iris rocq-equations   # also installs rocq-stdpp and coq-core, which provides coqc

# Katamaran has no .opam file; build and install it from a checkout with its Makefile
# (not with dune, which also builds files that _CoqProject leaves out)
$ make -C <katamaran> && make -C <katamaran> install
```

## Uninstalling

```sh
$ dune uninstall
```
