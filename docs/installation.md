# Installation

These are the same steps the GitHub workflow (`.github/workflows/build.yaml`) runs.
The project targets OCaml 5.1.0 and Sail 0.20.3.

```sh
# Install Sail's system dependencies
# (see https://github.com/rems-project/sail/blob/sail2/INSTALL.md)
$ sudo apt install build-essential libgmp-dev z3 opam pkg-config zlib1g-dev

# Create new switch (optional)
# "katamaran" is the name of the switch and can be chosen freely
$ opam switch create katamaran ocaml.5.1.0

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
so they additionally need Rocq/Coq and the Katamaran library in the same switch.
See the [Katamaran README](https://github.com/katamaran-project/katamaran) for how to install them with opam.
A known working combination is Coq 8.19.2, Iris 4.3.0 and `coq-katamaran` (dev version).

## Uninstalling

```sh
$ dune uninstall
```
