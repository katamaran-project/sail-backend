# Installation

```sh
# Install Sail's system dependencies
# (see https://github.com/rems-project/sail/blob/sail2/INSTALL.md)
$ sudo apt install build-essential libgmp-dev z3 cvc4 opam pkg-config zlib1g-dev

# Create new switch (optional)
# "katamaran" is the name of the switch and can be chosen freely
$ opam switch create katamaran ocaml.5.1.0

# Install dependencies
$ opam install . --deps-only
$ opam install ounit2

# Build
$ dune build

# Install plugin
$ dune install
```

You can also avoid installing the plugin after building the project, and load
the plugin on demand each time you want to access the Katamaran backend options
with the `-plugin` option of sail:

```sh
sail -plugin _build/default/src/sail_katamaran_backend/sail_plugin_katamaran.cmxs ...
```

## Checking Installation

The plugin is installed correctly if the following command produces the output shown.

```sh
$ sail --katamaran
No configuration file specified; use --katamaran-config to specify one
```

## Uninstalling

```sh
$ dune uninstall
```
