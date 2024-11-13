# Sail Katamaran Backend

## Installation

```sh
# Install Sail's system dependencies
# (see https://github.com/rems-project/sail/blob/sail2/INSTALL.md)
$ sudo apt install build-essential libgmp-dev z3 pkg-config zlib1g-dev

# Create new switch (optional)
# "katamaran" is the name of the switch and can be chosen freely
$ opam switch create katamaran ocaml.5.1.0

# Install dependencies
$ opam install . --deps-only

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

The plugin is installed correctly if the following
command produces the output shown.

```sh
$ sail --katamaran-check
The Katamaran plugin is functioning correctly
```

## Running Tests

```sh
$ dune test
```

## Uninstalling

```sh
$ dune uninstall
```
