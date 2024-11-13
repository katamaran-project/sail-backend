# Sail Katamaran Backend

## Installation

```sh
# Create new switch (optional)
$ opam switch create sail-katamaran ocaml.5.1.0

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

## Uninstallation

```sh
$ dune uninstall
```
