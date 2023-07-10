Sail Katamaran Backend
================================================================================


Installation
--------------------------------------------------------------------------------
```sh
opam install . --deps-only
dune build
dune install
```

You can also avoid to install the plugin after building the project, and load
the plugin on demand each time you want to access the Katamaran backend options
with the `-plugin` option of sail:

```sh
sail -plugin _build/default/src/sail_katamaran_backend/sail_plugin_katamaran.cmxs ...
```

Anyway, the `-ok` option of `sail` should print "ok." if the backend is
installed/the plugin loaded.


Katamaran
--------------------------------------------------------------------------------
In order to test the generated files,
[Katamaran](https://github.com/katamaran-project/katamaran/) (with its
dependencies) is required.


Usage
--------------------------------------------------------------------------------
+ `-katamaran` is the sail option to activate the Katamaran target.
+ `-list_notations` allows the µSail generated file to use some more readable
  list notations defined by Katamaran.


Test Nanosail
--------------------------------------------------------------------------------
```sh
dune exec -- test/main.exe <option> <input>
```
*\<input>* can be any example name (*lists*, *long*, *prod* or *expr* for the
moment). Without *\<input>*, every example is loaded.
`--help` to see the list of options


Test Full Backend
--------------------------------------------------------------------------------
```sh
cd examples/lists/
make katamaran # or katamaran-list to enable list notations
```
lists.v will be generated from lists.sail


Uninstallation
---------------
```sh
dune uninstall
```