Sail Katamaran Backend
================================================================================


This project use the opam package manager.

Opam
--------------------------------------------------------------------------------
First, your current [opam](https://opam.ocaml.org/doc/Install.html) switch must
use the **ocaml.5.0.0** compiler. To create a new switch you can run the
following commands (replace `sail-katamaran` with whatever name you
want for your switch):

```sh
opam switch create sail-katamaran ocaml.5.0.0
```

Later if you want to return to your default switch, run:

```sh
opam switch set default
```
Change `default` with any other switch name to use it.


Installation
--------------------------------------------------------------------------------
This project use the [dune](https://opam.ocaml.org/packages/dune/),
[sail](https://opam.ocaml.org/packages/sail/) and
[pprint](https://opam.ocaml.org/packages/pprint/) packages. To install the
project with its dependencies, run:
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
+ `-ok` prints "ok.".


Test Nanosail
--------------------------------------------------------------------------------
### Automatic tests
```sh
dune test
```

### Manual tests
```sh
dune exec -- test/main.exe <option> <input>
```
*\<input>* can be any example name (*lists*, *long*, *prod* or *expr* for the
moment). Without *\<input>*, every example is loaded.  
Use `-help` to see the list of options, `-inputs` to see the list of
available tests.


Test Full Backend
--------------------------------------------------------------------------------
```sh
cd test/full_backend/
make katamaran # or katamaran-list to enable list notations
```
*lists.v* will be generated from *lists.sail*


Uninstallation
---------------
```sh
dune uninstall
```