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
This project uses the [dune](https://opam.ocaml.org/packages/dune/),
[sail](https://opam.ocaml.org/packages/sail/) and
[pprint](https://opam.ocaml.org/packages/pprint/) packages. To install the
project with its dependencies, run:
```sh
opam install . --deps-only
dune build
dune install
```

You can also avoid installing the plugin after building the project, and load
the plugin on demand each time you want to access the Katamaran backend options
with the `-plugin` option of sail:

```sh
sail -plugin _build/default/src/sail_katamaran_backend/sail_plugin_katamaran.cmxs ...
```

Either way, the `-ok` option of `sail` should print "ok." if the backend is
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
+ `-w` defines the page width for the µSail generated code. Must be followed by an integer.


Test Nanosail
--------------------------------------------------------------------------------
### Automatic tests
```sh
dune test
```

### Manual tests
```sh
dune exec -- test/nanosail_to_microsail/main.exe <option> <input>
```
*\<input>* can be any example name (*lists*, *long*, *prod* or *expr* for the
moment). Without *\<input>*, every example is loaded.  
Use `-help` to see the list of options, `-inputs` to see the list of
available tests.


Test Full Backend
--------------------------------------------------------------------------------
### Examples
An example with list functions:

```sh
cd test/full_backend/examples/lists/
make katamaran # or katamaran-list to enable list notations
```
*lists.v* will be generated from *lists.sail*. It can be compared manually
with the expected results in `expected/` using `make compare`.


List of some of the `make` targets:
+ *check* : Sail type checking.
+ *ast* : Prints the sail AST after getting rid of syntactic sugar.
+ *interpreter* : Loads the sail file in the sail interpreter.
+ *katamaran*, *katamaran-rewrites*, *katamaran-rewrites-clean* : Respectively
  calls the sail coq target, same as *katamaran* prints each rewrite step AST in
  a new directory, same as *katamaran-rewrites* but deletes rewrite step ASTs
  when nothing has been changed.
+ *katamaran-list* : Identical to *katamaran* but uses the better list notations
  for µSail.
+ *compare*, *compare-list* : Compares the output files of respectively
  *katamaran* and *katamaran-list* with the expected µSail files.
+ *coq*, *coq-rewrites*, *coq-rewrites-clean* : Respectively calls the sail coq
  target, same as *coq* but prints each rewrite step AST in a new directory,
  same as *coq-rewrites but deletes rewrite step ASTs when nothing has been
  changed.
+ *clean* : Cleans the directory of any generated file.

### Feature tests
`test/fullbackend/feature_tests/supported.txt` contains a brief description of
the current supported featured of Sail.

```sh
cd test/full_backend/feature_tests
make test
```
`.v` files will be generated from the `.sail` files and then compared to the
`expected/` files.

Uninstallation
---------------
```sh
dune uninstall
```
