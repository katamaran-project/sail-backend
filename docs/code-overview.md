# Code Overview

This repository contains three projects:

* `monads`: a small library providing a number of monads used by the Sail backend.
* `nanosail`: relies on `monads` and contains all of the actual translation logic.
* `sail_plugin`: a very small project that imports `nanosail` and registers itself with Sail as a plugin.

**Note regarding dependencies**
When Sail loads the plugin, it does not automatically also load the dependencies needed by this plugin.
It is therefore necessary to statically link nanosail's dependencies.
See the `dune` file for the plugin project (`embed_in_plugin_libraries`).


## Logging

## F-Expressions

## Document/PP

## Slang

## Coding Style

### No wildcards

We shied away from using wildcards in match expressions, instead opting to handle each case explicitly.
Whenever a type is extended with a new case (something that was expected to occur frequently as development progressed),
a simple rebuild would lead the compiler to point out all the locations in the code that required an update to deal with this new case.

**Note**
This has somewhat been thwarted by what seems to be a bug in the OCaml compiler:
it happened more than once that the build process simply got stuck until typing errors were fixed.

### Explicit equals
### Explicit Typing
### Module per Type
### Recursive Module
