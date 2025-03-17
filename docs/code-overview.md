# Code Overview

## Project Structure

This repository contains three projects:

* `monads`: a small library providing a number of monads used by the Sail backend.
* `nanosail`: relies on `monads` and contains all of the actual translation logic.
* `sail_plugin`: a very small project that imports `nanosail` and registers itself with Sail as a plugin.

**Note regarding dependencies**
When Sail loads the plugin, it does not automatically also load the dependencies needed by this plugin.
It is therefore necessary to statically link nanosail's dependencies.
See the `dune` file for the plugin project (`embed_in_plugin_libraries`).

## Translation Overview

The translation takes place in three phases:

* The configuration file is read.
* Sail is translated into nanosail, an intermediate language bridging Sail and muSail.
* Nanosail is translated into muSail.
  This is done using templates: the user provides a number of template files,
  which specify which translations belong where.

This top level logic resides in the `sail_plugin` project.

## `Ast` Module

The `Ast` module contains all definitions related to the nanosail intermediate language.



## Sail to nanosail



TranslationContext

## Nanosail to muSail

GenerationContext

## Not Yet Implemented

### NYI in Sail to nanosail
### NYI in Nanosail to Sail


## Logging

## F-Expressions

## Document/PP

## Coding Style

Below we justify our coding style, which may be considered to be rather unidiomatic.

### No wildcards

We shied away from using wildcards in match expressions, instead opting to handle each case explicitly.
Whenever a type is extended with a new case (something that was expected to occur frequently as development progressed),
a simple rebuild would lead the compiler to point out all the locations in the code that required an update to deal with this new case.

**Note**
This has somewhat been thwarted by what seems to be a bug in the OCaml compiler:
it happened more than once that the build process simply got stuck until typing errors were fixed.

### Module per Type

Many types are defined in their own personal module.
We felt this led to much cleaner code:

* No prefixes necessary for constructor: `E_Variable` but simply `Variable`.
* Clearer names: `Ast.Expression.Variable` instead of `E_Variable`.
* Typing context often allows us to omit the `Ast.Expression.` prefix in many cases such as in match patterns.
* `open Ast.Expression` is always available if one gets tired of the long names.
* Related functionality can be bundled in the same module, e.g., `Ast.Variable.equal`.
* More consistent naming for related functions, i.e., all equality functions can be called `equal`.

### Explicit `equals`

Given that the standard equality operator `=` cannot be customized for specific types,
it seemed better to eschew it completely.
Instead, we chose to define `equal` function for each type, accepting
that large amounts boilerplate code is the cost for more robust code.

### Explicit Typing

We chose to annotate most of our code with types.

* In our opinion, it leads to clearer code.
* It avoids issues with records with share field names.
* Our functions often receive "too many" parameters, i.e., they were made
  to receive all information that was available, not just information that
  was actually required.
  We chose this approach due to the very incremental/iterative nature of the development process:
  the first implementation of a function would contain only very specific functionality,
  and it was unclear what information would be necessary when the function would later be fleshed out more.
  Having functions receive all available information from the get-go
  made it much easier when we later had to revisit the function to extend its functionality,
  as we had a clear view of all available puzzle pieces.
