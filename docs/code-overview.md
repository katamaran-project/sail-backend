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

The translation takes place in two phases:

* Sail is translated into nanosail, an intermediate language bridging Sail and muSail.
* Nanosail is translated into muSail: user-provided templates describe which translation belongs where in which file.

This top level logic resides in the `sail_plugin` project.

## `Ast` Module

The `Ast` module contains all definitions related to the nanosail intermediate language.
When Sail and muSail disagree in their representations, nanosail tends to side with muSail.

* Nanosail makes the distinction between expressions and statements the same way muSail does.
* Nanosail keeps track of some extra information that cannot be directly translated to muSail,
  such as numeric expressions and constraints, which come in handy for monomorphization.
* Similarly to Sail, a nanosail program (see `Ast.Program.t`) consists of a series of definitions.
  See `Ast.Definition` for which types of definitions exist.

## Sail to Nanosail

All Sail to nanosail translation logic is grouped in the `SailToNanosail` module.
To assist in the translation, we defined a monad `TranslationContext`, which is basically a combination of the State and the Result monads.
It provides the following functionality:

* It can generate unique identifiers using `generate_unique_identifier` and `generate_unique_identifiers`.
* It keeps track of all translated definitions.
  * `store_definition` adds a new definition to the list.
  * `lookup_definition`/`lookup_definition_opt` looks for a definition that satisfies a predicate.
    Useful predicates are readily available in `Ast.Definition.Select`.
  * Other helper functions such as `lookup_register_type`, `is_register`, `lookup_variant_by_constructor` are also available.
* It keeps track of calls made to polymorphic functions (`register_polymorphic_function_call_type_arguments`), which is useful for reporting them to the user
  so that they know which monomorphizations to ask for.
* There are two types of failures: `NotYetImplemented` and `Failure`

A `Failure` causes the translation to be halted and should be used sparingly.

Each Sail definition is translated in turn.
When a `NotYetImplemented` is signaled, the current definition being translated is given up on:
a `Ast.Definition.UntranslatedDefinition` gets registered and the translation process moves on to the next definition.

## Nanosail to muSail

All nanosail to muSail functionality can be found in the `NanosailToSail` module.
Similary to the first translation phase, we defined a monad to provide us with some useful functionality:

* It holds a list of all definitions gathered during the first phase.
  These can be looked up with helper functions such as `select_definitions` and `lookup_definition_opt`.
* Translations can be grouped into blocks, which are discussed later.
* 

### Blocks and Frames

Frames can be opened (= new frame pushed onto the stack) and closed (= last frame popped from the stack).

During the translation it is possible to create comments and annotations (= numbered comments),
  which are stored in the frame at the top of the stack.
  Whenever a frame is closed, 

What's a generation block?
Check `pp_annotate'`.

## Not Yet Implemented

### NYI in Sail to nanosail
### NYI in Nanosail to Sail


## Logging

## F-Expressions

More robust than `string_of`.
Allows for diffs.

## Document/PP

Annotations

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
