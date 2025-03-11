# Code Overview

This repository contains three projects:

* `monads`: a small library providing a number of monads used by the Sail backend.
* `nanosail`: relies on `monads` and contains all of the actual translation logic.
* `sail_plugin`: a very small project that imports `nanosail` and registers itself with Sail as a plugin.

**Note regarding dependencies**
When Sail loads the plugin, it does not automatically also load the dependencies needed by this plugin.
It is therefore necessary to statically link nanosail's dependencies.
See the `dune` file for the plugin project (`embed_in_plugin_libraries`).


## HTML Generation

## Logging

## F-Expressions

## Document/PP

## Slang

## Coding Style

### No wildcards
### Explicit Typing
### Recursive Module
