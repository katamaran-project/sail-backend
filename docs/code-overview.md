# Code Overview

This repository contains three projects:

* `monads`: a small library providing a number of monads used by the Sail backend.
* `nanosail`: relies on `monads` and contains all of the actual translation logic.
* `sail_plugin`: a very small project that imports `nanosail` and registers itself with Sail as a plugin.


