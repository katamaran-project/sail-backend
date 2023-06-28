Setup:
```sh
opam install . --deps-only
dune build
```

Test:
```sh
dune exec test/main.exe <input> [width]
```
\<input\> can be any example name (*test*, *lists*, *long* or *all* for the moment).  
[width] is an optional integer.
