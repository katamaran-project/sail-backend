Setup:
```sh
opam install . --deps-only
dune build
```

Test:
```sh
dune exec test/main.exe <input> [width]
```
\<input\> can be any example name (*lists* or *long* for the moment).  
[width] is an optional integer.
