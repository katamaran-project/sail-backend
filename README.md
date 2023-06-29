Setup:
```sh
opam install . --deps-only
dune build
```

Test:
```sh
dune exec test/main.exe <option> <input>
```
\<input\> can be any example name (*test*, *lists*, *long* or *all* for the moment).  
`--help` to see the list of options
