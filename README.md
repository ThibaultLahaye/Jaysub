# J< Starter project

Read the assignment, follow the instructions.

# How to run

```shell
opam install dune menhir ppx_deriving

eval $(opam config env)

dune build

dune exec jaysub forward examples/fib.jsub
```

## CLI usage

```
jaysub [forward|backward|invert|optimize] <filename.jsub>
```

