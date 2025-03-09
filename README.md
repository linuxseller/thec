# thec

thec - `The C` compiler written in haskell

## Testing (?)

```console
$ ghc nob.hs
$ ./nob
```

nob (inspired by Tsoding's nob.h) is building program that rebuilds project (when `nob.hs` is changed running `./nob` will recompile itself)

```console
$ ./nob runMain # will compile project and run it on `testing/test.c` file and will generate `./testing/asmest.asm`, a fasm assembly
$ ./nob runMain runAsm # will compile project and generated assembly, and also link it, then you can
$ ./testing/asmest
```
