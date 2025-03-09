# thec

thec - `The C` (pronounced zek) compiler written in haskell

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

**thec** is able to compile only hello world for now (without any includes)

```c
int main(){
    printf("Hello World\n");
    printf("Bye World\n");
    return 0;
}
```

## Dependencies

Works only on x86_64 linux machines that use glibc.

* ghc
* fasm
* ld

Uses `fasm` as a backend assembly compiler and `ld` as a linker
