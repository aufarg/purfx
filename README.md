# PurFX

PurFX is an imperative language that separates effects and functions.

# Build

The project currently depends on the following libraries:
- LLVM 9.0.1

Since the project builds a static binary, so it requires static version of
dependencies. Static linking with gold will fail, using other linkers seems to
work fine (preferrably lld).

The following command will build the project and generate `pfc` binary:
```
stack build
```

# Usage

`pfc` can only read code from stdin for now. The following command will generate object file called `a.o`:

```
stack exec pfc <<< 'pure a(b) if b > 0 then a(b-1) * b else 1; extern printf(x); pure main() printf("%d\n", a(5));'
```

You can then link it with gcc:

```
gcc -no-pie -o a a.o
```

Or clang:
```
clang -o a a.o
```

# Language

The language only accepts 64-bit word for variable types. It also supports
string literal which is useful for string formatting. Since it is linked with C
runtime, you can import any functions in libc.
