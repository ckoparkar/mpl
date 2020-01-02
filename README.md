# MPL

MaPLe (MPL) is an extension of the [MLton](http://mlton.org)
compiler for Standard ML which implements support for nested parallelism.

MPL is research software and is being actively developed.

## Build and Install (from source)

### Requirements

MPL has only been tested on Linux with x86-64. The following software is
required.
 * [GCC](http://gcc.gnu.org)
 * [GMP](http://gmplib.org) (GNU Multiple Precision arithmetic library)
 * [GNU Make](http://savannah.gnu.org/projects/make), [GNU Bash](http://www.gnu.org/software/bash/)
 * binutils (`ar`, `ranlib`, `strip`, ...)
 * miscellaneous Unix utilities (`diff`, `find`, `grep`, `gzip`, `patch`, `sed`, `tar`, `xargs`, ...)
 * Standard ML compiler and tools:
   - [MLton](http://mlton.org) (`mlton`, `mllex`, and `mlyacc`) recommended.  Pre-built binary packages for MLton can be installed via an OS package manager or (for select platforms) obtained from http://mlton.org.
   - [SML/NJ](http://www.smlnj.org) (`sml`, `ml-lex`, `ml-yacc`) supported, but not recommended.

### Instructions

The following builds the compiler at `build/bin/mpl`.
```
$ make all
```

After building, MPL can then be installed to `/usr/local`:
```
$ make install
```
or to a custom directory with the `PREFIX` option:
```
$ make PREFIX=/opt/mpl install
```

## Parallel and Concurrent Extensions

MPL extends SML with a number of primitives for parallelism and concurrency.
For concrete examples, see the `examples/` subdirectory.

### The `ForkJoin` Structure
```
val fork: (unit -> 'a) * (unit -> 'b) -> 'a * 'b
val alloc: int -> 'a array
```
The `fork` primitive takes two functions to execute in parallel and
returns their results.

The `alloc` primitive takes a length and returns a fresh, uninitialized array
of that size. **Warning**: To guarantee no errors, the programmer must be
careful to initialize the array before reading from it. `alloc` is intended to
be used as a low-level primitive in the efficient implementation of
high-performance libraries. It is integrated with the scheduler and memory
management system to perform allocation in parallel and be safe-for-GC.

### The `MLton.Parallel` Structure
```
val compareAndSwap: 'a ref -> ('a * 'a) -> 'a
val arrayCompareAndSwap: ('a array * int) -> ('a * 'a) -> 'a
```

`compareAndSwap r (x, y)` performs an atomic
[CAS](https://en.wikipedia.org/wiki/Compare-and-swap)
which attempts to atomically swap the contents of `r` from `x` to `y`,
returning the original value stored in `r` before the CAS.
Polymorphic equality is determined
in the same way as [MLton.eq](http://mlton.org/MLtonStructure), which is a
standard equality check for simple types (`char`, `int`, `word`, etc.) and
a pointer equality check for other types (`array`, `string`, tuples, datatypes,
etc.). The semantics are a bit murky.

`arrayCompareAndSwap (a, i) (x, y)` behaves the same as `compareAndSwap` but
on arrays instead of references. This performs a CAS at index `i` of array
`a`, and does not read or write at any other locations of the array.

## Using MPL

MPL uses `.mlb` files ([ML Basis](http://mlton.org/MLBasis)) to describe
source files for compilation. A typical compilation file with MPL is shown
below. The first three lines of this file respectively load:
* The [SML Basis Library](http://sml-family.org/Basis/)
* The `ForkJoin` structure, as described above
* The `MLton` structure, which includes the MPL extension
  `MLton.Parallel` as described above, as well as various
  [MLton-specific features](http://mlton.org/MLtonStructure). Not all MLton
  features are supported (see "Unsupported MLton Features" below).
```
(* libraries *)
$(SML_LIB)/basis/basis.mlb
$(SML_LIB)/basis/schedulers/shh.mlb
$(SML_LIB)/basis/mlton.mlb

(* your source files... *)
A.sml
B.sml
```

The command to compile a file `XYZ.mlb` is as follows. By default, MPL
produces an executable with the same base name as the source file, i.e.
this would create an executable `XYZ`:
```
$ mpl [options...] XYZ.mlb
```

MPL has a number of command-line options derived from MLton, which are
documented [here](http://mlton.org/CompileTimeOptions). Note that MPL only
supports C codegen and does not support profiling.

Some useful compile-time options are
* `-output FOO` Give a specific name to the produced executable
* `-default-type int64 -default-type word64` Use 64-bit integers and words
by default.
* `-debug true -debug-runtime true -keep g` For debugging, keeps the generated
C files and uses the debug version of the runtime (with assertions enabled).
The resulting executable is somewhat peruse-able with tools like `gdb`.

### Run-time Options

MPL executables can take arguments at the command line that control the runtime
system. The syntax to run a program `foo` is
```
$ foo [@mpl [runtime args...] --] [program args...]
```
The runtime arguments must begin with `@mpl` and end with `--`, and these are
not visible to the program via
[CommandLine.arguments](http://sml-family.org/Basis/command-line.html).

Some useful runtime arguments are
* `number-processors N` Use N worker threads to run the program. Can also
be given as `procs N`.
* `set-affinity` Pin worker threads to processors. Can be used in combination
with `affinity-base B` and `affinity-stride S` to pin thread i to processor
number B + S*i.
* `min-chunk X` Set the minimum heap chunk size to X bytes. This can be
written with suffixes K, M, and G, e.g. `10M` is 10 megabytes. Chunks cannot be
smaller than one page, typically 4K. The default size is one page.

For example, the following runs a program `hello` with a single command-line
argument `world` using 4 pinned processors.
```
$ hello @mpl procs 4 set-affinity -- world
```

## Disentanglement

Currently, MPL only supports programs that are **disentangled**, which
(roughly speaking) is the property that concurrent threads remain oblivious
to each other's allocations. For more information, see the POPL 2020 paper
"Disentanglement in Nested-Parallel Programs" by Westrick et al.

MPL does not yet have a checker to verify
disentanglement, so the programmer must manually verify their own code.
This is typically not difficult, as disentanglement is fairly general.
In particular, all race-free programs are disentangled, as are many
programs that use shared memory to communicate between concurrent threads.

## Bugs and Known Issues

### Basis Library
In general, the basis library has not yet been thoroughly scrubbed, and many
functions may not be safe for parallelism. Some known issues:
* `Int.toString` is racy when called in parallel.
* `Real.fromString` may throw an error when called in parallel.

### Other
* Some programs that use `compareAndSwap` and `arrayCompareAndSwap` primitives
may fail to compile.

## Unsupported MLton Features
Many [MLton-specific features](http://mlton.org/MLtonStructure) are
unsupported, including (but not limited to):
* `share`
* `shareAll`
* `size`
* `Finalizable`
* `Profile`
* `Signal`
* `Thread` (partially supported but not documented)
* `Cont` (partially supported but not documented)
* `Weak`
* `World`

