# Reproducing the results from the paper "Spectector: Principled Detection of Speculative Information Flows"

Here we describe how to use Spectector to reproduce the experimental results from
the paper "Spectector: Principled Detection of Speculative Information Flows"
(available [here](TODO)).

## <a name="benchmarks"></a> Benchmarks

The benchmarks used in the paper are the fifteen variants of the SPECTRE v1
proof-of-concept developed by Paul Kocher (available [here](https://www.paulkocher.com/doc/MicrosoftCompilerSpectreMitigation.html)).

The folder `benchmarks/sources` contains the source files in the C language taken
from [here](https://www.paulkocher.com/doc/MicrosoftCompilerSpectreMitigation.html).
To account for minor syntactic differences between the C dialects supported by
the targets compilers, we adapted the Paul Kocher's examples for each compiler.
Specifically, the folders `benchmarks/sources/clang`, `benchmarks/sources/intel`,
and `benchmarks/sources/microsoft` contain respectively the  C programs used
with the Clang compiler, the Intel icc compiler, and the Microsoft Visual C++
compiler.


The folder `benchmarks/target` contains the corpus of 210 assembly programs that
have been obtained by compiling the fifteen variants of the SPECTRE v1
proof-of-concept developed by Paul Kocher (available [here](https://www.paulkocher.com/doc/MicrosoftCompilerSpectreMitigation.html))
using the Clang, Intel icc, and Microsoft Visual C++ compilers with different levels
of optimization and protection against SPECTRE attacks. 

The folders `benchmarks/target/clang`, `benchmarks/target/intel`, and
`benchmarks/target/microsoft` contain the assembly programs obtained by compiling
each of the Paul Kocher's example with several optimization and mitigation levels.
We refer the interested reader to Section VII.A of the [paper](TODO) for a detailed
description of how the assembly programs have been obtained.

[TODO:
1. Update names and folder structure
]

### Compile the files with the Clang compiler

We obtained the source programs in the `benchmarks/target/clang` folder by
compiling the files in `benchmarks/sources/clang` with the Clang compiler
v7.0.0.
We refer the reader to the script `generate_clang.sh` in the folder `benchmarks/scripts`
for more information. Note that running the script requires a working version of
the `clang` and `llc` binaries in your `PATH`.

### Compile the files with the Intel icc compiler

We obtained the source programs in the `benchmarks/target/intel` folder by
compiling the files in `benchmarks/sources/intel` with the Intel icc compiler
v19.0.0.117.
We refer the reader to the script `generate_intel.sh` in the folder `benchmarks/scripts`
for more information. Note that running the script requires a working version of
the Intel icc compiler installed at `/opt/intel/bin/icc`.

### Compile the files with the Microsoft Visual C++ compiler

We obtained the source programs in the `benchmarks/target/microsoft` folder by
compiling the files in `benchmarks/sources/microsoft` with the Microsoft Visual
C++ compiler v19.15.26732.1.

To generate the assembly program `assembly.asm` corresponding to a file `source.c`
in `benchmarks/target/microsoft`, we run the compiler from the command line with
the following parameters:

* In UNP -O0 mode:

```
    cl /c /FA /Od source.c /Faassembly.asm
```
* In UNP -O2 mode:

```
    cl /c /FA /O2 source.c /Faassembly.asm
```

* In FEN -O0 mode:

```
    cl /c /FA /Qspectre /Od source.c /Faassembly.asm
```

* In FEN -O2 mode:

```
    cl /c /FA /Qspectre /O2 source.c /Faassembly.asm
```

[TODO:
1. Can we get a simple script also for Windows? (using the Windows Linux subsystem)]

## Generate the results from Figure 9

Once Spectector is correctly installed, one may reproduce the results from
Figure 9 by running the script `check_security.sh`, which can be found in the
`benchmarks/scripts` folder.
The script runs Spectector on all the 210 assembly programs (in the folder `benchmarks/target`)
described in the paper.
The results of each execution will be stored in the folder `results/out`.
Additionally, the script produces a textual representation of Figure 9 in the
file `results/summary.txt`, where L indicates that Spectector identified a
speculative leak while S denotes that Spectector proved the corresponding program's
security.
Note that both `results/out` and `results/security.txt` will be overwritten every
time the script `check_security.sh` is ran.

[TODO:
1. Update the column names to match the paper.
2. Update the symbols to match the paper.
3. Update file names (see above).]

## Obtaining timing measurements

The script `timing.sh` in `benchmarks/scripts` can be used for measuring the time
taken by Spectector for analyzing all the programs in the folder `TODO`.
The script takes as argument the number of times each file should be analyzed
(the default value is 3). All the timing measurements are stored in the folder
`results/timing`.

The Python script `statistics_time.py` in the folder `benchmarks/script` processes
the timing measurements in the folder `results/timing` produced by `timing.sh`,
and it generates a CSV file containing the average, maximum and minimum times needed
to analyze each input file.

[
TODO:
1. rename script
2. rename folders
3. move stuff around
]
