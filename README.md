# Spectector

Spectector is a tool for reasoning about information flows produced by
speculatively executed instructions. It takes as input an x64 assembly
program (in the Intel or AT&T syntax), and it automatically
detects leaks introduced by speculatively executed instructions (or
proves their absence).

In a nutshell, Spectector symbolically executes the program under analysis with
respect to a semantics that accounts for the speculative execution's effects.
During the symbolic execution, Spectector derives SMT formulae characterizing
leaks caused by speculatively executed instructions. It then relies on the Z3
SMT solver to determine the presence of possible leaks. More precisely,
Spectector automatically proves whether a program satisfies _speculative
non-interference_, a security property formalized in the [Spectector's
paper](TODO).

Using Spectector, we detected subtle bugs in the way countermeasures
are placed by several major compilers, which may result in insecure
programs or unnecessary countermeasures. See the
[Spectector's paper](TODO) for more information.

## Requirements

Spectector is written using the Prolog dialect supported by the
[Ciao system](https://github.com/ciao-lang/ciao), and it needs the Ciao environment
to compile. Additionally, Spectector uses the
[Z3 SMT solver](https://github.com/Z3Prover/z3) to check the
satisfiability of formulae.

We have tested Spectector with Ciao version 1.18 and Z3 version 4.8.4 on Debian
Linux (kernel 4.9.110), Arch Linux (kernel 4.19.4), Mac OSX (version 10.14), and
Windows 10 Pro (using the Windows Linux Subsystem with kernel 4.4.0).

## Build and installation

Once the Ciao system is installed, you can automatically fetch, build, and install Spectector using:

```
ciao get github.com/imdea-software/spectector
```

The following dependendencies (including third-party code) will be
installed automatically:

1. [Concolic](https://github.com/ciao-lang/concolic) for
   SMT-enhanced symbolic reasoning and search
   (`ciao get concolic`). This will automatically install Z3.
2. [muASM translator](https://github.com/imdea-software/muasm_translator)
   for muASM translation from x64 assembly (`ciao get
   github.com/imdea-software/muasm_translator`)

All code will be downloaded and built under the first directory
specified in the `CIAOPATH` environment variable or `~/.ciao` by
default.

**For developing** it is recommended to define your own
_workspace directory_ and clone this repository into it. E.g., `export
CIAOPATH=~/ciao` and update your `PATH` with `eval "$(ciao-env)"`. 
The dependencies can be cloned manually or fetched automatically by
calling `ciao fetch` at the source directory.

[Marco: Clarify this! (By the way, wouldn't the above be needed anyway to run ciao and compile spectector?)]

Use the following commands to check your installation (it should show
`ok` in a few seconds):
```
cd SPECTECTORDIR/tests/
./runtests.sh
```

# Using Spectector

Spectector can be executed over x64 assembly programs. Additionally,
Spectector can analyze programs written in muASM, a simple assembly
language described in the [Spectector's paper](TODO).

Spectector treats each file with extension `.s` as an x64 program
written using the AT&T syntax, each file with extension `.asm` as an
x64 program written using the Intel syntax, and each file with
extension `.muasm` as a muASM program.

Once correctly installed, Spectector can be run from the command line
as follows:

```
spectector assembly.file -c "c([Memory],[Assignments])" --low "[Low,variables]"
spectector assembly.file -a analysis -c "c([Memory],[Assignments])" --low "[Low,variables]"
```

[Some documentation needed?]

See the output of `spectector -h` for more detailed instructions.

## Supported x64 instructions and other limitations

See Sections VI.C and VIII of the [Spectector's paper](TODO) for a
description of the supported instructions and limitations.
