# Obtaining assembly programs

Spectector analyzes assembly programs. Here we describe a few ways of obtaining
such programs.

## Writing your own assembly programs

The simplest way of obtaining an assembly program is writing it yourself :-)
Spectector can analyze x64 assembly programs written in the AT&T or Intel syntax.

## Generating assembly programs with compilers

Alternatively, one can use compilers to generate an assembly program that 
corresponds to a higher-level program. Here, we present a few alternatives for
generating assembly programs starting from C source code.

### Using Clang

Obtaining an assembly program `target.s` (in the AT&T syntax) from a C program
`program.c` can be done by:

```
clang -S program.c -otarget.s
```

### Using Intel icc compiler

Obtaining an assembly program `target.s` (in the AT&T syntax) from a C program
`program.c` can be done by:

```
icc -S -c program.c -o target.s
```

### Using gcc

Obtaining an assembly program `target.s` (in the AT&T syntax) from a C program
`program.c` can be done by:

```
gcc -S -c program.c -o target.s
```

### Using Microsoft Visual C++ compiler

Obtaining an assembly program `target.asm` (in the Intel syntax) from a C program
`program.c` can be done by:

```
    cl /FA program.c /Fatarget.asm
```

## Disassembling existing binaries

Finally, one could obtain an assembly program by disassembling existing binaries,
without relying on a program's source code. A basic script for doing that is the
following:

```
[TODO: Shall we try to analyze stuff from binaries?]
```
