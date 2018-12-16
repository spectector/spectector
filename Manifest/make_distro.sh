#!/bin/bash

# Binary distribution for artifact evaluation
# (contains benchmarks) 
#
# Usage: 
#   $ ciao custom_run . bindist
#
# (NOTE: Do NOT CALL this script directly)

# Physical directory where the script is located
_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
    cd "$d";done;cd "$(dirname "$e")";pwd -P)

# (Assume that we have cloned it there)
benchmarks="$_base"/../../spectector-benchmarks
distdir="$_base"/../dist

# Create directory for distribution
mkdir -p "$distdir"

# Prepare binaries
mkdir -p "$distdir"/spectector/bin
cd "$distdir"/spectector/bin
ciaoc -S -o spectector "$_base"/../src/spectector.pl || (echo "Failed at compilation" && exit 1)
# Copy Z3 (THIRDPARTY is defined externally)
cp "$THIRDPARTY"/bin/z3 .

# Copy READMEs
cd "$distdir"/spectector
cp "$_base"/../README.md README.md

# Copy benchmarks
mkdir -p "$distdir"/spectector/benchmarks
cd "$distdir"/spectector/benchmarks
cp "$benchmarks"/check_security.sh .
cp -r "$benchmarks"/target .
cp -r "$benchmarks"/sources .

cd "$distdir"
tar cfvz spectector.tar.gz spectector

