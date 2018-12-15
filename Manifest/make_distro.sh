#!/bin/bash

mkdir -p spectector/bin
mkdir -p spectector/benchmarks
mkdir -p spectector/results/out
mkdir -p spectector/results/time
cd code
ciaoc -S spectector.pl || (echo "Failed at compilation" && exit 1) # Makes standalone executable
cd - > /dev/null
# With ln we don't replicate info
ln -f code/spectector spectector/bin/spectector
ln -f code/third-party/bin/z3 spectector/bin/z3
cp README.md spectector/README.md
cp -r benchmarks/*.sh spectector/benchmarks
cp -r benchmarks/clang spectector/benchmarks
cp -r benchmarks/intel spectector/benchmarks
cp -r benchmarks/microsoft spectector/benchmarks
cp -r benchmarks/source spectector/benchmarks
cp -r benchmarks/tests spectector/benchmarks
cp -r benchmarks/*py spectector/benchmarks

tar cfvz spectector.tar.gz spectector
mv spectector dist
