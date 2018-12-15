# Spectector

Code organization:

  spectector.pl         Main tool module
  muasm_translator/     Translate from x86 code to muAsm
  solver/               Symbolic solver
  fetch-externals.sh    Script to download third-party components

# Build

Compile with `ciaoc spectector`. Execute `fetch-externals.sh` to
install third party components (external SMT solvers). The tool will
use the solvers at `third-party/` if they are not found in the `PATH`.

# Usage

See `spectector -h` output for usage instructions.

The scripts at the `benchmarks/` directory can be used to execute
basic tests and reproduce the benchmark results.

