Universo: A tool to play with universes
=======================================

Universo is a Dedukti tool that translates (whenever it is possible) a judgment written in one Cumulative Type System (CTS) specifcation to another. How it works is really similar to "Typical ambiguity":

1. First, terms are elaborated
2. Terms are type checked generating constraints.
3. Constraints are solved via a SMT Solver (currently only Z3 is used)
4. We reconstruct the terms via the solution found by Z3

More details can be found in the [documentation](doc/DOCUMENTATION.md).

Dependencies and compilation
----------------------------

Compiling Universo is not standard (yet). It requires:

- Dune
- Using Dedukti on branch
- Using Dkmeta

The branch will eventually be merge soon and eventually Universo will be released on `opam`.

To compile Universo, just use `make`. Then you can choose to install Universo with `make install` or simply use `dune exec -- universo`.
