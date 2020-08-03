Universo: A tool to play with universes
=======================================

Universo is a tool for Dedukti that translates (whenever it is possible) a judgment as a proof written in one Cumulative Type System (CTS) specifcation into another CTS specification.

Possible applications are:
- Minimization of universes
- Make proofs predicative
- interoperability between CTS

How it works is really similar to "Typical ambiguity":

1. First, terms are elaborated
2. Terms are type checked generating constraints.
3. Constraints are solved via a SMT Solver (currently only Z3 is used)
4. We reconstruct the terms via the solution found by Z3

More details can be found in the [documentation](doc/DOCUMENTATION.md).

Dependencies and compilation
----------------------------

Compiling Universo is not standard (yet). It requires:

- Dune
- Using Dedukti on master branch
- Using Dkmeta on master branch

To compile Universo, just use `make`. Then you can choose to install Universo with `make install` or simply use `dune exec -- universo`.
