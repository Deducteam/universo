# Universo: A tool to play with universes

Universo is a tool that translates if possible a judgment from one Cumulative Type System (CTS) specifcation to another. How it works is really similar to "Typical ambiguity": concrete universes are elaborated to fresh variables and then the type checking of these terms generate constraints that should be solved. Universo works in four steps:

1 - First, terms are elaborated
2 - Terms are type checked. The type checking generates constraints.
3 - Constraints are solved via a SMT Solver (currently only Z3 is used)
4 - We reconstruct the terms via the solution found by Z3 (which in the current version is trivial)

The first three steps can be computed independently (see the usage paragraph).

# How to use Universo

Universo works in a very generic way and hence need to be parametrized. Mainly it needs:

- A theory file
- An input file which indicates which universes has to be computed
- A target file which encodes the target CTS specification
- An output file which indicates how to translate back universes computed by universo

The main idea to parameterized Universo is to use rewrite rules. Such rewrite rules will be used as computional rules by `dkmeta`.

## The theory

 First, Universo needs to use some kind of specific CTS encoding for technial reasons. The public interface used by Universo is the one you can found in `encodings/public.dk`. The private part of the encoding is specific to Universo also and is found in `encodings/private.dk`. We require the user to build a theory file using these two files. Such theory file will be used by Universo during type checking.

## Input file

Each universes that should be elaborated should be mapped to the constant `var` that you can find in `encodings/public.dk`. All the other universes should be mapped to a natural number via the constants `uzero` and `usucc`.

## Target file

This file should encode the target specification. It has to implement the predicates `Axiom`, `Cumul` and `Rule`.

## Output file

This file should map universes built with `uzero` and `usucc` to the universes used by the target specification.

# Results files of Universo

For each file let us say `A.dk`, Universo generates three files :

- `A.dk` which is the same as `A.dk` but universes are elaborated. Each universe is now a fresh constant.
- `A_univ.dk` contains all the fresh universe constants for `A.dk`
- `A_cstr.dk` contains the specification induced by the type checking of universo on file `A.dk`. These constraints are reordered so that building a decision tree if the user does not compute the solution is not slow.
- `A_sol.dk` contains the solution found by Z3

In the `meta` directory, you may found a script `simpl.sh` which will instantiate the solution directly in the file `A.dk` so that `A_univ`, `A_cstr` and `A_sol` are no longer necessary.

# Dependencies

Compiling Universo is not standard (yet). It requires:

- Dune
- Using Dedukti on branch `typing-functor`
- Using Dkmeta (compiled for the branch above)

The branch will eventually be merge soon and eventually Universo will be released on `opam`.

# Compilation

Just use `make`.
