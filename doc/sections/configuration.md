Configuration file
==================

Universo needs a configuration file to work properly. The purpose of this section is to described the format of this file. Concrete examples might be found in TODO.

The configuration file obeys the Dedukti syntax. The file is cut in sections. Each section is introduced as `def <section> : Type`. Currently, all the section contains rewrite rules, which sometimes are interpreted as rewrite rules or as parameters given to Universo.

The sections recognized by Universo are:

- `elaboration`
- `output`
- `solver`
- `qfuf_specifcation`
- `lra_specification`
- `constraints`
- `end`

Only the section `constraints` is optional. Depending the logic you used for the SMT solver, only one of the specification section is needed: Either `qfuf_specification` or `lra_specition`.

# Meta rewrite rules

Most of the rules specified in the configuration file will be used as **meta** rewrite rules. In other word, these rules will be used to compute. To do this, Universo uses another tool called [Dkmeta](https://github.com/Deducteam/dkmeta) which is a wrapper around the `Dedukti` rewrite engine. Mainly this tool compute the strong normal form of a term according to some meta rules and prints the result. In the case of Universo, the terms are printed in a new file depending on the current step. Hence meta rewrite rules for elaboration are printed in the file `<output>/A.dk` if the module `<input>/A` was given as input where `<output>` is the directory given with the option `-o`.

In practice meta rules are simple and hence, the SNF is linear to compute.

## Elaboration section

This section contains rewrite rules that are used by Universo to elaborate the terms. These rewrite rules should replace all the sorts wanted by a fresh variable denoted by `<md>.var` where `<md>` is the module name of the theory file. For example :

``` dedukti
[s] cic.type s --> cic.var.
```

In practice, some universes are fixed and does not need to be elaborated (such as `prop`). In that case, such universes should be mapped to natural numbers. Universo uses unary natural numbers denoted by the constants `uzero` and `usucc`. A sort is introduced by the constant `enum`. In our case, `prop` can be elaborated to the universe `uzero` with the following meta rewrite rule :

``` dedukti
[] cic.prop --> cic.enum cic.uzero.
```

#### A hack (for the advanced user)

Any rewrite rules is allowed in this sections. Hence, the user might introduce others meta rewrite rule if he wants to. However, this might introduce conflict for the later steps.

## Output

Universo only knows sorts which are natural numbers such as `cic.enum (cic.usucc cic.uzero)`. The purpose of the output section is to translate these universes to the intented one (`cic.type`, `cic.kind`, ...). This section contains meta rewrite rules that maps Universo universes to the user's ones.

## Constraints

The solution found by Universo might not be the one intented. For example, without additional constraints, Universo might say that `nat` is a proposition. To avoid such behavior, additional constraints can be added. For example, to enforce that `nat` from the matita's arithmetic library lives in `type`, one may add the constraint:

``` dedukti
[] matita_arithmetics_nat.nat     --> cic.Cumul (cic.enum (cic.usucc cic.uzero)) cic.var.
```

Notice that:

- the left-hand side should be the complete name of the constant
- the right-hand side is a constraint (eithor Axiom, Rule or Cumul).
- cic.var denotes the sort of the constant


## Solver

This section contains only parameters to configure the SMT solver.

- `solver` (default `z3`): can only take the value `z3` currently
- `logic` (default `qfuf`): can take the values `qfuf` or `lra`
- `opt` (default `normal`): can take the values `normal` or `uf`. We realized that doing our own union-find was faster with `qfuf`. `uf` uses an union-find before calling `z3`
- `minimum` (default `0`): the minimum number of universes wanted
- `maximum` (default `-1`): the maximum number of universes wanted. If negative, then this number is not bounded. Only possible with `lra`.
- `print` (default false): print the problem in the file `./universo.smt2` (smt2-lib format)

## qfuf_specification

This section contains the target specification wanted (used when the logic is `qfuf`). For example in the case of STLC:

``` dedukti
[]  cic.Axiom cic.prop cic.type                --> cic.true.

[]  cic.Rule cic.prop cic.prop cic.prop        --> cic.true.
```

## lra_specification

This section contains the arithmetic interpretation of the predicates `Axiom`, `Rule` and `Cumul`.
This interpretation has to be a term over the following signature:

- eq/2, ite/3, and/2, or/2, true/0, false/0
- succ/1, max/2, imax/2, le/2, plus/2, zero/0

`imax(a,b)` is a shortcut for `ite (eq b zero) zero (max a b)`

For example:

``` dedukti
[a,b]   axiom a b   --> eq (succ a) b.
[a,b,c] rule  a b c --> eq (imax a b) c.
[a,b]   cumul a b   --> eq a b.
```

## end

Contains nothing, but is mandatory.
