module type LOGIC =
sig
  type t
  type model
  type ctx

  val mk_name   : Basic.name -> string
  val mk_var    : ctx -> string -> t
  val mk_univ   : ctx -> Common.Universes.univ -> t
  val mk_axiom  : ctx -> t -> t -> t
  val mk_cumul  : ctx -> t -> t -> t
  val mk_rule   : ctx -> t -> t -> t -> t
  val mk_bounds : ctx -> string -> int -> t
  val solution_of_var : ctx -> int -> model -> string -> Common.Universes.univ option
end

(** [model] is a function that associate to each fresh universe a concrete universe. *)
type model = Basic.name -> Common.Universes.univ

type env =
  {
    mk_theory: Common.Oracle.theory_maker;
    (** construct a list of axioms,rules and cumulativity with there truth value for n universes. *)
    min: int;
    (** minimum number of universes to check *)
    max: int;
    (** maximum number of universes to check *)
    print: bool;
    (** print the problem in a file *)
  }

module type SMTSOLVER =
sig

  (** [add pred] add the predicate [cstr] to the solver  *)
  val add   : Common.Universes.cstr -> unit

  (** [solve mk_theory] call the solver and returns the mimimum number of universes needed to solve the constraints as long as the model. The theory used by solver depends on the number of universes needed. Hence one needs to provide a function [mk_theory] that builds a theory when at most [i] are used.*)
  val solve : env -> int * model
end

module type SOLVER =
sig
  val parse : Dkmeta.cfg -> Common.Files.path -> unit

  val print_model : Dkmeta.cfg -> model -> Common.Files.path -> unit

  val solve : env -> int * model
end
