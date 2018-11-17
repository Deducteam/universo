(** model is a function that associate to each fresh universe a concrete universe *)
type model = Basic.name -> Common.Universes.univ

(** Signature for an abstract solver *)
module type SOLVER =
sig

  (** [parse md_elab md_check compat s] parse the file [s] containing constraints generated before.  *)
  (* FIXME: Interface is to specific here, this should be encapsulated *)
  val parse   : Basic.mident -> Basic.mident -> string -> string -> unit

  (** [solve meta] call the solver and returns the maximum number of universes needed and the model found *)
  (* FIXME: probably the interface should specify an upper bound instead of hard-coding one *)
  val solve   : Dkmeta.cfg -> int * model

  (** [reset ()] resets the solver *)
  val reset   : unit -> unit
end

(** A concrete implementation of a solver using Z3 with non interpreted symbol functions for universes *)
module Z3Syn : SOLVER  = Z3solver.Syn
