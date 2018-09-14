
exception Not_uvar

module type S =
sig
  val is_uvar      : Term.term -> bool

  val uvar_of_term : Term.term -> Basic.name

  val fresh_uvar   : Signature.t -> Basic.name

  val count        : unit -> int
end

module UVar : S
