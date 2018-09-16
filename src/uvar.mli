
exception Not_uvar

module type S =
sig
  val is_uvar      : Term.term -> bool

  val name_of_uvar : Term.term -> Basic.name

  val fresh_uvar   : ?oc:out_channel option -> Signature.t -> Term.term

  val count        : unit -> int
end

module Uvar : S


module Make(TO:Theory.Out) : S
