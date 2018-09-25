exception Not_uvar

module type S =
sig
  val is_uvar      : Term.term -> bool

  val name_of_uvar : Term.term -> Basic.name

  val fresh_uvar   : Configuration.t -> Term.term

  val count        : unit -> int
end

module Make(T:Theory.S) : S
