module type S =
sig
  val elab_term : Signature.t -> Term.term -> Term.term

  val elab_entry : Signature.t -> Entry.entry -> Entry.entry
end


module MakeUniv(U:Uvar.S) : S

module MakePre(T:Theory.Compat) : S

module Make(T:Theory.Compat)(U:Uvar.S) : S
