module type S =
sig
  val elab_term : Signature.t -> Term.term -> Term.term

  val elab_entry : Signature.t -> Entry.entry -> Entry.entry
end


module Make(U:Uvar.S) : S
