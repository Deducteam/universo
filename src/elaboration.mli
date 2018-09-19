module type S =
  sig
    val elab_term    : Configuration.t -> Term.term -> Term.term

    val elab_entry   : Configuration.t -> Entry.entry -> Entry.entry
  end


module MakeUniv(U:Uvar.S) : S

module MakePre(T:Theory.In) : S

module Make(T:Theory.In)(U:Uvar.S) : S
