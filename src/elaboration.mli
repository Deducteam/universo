type cfg =
  {
    sg:Signature.t;
    oc:out_channel option
  }

module type S =
  sig
    val elab_term    : cfg -> Term.term -> Term.term

    val elab_entry   : cfg -> Entry.entry -> Entry.entry
  end


module MakeUniv(U:Uvar.S) : S

module MakePre(T:Theory.In) : S

module Make(T:Theory.In)(U:Uvar.S) : S
