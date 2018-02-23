type model = Basic.ident -> Term.term

val solve : Constraints.Basic.CS.t -> model
