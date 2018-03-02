type model = Basic.ident -> Term.term

val solve : Constraints.CS.t -> model
