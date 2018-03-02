val cic : Basic.mident

val z    : Basic.name

val s    : Basic.name

val prop : Basic.name

val typ  : Basic.name

val succ : Basic.name

val sort : Basic.name

val lift : Basic.name

val max  : Basic.name

val rule : Basic.name

val term : Basic.name

val prod : Basic.name

val cuni : Basic.name

val univ : Basic.name

val is_prop : Term.term -> bool

val is_z    : Term.term -> bool

val is_s    : Term.term -> bool

val is_type : Term.term -> bool

val is_succ : Term.term -> bool

val is_lift : Term.term -> bool

val is_max  : Term.term -> bool

val is_rule : Term.term -> bool

val is_univ : Term.term -> bool

val is_cuni : Term.term -> bool

val is_term : Term.term -> bool

val is_prod : Term.term -> bool

val is_lam  : Term.term -> bool

val is_var  : Term.term -> bool

val is_app  : Term.term -> bool

val extract_var  : Term.term -> Basic.ident

val extract_s    : Term.term -> Term.term

val extract_type : Term.term -> Term.term

val extract_succ : Term.term -> Term.term

val extract_univ : Term.term -> Term.term

val extract_cuni : Term.term -> Term.term

val extract_term : Term.term -> (Term.term * Term.term)

val extract_max  : Term.term -> (Term.term * Term.term)

val extract_rule : Term.term -> (Term.term * Term.term)

val extract_app  : Term.term -> (Term.term * Term.term * Term.term list)

val extract_lam  : Term.term -> (Basic.ident * Term.term * Term.term)

val extract_prod : Term.term -> (Term.term * Term.term * Term.term * Term.term)

val extract_lift : Term.term -> (Term.term * Term.term * Term.term)
