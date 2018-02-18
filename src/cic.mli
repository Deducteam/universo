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

val is_prop : Term.term -> bool

val is_type : Term.term -> bool

val is_succ : Term.term -> bool

val is_lift : Term.term -> bool

val is_max  : Term.term -> bool

val is_rule : Term.term -> bool

val extract_type : Term.term -> Term.term

val extract_succ : Term.term -> Term.term

val extract_lift : Term.term -> (Term.term * Term.term)

val extract_max  : Term.term -> (Term.term * Term.term)

val extract_rule : Term.term -> (Term.term * Term.term)
