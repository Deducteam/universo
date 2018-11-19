module U = Common.Universes
module Z = Z3cfg

(** Z3 Solver with non interpreted symbol Functions *)

type t = Z.Expr.expr

(* Z3 type for universes *)
let sort      = Z.Sort.mk_uninterpreted_s Z.ctx "Sort"

(** non-interpreted symbol for Prop *)
(* FIXME: should be given by the model *)
let mk_prop =
  Z.Expr.mk_const_s Z.ctx "Prop" sort

(** non-interpreted symbol for Set *)
(* FIXME: should be given by the model *)
let mk_set =
  Z.Expr.mk_const_s Z.ctx "Set" sort


(** [var_of_name name] returns a variable string for Z3. ASSUME that all identifiers representing fresh universe variables are unique, hence the module can be forgetten. *)
let mk_name cst = Basic.string_of_ident (Basic.id cst)

(** non-interpreted symbol for Type i *)
(* FIXME: should be given by the model *)
let mk_type i =
  Z.Expr.mk_const_s Z.ctx ("Type"^string_of_int i) sort

let mk_var s = Z.Expr.mk_const_s Z.ctx s sort

(** [mk_univ u] construct a Z3 expression from a universe. *)
let mk_univ  = fun t ->
  let open U in
  match t with
  | Var cst -> mk_var (mk_name cst)
  | Prop -> mk_prop
  | Set -> mk_set
  | Type(i) -> mk_type i

let bool_sort = Z.Boolean.mk_sort Z.ctx

(** [mk_axiom s s'] construct the Z3 predicate associated to the Axiom Predicate *)
let mk_axiom s s' =
  let axiom = Z.FuncDecl.mk_func_decl_s Z.ctx "A" [sort;sort] bool_sort in
  Z.Expr.mk_app Z.ctx axiom [s;s']

(** [mk_cumul s s'] construct the Z3 predicate associated to the Cumul Predicate *)
let mk_cumul s s' =
  let cumul = Z.FuncDecl.mk_func_decl_s Z.ctx "C" [sort;sort] bool_sort in
  Z.Expr.mk_app Z.ctx cumul [s;s']

(** [mk_rule s s' s''] construct the Z3 predicate associated to the Rule Predicate *)
let mk_rule s s' s'' =
  let cumul = Z.FuncDecl.mk_func_decl_s Z.ctx "R" [sort;sort;sort] bool_sort in
  Z.Expr.mk_app Z.ctx cumul [s;s';s'']
