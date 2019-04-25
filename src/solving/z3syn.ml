module B = Basic
module O = Common.Oracle
module U = Common.Universes
module Z = Z3cfg

(** Z3 Solver with non interpreted symbol Functions *)

type t = Z.Expr.expr

(* Z3 type for universes *)
let sort      = Z.Sort.mk_uninterpreted_s Z.ctx "Sort"

(** [var_of_name name] returns a variable string for Z3. *)
let mk_name cst = B.string_of_mident (B.md cst) ^ (B.string_of_ident (B.id cst))

(** non-interpreted symbol for Type i *)
(* FIXME: should be given by the model *)
let mk_enum i =
  Z.Expr.mk_const_s Z.ctx ("Enum"^string_of_int i) sort

let mk_var s = Z.Expr.mk_const_s Z.ctx s sort

(** [mk_univ u] construct a Z3 expression from a universe. *)
let mk_univ  = function
  | U.Var x -> mk_var (B.string_of_ident (B.id x))
  | U.Enum i -> mk_enum i

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

(** [register_vars vars i] give bound for each variable [var] between [0] and [i] *)
let mk_bounds var i =
  let univs = O.enumerate i in
  let or_eqs = List.map (fun u -> Z.Boolean.mk_eq Z.ctx (mk_var var) (mk_univ u)) univs in
  Z.Boolean.mk_or Z.ctx or_eqs

(** [solution_of_var univs model var] looks for the concrete universe associated to [var]
    in the [model]. Such universe satisfy that model(univ) = model(var). *)
let solution_of_var i model var =
  let univs = O.enumerate i in
  let exception Found of U.univ in
  let find_univ e u  =
    match Z.Model.get_const_interp_e model (mk_univ u) with
    | None -> assert false
    | Some u' ->
      if e = u' then raise (Found u) else ()
  in
  match Z.Model.get_const_interp_e model (mk_var var) with
  | None -> assert false
  | Some e ->
    try
      List.iter (find_univ e) univs;
      None
    with Found(u) -> Some u
