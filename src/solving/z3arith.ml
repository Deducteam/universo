module B = Basic
module U = Common.Universes
module Z = Z3cfg
module ZA = Z.Arithmetic
module ZB = Z.Boolean
module ZI = ZA.Integer

type t = Z.Expr.expr

let mk_name : B.name -> string = fun name ->
  B.string_of_mident (B.md name) ^ (B.string_of_ident (B.id name))

let int_sort = ZI.mk_sort Z.ctx

let mk_var  : string -> t = fun s ->
  Z.Expr.mk_const_s Z.ctx s int_sort

let to_int : int -> t = fun i -> ZI.mk_numeral_i Z.ctx i

let mk_prop : t = to_int 0

let mk_set : t = to_int 1

let mk_type : int -> t = fun i -> to_int (i+1)

let mk_univ : U.univ -> t = function
  | Var cst -> mk_var (mk_name cst)
  | Prop -> mk_prop
  | Set -> mk_set
  | Type(i) -> mk_type i

let mk_axiom : t -> t -> t = fun l r ->
  ZB.mk_ite Z.ctx (ZB.mk_eq Z.ctx l mk_prop)
    (ZB.mk_eq Z.ctx (ZA.mk_add Z.ctx [l;(to_int 1)]) r)
    (ZB.mk_eq Z.ctx (ZA.mk_add Z.ctx [l;(to_int 1)]) r)

let mk_cumul : t -> t -> t = fun l r -> ZA.mk_le Z.ctx l r

let mk_max : t -> t -> t = fun l r ->
  ZB.mk_ite Z.ctx (ZA.mk_le Z.ctx l r) r l

let mk_rule : t -> t -> t -> t = fun x y z ->
  ZB.mk_ite Z.ctx (ZB.mk_eq Z.ctx y mk_prop)
    (ZB.mk_eq Z.ctx z mk_prop)
    (ZB.mk_eq Z.ctx z (mk_max x y))

let mk_bounds : string -> int -> t = fun var i ->
  let var = mk_var var in
  ZB.mk_and Z.ctx [ZA.mk_le Z.ctx (to_int 0) var; ZA.mk_lt Z.ctx var (to_int i)]

let solution_of_var : int -> Z.Model.model -> string -> U.univ option = fun _ model var ->
  match Z.Model.get_const_interp_e model (mk_var var) with
  | None -> assert false
  | Some e ->
    let i = Big_int.int_of_big_int (ZI.get_big_int e) in
    if i = 0 then
      Some U.Prop
    else if i = 1 then
      Some (U.Type 0)
    else
      Some (U.Type (i - 1))
