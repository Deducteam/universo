module B = Basic
module U = Common.Universes
module Z = Z3
module ZA = Z.Arithmetic
module ZB = Z.Boolean
module ZI = ZA.Integer

type t = Z.Expr.expr
type model = Z.Model.model
type ctx = Z.context

let mk_name : B.name -> string = fun name ->
  B.string_of_mident (B.md name) ^ (B.string_of_ident (B.id name))

let int_sort ctx = ZI.mk_sort ctx

let mk_var  : ctx -> string -> t = fun ctx s ->
  Z3.Expr.mk_const_s ctx s (int_sort ctx)

let to_int : ctx -> int -> t = fun ctx i -> ZI.mk_numeral_i ctx i

let mk_prop : ctx -> t = fun ctx -> to_int ctx 0

let mk_set : ctx -> t = fun ctx -> to_int ctx 1

let mk_type : ctx -> int -> t = fun ctx i -> to_int ctx (i+1)

let mk_univ : ctx -> U.univ -> t = fun _ -> failwith "todo mk_univ z3arith"

let mk_axiom : ctx -> t -> t -> t = fun ctx l r ->
  ZB.mk_ite ctx (ZB.mk_eq ctx l (mk_prop ctx))
    (ZB.mk_eq ctx (ZA.mk_add ctx [l;(to_int ctx 1)]) r)
    (ZB.mk_eq ctx (ZA.mk_add ctx [l;(to_int ctx 1)]) r)

let mk_cumul : ctx -> t -> t -> t = fun ctx l r -> ZA.mk_le ctx l r

let mk_max : ctx -> t -> t -> t = fun ctx l r ->
  ZB.mk_ite ctx (ZA.mk_le ctx l r) r l

let mk_rule : ctx -> t -> t -> t -> t = fun ctx x y z ->
  ZB.mk_ite ctx (ZB.mk_eq ctx y (mk_prop ctx))
    (ZB.mk_eq ctx z (mk_prop ctx))
    (ZB.mk_eq ctx z (mk_max ctx x y))

let mk_bounds : ctx -> string -> int -> t = fun _ _ ->
  failwith "mk_bounds (z3arith)"

let solution_of_var : ctx -> int -> Z.Model.model -> string -> U.univ option = fun ctx _ model var ->
  match Z.Model.get_const_interp_e model (mk_var ctx var) with
  | None -> assert false
  | Some e ->
    let _ = Big_int.int_of_big_int (ZI.get_big_int e) in
    failwith "solution_of_var (z3 arith)"
