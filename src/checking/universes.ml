open Basic

type env =
  {
    out_fmt:Format.formatter;
    meta:Dkmeta.cfg
  }

type univ =
    Var of name
  | Prop
  | Set
  | Type of int
  | Succ of univ
  | Max of univ * univ
  | Rule of univ * univ

type cstr = univ * univ

module C = Set.Make(struct type t = cstr let compare = compare end)

let get_number s =
  int_of_string (String.sub s 1 (String.length s - 1))

(* Implement RPO order with Succ > Max ; should be total FIXME *)
let rec gt l r =
  match (l,r) with
  | Var n, Var m ->
    let n = get_number (string_of_ident @@ id n) in
    let m = get_number (string_of_ident @@ id m) in
    n > m
  | Var _, Prop
  | Var _, Set
  | Var _, Type _ -> true
  | Var _, Max(m,n) ->
    gt l m && gt l n
  | Var _, Rule(m,n) ->
    gt l m && gt l n
  | Var _, Succ m ->
    gt l m
  | Max(l1,r1), Max(l2,r2) ->
    gt l1 l2 || (l1 = l2 && gt r1 r2)
  | Succ l, Succ m ->
    gt l m
  | Succ _, Max(m,n) ->
    gt l m && gt l n
  | Max(m,n), Succ _ ->
    le m r || le n r
  | Max(m,n), Var _ ->
    le m r || le n r
  | Rule(m,n), Var _ ->
    le m r || le n r
  | Succ m, Var _ ->
    le m r
  | _ -> false

and le l r =
  l = r || gt l r

let md_universo = mk_mident "universo"
let md_univ = ref (mk_mident "")

exception Not_univ

let typ = mk_name md_universo (mk_ident "type")

let set = mk_name md_universo (mk_ident "set")

let prop = mk_name md_universo (mk_ident "prop")

let univ = mk_name md_universo (mk_ident "Univ")

let max = mk_name md_universo (mk_ident "max")

let rule = mk_name md_universo (mk_ident "rule")

let succ = mk_name md_universo (mk_ident "succ")

let lift = mk_name md_universo (mk_ident "lift")

let z = mk_name md_universo (mk_ident "0")

let s = mk_name md_universo (mk_ident "S")

let rec term_of_level l =
  let lc = Basic.dloc in
  if l = 0 then
    Term.mk_Const lc z
  else
    Term.mk_App2 (Term.mk_Const lc s) [(term_of_level (l-1))]

let rec term_of_univ u =
  let lc = Basic.dloc in
  match u with
  | Var n -> Term.mk_Const lc n
  | Max(l,r) -> Term.mk_App2 (Term.mk_Const lc max) [term_of_univ l;term_of_univ r]
  | Rule(l,r) -> Term.mk_App2 (Term.mk_Const lc rule) [term_of_univ l;term_of_univ r]
  | Succ(l) -> Term.mk_App2 (Term.mk_Const lc succ) [term_of_univ l]
  | Set    -> Term.mk_Const lc set
  | Prop   -> Term.mk_Const lc prop
  | Type l ->  Term.mk_App2 (Term.mk_Const lc typ) [term_of_level l]

let is_uvar t =
  match t with
  | Term.Const(_,n) -> md n = !md_univ
  | _ -> false

let set = ref (C.empty)

let print_rule env left right =
  let normalize t = Dkmeta.mk_term env.meta t in
  let left' = normalize (term_of_univ left) in
  let right' = normalize (term_of_univ right) in
  Format.fprintf env.out_fmt "[] %a --> %a.@." Pp.print_term left' Pp.print_term right'

let add_cstr env left right =
  set := C.add (left,right) !set;
  print_rule env left right

let mk_cstr env left right =
  assert (left <> right);
  if gt left right then
    add_cstr env left right
  else
    begin
      assert (gt right left);
      add_cstr env right left
    end
