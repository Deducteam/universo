open Basic

let cic = mk_mident "cic"

let mk_const id = Term.mk_Const dloc (mk_name cic id)

let z = mk_name cic (mk_ident "z")

let s = mk_name cic (mk_ident "s")

let succ = mk_name cic (mk_ident "succ")

let sort = mk_name cic (mk_ident "Sort")

let lift = mk_name cic (mk_ident "lift")

let max = mk_name cic (mk_ident "max")

let rule = mk_name cic (mk_ident "rule")

let prop = mk_name cic (mk_ident "prop")

let typ = mk_name cic (mk_ident "type")

let is_const cst t =
  match t with
  | Term.Const(_,n) -> name_eq cst n
  | _ -> false

let is_prop t =
  match t with
  | Term.Const(_,n) when is_const prop t -> true
  | _ -> false

let is_type t =
  match t with
  | Term.App(t,_,[]) when is_const typ t -> true
  | _ -> false

let is_succ t =
  match t with
  | Term.App(c,arg,[]) when is_const succ c -> true
  | _ -> false

let is_lift t =
  match t with
  | Term.App(c, s1, [s2;a]) when is_const lift c -> true
  | _ -> false

let is_max t =
  match t with
  | Term.App(c, s1, [s2]) when is_const max c -> true
  | _ -> false

let is_rule t =
  match t with
  | Term.App(c, s1, [s2]) when is_const rule c -> true
  | _ -> false

let extract_type t =
  let rec to_int t =
    match t with
    | Term.Const(_,z) when is_const z t -> 0
    | Term.App(t,u, []) when is_const s t -> 1+(to_int u)
    | _ -> assert false
  in
  match t with
  | Term.App(t,u,[]) when is_const typ t -> to_int u
  | _ -> failwith "is not a type"

let extract_succ t =
  match t with
  | Term.App(c,arg,[]) when is_const succ c -> arg
  | _ -> failwith "is not a succ"

let extract_lift t =
  match t with
  | Term.App(c,s1,[s2;a]) when is_const lift c -> s1,s2
  | _ -> failwith "is not a lift"

let extract_max t =
  match t with
  | Term.App(c,s1,[s2]) when is_const max c -> s1,s2
  | _ -> failwith "is not a max"

let extract_rule t =
  match t with
  | Term.App(c, s1, [s2]) when is_const rule c -> s1, s2
  | _ -> failwith "is not a rule"
