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

let univ = mk_name cic (mk_ident "Univ")

let cuni = mk_name cic (mk_ident "univ")

let term = mk_name cic (mk_ident "Term")

let prod = mk_name cic (mk_ident "prod")

let is_const cst t =
  match t with
  | Term.Const(_,n) -> name_eq cst n
  | _ -> false

let is_z t =
  match t with
  | Term.Const(_,u) when is_const z t -> true
  | _ -> false

let is_s t =
  match t with
  | Term.App(u,_,[]) when is_const s u -> true
  | _ -> false

let is_term t =
  match t with
  | Term.App(u,_,[_]) when is_const term u -> true
  | _ -> false

let is_univ t =
  match t with
  | Term.App(u,_,[]) when is_const univ u -> true
  | _ -> false

let is_cuni t =
  match t with
  | Term.App(u,_,[]) when is_const cuni u -> true
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

let is_prod t =
  match t with
  | Term.App(c, s1, [s2;a;f]) when is_const prod c -> true
  | _ -> false

let is_lam t =
  match t with
  | Term.Lam _ -> true
  | _ -> false

let extract_s t =
  match t with
  | Term.App(t,u,[]) when is_const s t -> u
  | _ -> failwith "is not a s"

let extract_type t =
  match t with
  | Term.App(t,u,[]) when is_const typ t -> u
  | _ -> failwith "is not a type"

let extract_term t =
  match t with
  | Term.App(t,s,[u]) when is_const term t -> s,u
  | _ -> failwith "is not a term"

let extract_succ t =
  match t with
  | Term.App(c,arg,[]) when is_const succ c -> arg
  | _ -> failwith "is not a succ"

let extract_lift t =
  match t with
  | Term.App(c,s1,[s2;a]) when is_const lift c -> s1,s2,a
  | _ -> failwith "is not a lift"

let extract_max t =
  match t with
  | Term.App(c,s1,[s2]) when is_const max c -> s1,s2
  | _ -> failwith "is not a max"

let extract_rule t =
  match t with
  | Term.App(c, s1, [s2]) when is_const rule c -> s1, s2
  | _ -> failwith "is not a rule"

let extract_univ t =
  match t with
  | Term.App(c, s, []) when is_const univ c -> s
  | _ -> failwith "is not a univ"

let extract_cuni t =
  match t with
  | Term.App(c, s, []) when is_const cuni c -> s
  | _ -> failwith "is not a cuni"

let extract_prod t =
  match t with
  | Term.App(c, s1, [s2;a;f]) when is_const prod c -> s1,s2,a,f
  | _ -> failwith "is not a prod"

let extract_lam t =
  match t with
  | Term.Lam(_,x,Some ty,te) -> x,ty,te
  | _ -> failwith "not a lambda or lambda without a type"
