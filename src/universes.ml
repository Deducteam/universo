open Basic

let pre_univ = Basic.(mk_name (mk_mident "universo") (mk_ident "var"))

let is_pre_univ = function
  | Term.Const(_,n) -> Basic.name_eq n pre_univ
  | _ -> false

type univ =
    Var of name
  | Prop
  | Set
  | Type of int
  | Succ of univ
  | Max of univ * univ
  | Rule of univ * univ

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

let is_const cst t =
  match t with
  | Term.Const(_,n) -> name_eq cst n
  | _ -> false

let is_uvar t =
  match t with
  | Term.Const(_,n) -> md n = !md_univ
  | _ -> false

let is_univ t =
  match t with
  | Term.App(f,_,[]) when is_const univ f -> true
  | _ -> false

let is_lift t =
  match t with
  | Term.Const(_,n) -> md n = !md_univ
  | Term.App(f,_,[_;_]) when is_const lift f -> true
  | _ -> false

let is_max t =
  match t with
  | Term.App(f,_,[_]) when is_const max f -> true
  | _ -> false

let rec mem cst r =
  match r with
  | Term.Const(_,n) -> n = cst
  | Term.App(f,a,args) -> List.exists (fun t -> mem cst t) (f::a::args)
  | _ -> false

let rec extract_level l =
  match l with
  | Term.Const(_,n) when Basic.name_eq n z -> 0
  | Term.App(f,l,[]) when is_const s f -> 1 + (extract_level l)
  | _ -> Format.eprintf "%a@." Pp.print_term l;
    assert false

let extract_lift t =
  match t with
  | Term.App(f,s1,[s2;a]) when is_const lift f -> s1,s2
  | _ -> Format.eprintf "%a@." Pp.print_term t; assert false

let rec extract_univ t =
  let open Basic in
  match t with
  | Term.Const(_,n) when md n = !md_univ -> Var n
  | Term.Const(_,n) when Basic.name_eq n prop -> Prop
  | Term.Const(_,n) when Basic.name_eq n set -> Set
  | Term.App(f,u,[]) when is_const univ f -> extract_univ u
  | Term.App(f,l,[]) when is_const typ f -> Type (extract_level l)
  | Term.App(f,l,[r]) when is_const max f -> Max(extract_univ l, extract_univ r)
  | Term.App(f,l,[r]) when is_const rule f -> Rule(extract_univ l, extract_univ r)
  | Term.App(f,l, []) when is_const succ f -> Succ (extract_univ l)
  | _ -> raise Not_univ

let rec occur n u =
  match u with
  | Prop
  | Set
  | Type _ -> false
  | Var m -> Basic.name_eq m n
  | Max(m,o)
  | Rule(m,o) -> occur n m || occur n o
  | Succ m -> occur n m

let get_number s =
  int_of_string (String.sub s 1 (String.length s - 1))

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

let rec pattern_of_level l =
  let lc = Basic.dloc in
  if l = 0 then
    Rule.Pattern(lc,z,[])
  else
    Rule.Pattern(lc,s,[pattern_of_level (l-1)])

let rec pattern_of_univ u =
  let lc = Basic.dloc in
  match u with
  | Var n -> Rule.Pattern(lc, n, [])
  | Max(l,r) -> Rule.Pattern(lc, max, [pattern_of_univ l;pattern_of_univ r])
  | Rule(l,r) -> Rule.Pattern(lc, rule, [pattern_of_univ l;pattern_of_univ r])
  | Succ(l) -> Rule.Pattern(lc, succ, [pattern_of_univ l])
  | Set-> Rule.Pattern(lc, set, [])
  | Prop -> Rule.Pattern(lc, prop, [])
  | Type l ->  Rule.Pattern(lc, typ, [pattern_of_level l])


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

let rec pp_univ fmt = function
  | Var n       -> Format.fprintf fmt "?%a" Pp.print_ident (Basic.id n)
  | Set         -> Format.fprintf fmt "S"
  | Prop        -> Format.fprintf fmt "P"
  | Type i      -> Format.fprintf fmt "T%d" i
  | Succ u      -> Format.fprintf fmt "S(%a)" pp_univ u
  | Max(ul,ur)  -> Format.fprintf fmt "M(%a,%a)" pp_univ ul pp_univ ur
  | Rule(ul,ur) -> Format.fprintf fmt "R(%a,%a)" pp_univ ul pp_univ ur
