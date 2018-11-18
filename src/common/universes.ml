open Basic

type univ =
    Var of name
  | Prop
  | Set
  | Type of int

type pred =
  | Axiom of univ * univ
  | Cumul of univ * univ
  | Rule  of univ * univ * univ

type cstr = Pred of pred | EqVar of name * name

module C = Set.Make(struct type t = cstr let compare = compare end)

let md_universo = mk_mident "universo"
let md_univ = ref (mk_mident "")

let sort  = mk_name md_universo (mk_ident "Sort")

let typ   = mk_name md_universo (mk_ident "type")

let set   = mk_name md_universo (mk_ident "set")

let prop  = mk_name md_universo (mk_ident "prop")

let univ  = mk_name md_universo (mk_ident "Univ")

let lift  = mk_name md_universo (mk_ident "lift")

let axiom = mk_name md_universo (mk_ident "Axiom")

let rule  = mk_name md_universo (mk_ident "Rule")

let cumul = mk_name md_universo (mk_ident "Cumul")

let pvar  = mk_name md_universo (mk_ident "var")

let z     = mk_name md_universo (mk_ident "0")

let s     = mk_name md_universo (mk_ident "S")


let true_ = Basic.(Term.mk_Const dloc (mk_name (md_universo) (mk_ident "True")))

let rec term_of_level l =
  let lc = Basic.dloc in
  if l = 0 then
    Term.mk_Const lc z
  else
    Term.mk_App2 (Term.mk_Const lc s) [(term_of_level (l-1))]

let term_of_univ u =
  let lc = Basic.dloc in
  match u with
  | Var n -> Term.mk_Const lc n
  | Set    -> Term.mk_Const lc set
  | Prop   -> Term.mk_Const lc prop
  | Type l ->  Term.mk_App2 (Term.mk_Const lc typ) [term_of_level l]

let term_of_pred p =
  let lc = Basic.dloc in
  match p with
  | Axiom(s,s') -> Term.mk_App2 (Term.mk_Const lc axiom)
                     [term_of_univ s;term_of_univ s']
  | Cumul(s,s') -> Term.mk_App2 (Term.mk_Const lc cumul)
                     [term_of_univ s;term_of_univ s']
  | Rule(s,s',s'') -> Term.mk_App2 (Term.mk_Const lc rule)
                        [term_of_univ s; term_of_univ s'; term_of_univ s'']

let rec pattern_of_level l =
  let lc = Basic.dloc in
  if l = 0 then
    Rule.Pattern(lc,z,[])
  else
    Rule.Pattern(lc,s,[pattern_of_level (l-1)])

let is_const cst t =
  match t with
  | Term.Const(_,n) -> name_eq cst n
  | _ -> false


let is_var md_elab t =
  match t with
  | Term.Const(_,n) -> md n = md_elab
  | _ -> false

let is_lift t =
  match t with
  | Term.Const(_,n) -> md n = !md_univ
  | Term.App(f,_,[_;_]) when is_const lift f -> true
  | _ -> false

let extract_lift t =
  match t with
  | Term.App(f,s1,[s2;_]) when is_const lift f -> s1,s2
  | _ -> Format.eprintf "%a@." Pp.print_term t; assert false

let rec extract_level l =
  match l with
  | Term.Const(_,n) when Basic.name_eq n z -> 0
  | Term.App(f,l,[]) when is_const s f -> 1 + (extract_level l)
  | _ -> assert false

exception Not_univ
exception Not_pred

let extract_univ s =
  match s with
  | Term.Const(_,n) when Basic.name_eq n prop -> Prop
  | Term.Const(_,n) when Basic.name_eq n set -> Set
  | Term.Const(_,n)  -> Var n
  | Term.App(f,l,[]) when is_const typ f -> Type (extract_level l)
  | _ -> raise Not_univ

let extract_pred t =
  match t with
  | Term.App(f,s,[s'])     when is_const axiom f ->
    Axiom(extract_univ s, extract_univ s')
  | Term.App(f,s,[s'])     when is_const cumul f ->
    Cumul(extract_univ s, extract_univ s')
  | Term.App(f,s,[s';s'']) when is_const rule f ->
    Rule(extract_univ s, extract_univ s', extract_univ s'')
  | _ -> raise Not_pred
