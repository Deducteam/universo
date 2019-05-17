open Basic

type univ =
    Var of name
  | Enum of int


type pred =
  | Axiom of univ * univ
  | Cumul of univ * univ
  | Rule  of univ * univ * univ

type cstr = Pred of pred | EqVar of name * name

module C = Set.Make(struct type t = cstr let compare = compare end)

let md_theory = ref @@ mk_mident ""

let md_univ = ref (mk_mident "")


let pvar  () = mk_name !md_theory (mk_ident "var")

let sort  () = mk_name !md_theory (mk_ident "Sort")

let typ   () = mk_name !md_theory (mk_ident "type")

let set   () = mk_name !md_theory (mk_ident "set")

let prop  () = mk_name !md_theory (mk_ident "prop")

let univ  () = mk_name !md_theory (mk_ident "Univ")

let cast' () = mk_name !md_theory (mk_ident "cast'")

let axiom () = mk_name !md_theory (mk_ident "Axiom")

let rule  () = mk_name !md_theory (mk_ident "Rule")

let cumul () = mk_name !md_theory (mk_ident "Cumul")

let enum  () = mk_name !md_theory (mk_ident "enum")

let uzero () = mk_name !md_theory (mk_ident "uzero")

let usucc () = mk_name !md_theory (mk_ident "usucc")

let subtype () = mk_name !md_theory (mk_ident "SubType")

let forall () = mk_name !md_theory (mk_ident "forall")

let true_ () = Term.mk_Const dloc (mk_name !md_theory (mk_ident "true"))

let sinf () = Term.mk_Const dloc (mk_name !md_theory (mk_ident "sinf"))

let rec term_of_level i =
  assert (i>=0);
  if i = 0 then
    Term.mk_Const Basic.dloc (uzero ())
  else
    Term.mk_App (Term.mk_Const Basic.dloc (usucc ())) (term_of_level (i-1)) []

let term_of_univ u =
  let lc = Basic.dloc in
  match u with
  | Var n -> Term.mk_Const lc n
  | Enum i -> Term.mk_App (Term.mk_Const Basic.dloc (enum ())) (term_of_level i) []

let term_of_pred p =
  let lc = Basic.dloc in
  match p with
  | Axiom(s,s') -> Term.mk_App2 (Term.mk_Const lc (axiom ()))
                     [term_of_univ s;term_of_univ s']
  | Cumul(s,s') -> Term.mk_App2 (Term.mk_Const lc (cumul ()))
                     [term_of_univ s;term_of_univ s']
  | Rule(s,s',s'') -> Term.mk_App2 (Term.mk_Const lc (rule ()))
                        [term_of_univ s; term_of_univ s'; term_of_univ s'']

let pattern_of_level _ = failwith "todo pattern of level"

let is_const cst t =
  match t with
  | Term.Const(_,n) -> name_eq cst n
  | _ -> false


let is_var md_elab t =
  match t with
  | Term.Const(_,n) -> md n = md_elab
  | _ -> false

let is_enum t =
  match t with
  | Term.App(f,_,[]) when is_const (enum ()) f -> true
  | _ -> false

let is_subtype t =
  match t with
  | Term.App(f,_,[_;_;_]) when is_const (subtype ()) f -> true
  | _ -> false

let extract_subtype t =
  match t with
  | Term.App(f,_,[_;_;_]) as s when is_const (subtype ()) f -> s
  | _ -> assert false

let is_forall t =
  match t with
  | Term.App(f,_,[_;_]) when is_const (forall ()) f -> true
  | _ -> false

let extract_forall t =
  match t with
  | Term.App(f,_,[_;Term.Lam(_,_,_,t)]) when is_const (forall ()) f -> t
  | _ -> assert false

let is_cast' t =
  match t with
  | Term.App(f,_,_) when is_const (cast' ()) f -> true
  | _ -> false

let extract_cast' t =
  match t with
  | Term.App(f,s1,[s2;a;b;t]) when is_const (cast' ()) f -> s1,s2,a,b,t
  | Term.App(f,s1,s2::a::b::m::n) when is_const (cast' ()) f -> s1,s2,a,b,Term.mk_App2 m n
  | _ -> Format.eprintf "%a@." Pp.print_term t; assert false

let rec extract_level : Term.term -> int = fun t ->
  match t with
  | Term.Const(_,n) when Basic.name_eq n (uzero ()) -> 0
  | Term.App (Term.Const(_,n),l,[]) when Basic.name_eq n (usucc ()) ->
    1 + (extract_level l)
  | _ -> assert false

exception Not_univ
exception Not_pred

let extract_univ : Term.term -> univ = fun t ->
  match t with
  | Term.Const(_,n) -> Var n
  | Term.App (Term.Const(_,c),n,[]) when Basic.name_eq c (enum ()) ->
    Enum (extract_level n)
  | _ -> Format.eprintf "%a@." Pp.print_term t; assert false

let extract_pred t =
  match t with
  | Term.App(f,s,[s'])     when is_const (axiom ()) f ->
    Axiom(extract_univ s, extract_univ s')
  | Term.App(f,s,[s'])     when is_const (cumul ()) f ->
    Cumul(extract_univ s, extract_univ s')
  | Term.App(f,s,[s';s'']) when is_const (rule ()) f ->
    Rule(extract_univ s, extract_univ s', extract_univ s'')
  | _ -> raise Not_pred
