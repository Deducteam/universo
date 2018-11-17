open Basic

type t =
  {
    out_fmt:Format.formatter;
    meta:Dkmeta.cfg
  }

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

exception Not_pred

let typ = mk_name md_universo (mk_ident "type")

let set = mk_name md_universo (mk_ident "set")

let prop = mk_name md_universo (mk_ident "prop")

let univ = mk_name md_universo (mk_ident "Univ")

let lift = mk_name md_universo (mk_ident "lift")

let axiom = mk_name md_universo (mk_ident "Axiom")

let rule = mk_name md_universo (mk_ident "Rule")

let cumul = mk_name md_universo (mk_ident "Cumul")

let z = mk_name md_universo (mk_ident "0")

let s = mk_name md_universo (mk_ident "S")

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

let true_ = Basic.(Term.mk_Const dloc (mk_name (mk_mident "universo") (mk_ident "True")))

let rec extract_level l =
  match l with
  | Term.Const(_,n) when Basic.name_eq n z -> 0
  | Term.App(f,l,[]) when is_const s f -> 1 + (extract_level l)
  | _ -> Format.eprintf "%a@." Pp.print_term l;
    assert false

let extract_univ s =
  match s with
  | Term.Const(_,n) when Basic.name_eq n prop -> Prop
  | Term.Const(_,n) when Basic.name_eq n set -> Set
  | Term.Const(_,n)  -> Var n
  | Term.App(f,l,[]) when is_const typ f -> Type (extract_level l)
  | _ -> assert false

let extract_pred t =
  match t with
  | Term.App(f,s,[s'])     when is_const axiom f ->
    Axiom(extract_univ s, extract_univ s')
  | Term.App(f,s,[s'])     when is_const cumul f ->
    Cumul(extract_univ s, extract_univ s')
  | Term.App(f,s,[s';s'']) when is_const rule f ->
    Rule(extract_univ s, extract_univ s', extract_univ s'')
  | _ -> raise Not_pred


let global_cstr = ref (C.empty)

let print_rule env cstr =
  let normalize t = Dkmeta.mk_term env.meta t in
  match cstr with
  | Pred(p) ->
    let left' = normalize (term_of_pred p) in
    let right' = normalize true_ in
    Format.fprintf env.out_fmt "@.[] %a --> %a.@." Pp.print_term left' Pp.print_term right'
  | EqVar(l,r) ->
    Format.fprintf env.out_fmt "@.[] %a --> %a.@." Pp.print_name l Pp.print_name r

let add_cstr env p =
  global_cstr := C.add p !global_cstr;
  print_rule env p

let mk_cstr env l r =
  assert(Term.term_eq r true_);
  let p = extract_pred l in
  add_cstr env (Pred p)

let mk_var_cstr env l r =
  let get_number s =
    int_of_string (String.sub s 1 (String.length s - 1))
  in
  let nl = get_number (string_of_ident @@ id l) in
  let nr = get_number (string_of_ident @@ id r) in
  if nr < nl then
    begin
      global_cstr := C.add (EqVar(l,r)) !global_cstr;
      add_cstr env (EqVar(l,r))
    end
  else
    begin
      global_cstr := C.add (EqVar(r,l)) !global_cstr;
      add_cstr env (EqVar(r,l))
    end

type model = (pred * bool) list

(* FIXME: do not scale for any CTS *)
let rec enumerate i =
  if i = 1 then
    [Prop]
  else
    Type (i-2)::(enumerate (i-1))

let is_true meta p =
  let t = term_of_pred p in
  let t' = Dkmeta.mk_term meta t in
  Term.term_eq (true_) t'

let mk_axiom_model meta s s' =
  let p = Axiom(s,s') in
  (p,is_true meta p)

let mk_cumul_model meta s s' =
  let p = Cumul(s,s') in
  (p, is_true meta p)

let mk_rule_model meta s s' s'' =
  let p = Rule(s,s',s'') in
  (p,is_true meta p)

let rec map3 f l1 l2 l3 =
  match l1,l2,l3 with
  | [],[],[] -> []
  | a::l1,b::l2,c::l3 -> f a b c::(map3 f l1 l2 l3)
  | _ -> assert false

let cartesian2 f l l' =
  List.concat (List.map (fun e -> List.map (fun e' -> f e e') l') l)

let cartesian3 f l l' l'' =
  List.concat (
    List.map (fun e ->
        List.concat (
          List.map (fun e' ->
              List.map (fun e'' -> f e e' e'') l'') l')) l)

(* FIXME: can be optimized by avoiding concatenation of lists *)
let mk_model meta (i:int) =
  let u  = enumerate i in
  let model_ax  = cartesian2 (fun l r -> mk_axiom_model meta l r) u u in
  let model_cu = cartesian2 (fun l r -> mk_cumul_model meta l r) u u in
  let model_ru = cartesian3 (fun l m r -> mk_rule_model meta l m r) u u u in
  model_ax@model_cu@model_ru
