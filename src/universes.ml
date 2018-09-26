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


    let univ = mk_name md_universo (mk_ident "Univ")

    let max = mk_name md_universo (mk_ident "max")

    let rule = mk_name md_universo (mk_ident "rule")

    let succ = mk_name md_universo (mk_ident "succ")

    let lift = mk_name md_universo (mk_ident "lift")

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

let rec to_univ t =
  let open Basic in
  match t with
  | Term.Const(_,n) when md n = !md_univ -> Var n
  | Term.App(f,l,[r]) when is_const max f -> Max(to_univ l, to_univ r)
  | Term.App(f,l,[r]) when is_const rule f -> Rule(to_univ l, to_univ r)
  | Term.App(f,l, []) when is_const succ f -> Succ (to_univ l)
  | _ -> raise Not_univ


let rec pattern_of_univ u =
  let lc = Basic.dloc in
  match u with
  | Var n -> Rule.Pattern(lc, n, [])
  | Max(l,r) -> Rule.Pattern(lc, max, [pattern_of_univ l;pattern_of_univ r])
  | Rule(l,r) -> Rule.Pattern(lc, rule, [pattern_of_univ l;pattern_of_univ r])
  | Succ(l) -> Rule.Pattern(lc, succ, [pattern_of_univ l])
  | _ -> assert false


let rec pp_univ fmt = function
  | Var n       -> Format.fprintf fmt "?%a" Pp.print_ident (Basic.id n)
  | Set         -> Format.fprintf fmt "S"
  | Prop        -> Format.fprintf fmt "P"
  | Type i      -> Format.fprintf fmt "T%d" i
  | Succ u      -> Format.fprintf fmt "S(%a)" pp_univ u
  | Max(ul,ur)  -> Format.fprintf fmt "M(%a,%a)" pp_univ ul pp_univ ur
  | Rule(ul,ur) -> Format.fprintf fmt "R(%a,%a)" pp_univ ul pp_univ ur
