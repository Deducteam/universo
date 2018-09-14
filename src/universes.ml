let pre_univ = Basic.(mk_name (mk_mident "universo") (mk_ident "var"))

let is_pre_univ = function
  | Term.Const(_,n) -> Basic.name_eq n pre_univ
  | _ -> false

type univ =
    Var of Basic.name
  | Prop
  | Set
  | Type of int
  | Succ of univ
  | Max of univ * univ
  | Rule of univ * univ

let rec pp_univ fmt = function
  | Var n       -> Format.fprintf fmt "?%a" Pp.print_ident (Basic.id n)
  | Set         -> Format.fprintf fmt "S"
  | Prop        -> Format.fprintf fmt "P"
  | Type i      -> Format.fprintf fmt "T%d" i
  | Succ u      -> Format.fprintf fmt "S(%a)" pp_univ u
  | Max(ul,ur)  -> Format.fprintf fmt "M(%a,%a)" pp_univ ul pp_univ ur
  | Rule(ul,ur) -> Format.fprintf fmt "R(%a,%a)" pp_univ ul pp_univ ur
