module B = Basic
module F = Files
module U = Universes

type t =
  {
    file: F.cout F.t;
    meta:Dkmeta.cfg
  }

type print_cstrs =
  {
    eqvar: (B.name * B.name) list;
    axiom: (U.univ * U.univ) list;
    cumul: (U.univ * U.univ) list;
    rule: (U.univ * U.univ * U.univ) list;
  }

let predicative = ref false

let dummy_name = Rule.Gamma(false, B.mk_name (B.mk_mident "dummy") (B.mk_ident "dummy"))

(* FIXME: copy/paste from checker.ml *)
let add_rule  sg vl vr =
  let pat = Rule.Pattern(Basic.dloc,vl,[]) in
  let rhs = Term.mk_Const Basic.dloc vr in
  let rule = Rule.(
      {
        ctx = [];
        pat;
        rhs;
        name=dummy_name;
      })
  in
  Signature.add_rules sg  [Rule.to_rule_infos rule]

let print_rule pp fmt (l,r) =
  Format.fprintf fmt "@.[] %a --> %a" pp l pp r

let print_eq_var fmt (l,r) =
  Format.fprintf fmt "%a.@." (print_rule Pp.print_name) (l,r)

let print_predicate fmt p =
  let l' = U.term_of_pred p in
  let r' = U.true_ () in
  Format.fprintf fmt "%a.@." (print_rule Pp.print_term) (l', r')

(** [mk_var_cstre env f l r] add the constraint [l =?= r]. Call f on l and r such that
    l >= r. *)
let mk_var_cstr f l r =
  let get_number s =
    int_of_string (String.sub s 1 (String.length s - 1))
  in
  let nl = get_number (Basic.string_of_ident @@ Basic.id l) in
  let nr = get_number (Basic.string_of_ident @@ Basic.id r) in
  if nr < nl then (
    f l r; (l,r))
  else (
    f r l; (r,l))

let deps = ref []

let mk_cstr env f cstr =
  let fmt = F.fmt_of_file env.file in
  match cstr with
  | U.Pred p ->
    Format.fprintf fmt "%a@." print_predicate p;
    true
  | U.EqVar(l,r) ->
    let (l,r) = mk_var_cstr f l r in
    add_rule env.meta.sg l r;
    if not (List.mem (Basic.md r) !deps) then deps := (Basic.md r)::!deps;
    Format.fprintf fmt "%a@." print_eq_var (l,r);
    true

let get_deps () = !deps

let flush () = deps := []
